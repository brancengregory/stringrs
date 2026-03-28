#!/usr/bin/env Rscript
# Metrics calculation and reporting utilities
#
# This file provides functions for calculating and formatting benchmark metrics.

#' Extract metrics from benchmark results
extract_metrics <- function(bench_results) {
  map_dfr(bench_results, function(result) {
    if (!result$success) {
      return(tibble(
        engine = result$engine,
        success = FALSE,
        median_ms = NA_real_,
        min_ms = NA_real_,
        max_ms = NA_real_,
        mem_mb = NA_real_,
        throughput = NA_real_
      ))
    }
    
    tibble(
      engine = result$engine,
      success = TRUE,
      median_ms = result$median_time_ms,
      min_ms = result$min_time_ms,
      max_ms = result$max_time_ms,
      mem_mb = result$mem_alloc_mb,
      throughput = result$throughput
    )
  })
}

#' Calculate speedup relative to baseline
calculate_speedup <- function(metrics, baseline_engine = "grepl") {
  baseline <- metrics %>%
    filter(engine == baseline_engine, success) %>%
    pull(median_ms) %>%
    first()
  
  if (is.na(baseline)) {
    warning("Baseline engine not found or failed")
    return(metrics %>% mutate(speedup = NA_real_))
  }
  
  metrics %>%
    mutate(speedup = baseline / median_ms)
}

#' Calculate efficiency metrics
calculate_efficiency <- function(metrics, scenario) {
  metrics %>%
    mutate(
      # Work per unit time (strings * patterns / second)
      work_rate = (scenario$n_strings * scenario$n_patterns) / (median_ms / 1000),
      
      # Memory per string (bytes)
      mem_per_string = (mem_mb * 1024^2) / scenario$n_strings,
      
      # Time per match operation (microseconds)
      time_per_op = (median_ms * 1000) / (scenario$n_strings * scenario$n_patterns)
    )
}

#' Format time duration nicely
format_duration <- function(seconds) {
  if (seconds < 1) {
    sprintf("%.0f ms", seconds * 1000)
  } else if (seconds < 60) {
    sprintf("%.1f s", seconds)
  } else if (seconds < 3600) {
    sprintf("%.1f min", seconds / 60)
  } else {
    sprintf("%.1f hr", seconds / 3600)
  }
}

#' Format throughput nicely
format_throughput <- function(strings_per_sec) {
  if (strings_per_sec < 1000) {
    sprintf("%.0f str/s", strings_per_sec)
  } else if (strings_per_sec < 1e6) {
    sprintf("%.1fK str/s", strings_per_sec / 1000)
  } else {
    sprintf("%.1fM str/s", strings_per_sec / 1e6)
  }
}

#' Format memory nicely
format_memory <- function(mb) {
  if (mb < 1) {
    sprintf("%.1f KB", mb * 1024)
  } else if (mb < 1024) {
    sprintf("%.1f MB", mb)
  } else {
    sprintf("%.1f GB", mb / 1024)
  }
}

#' Calculate confidence interval for timing
calculate_confidence_interval <- function(times, confidence = 0.95) {
  n <- length(times)
  if (n < 2) return(list(lower = NA, upper = NA))
  
  mean_time <- mean(times)
  sd_time <- sd(times)
  se <- sd_time / sqrt(n)
  
  # t-distribution critical value
  alpha <- 1 - confidence
  t_crit <- qt(1 - alpha / 2, df = n - 1)
  
  margin <- t_crit * se
  
  list(
    lower = mean_time - margin,
    upper = mean_time + margin,
    margin = margin
  )
}

#' Detect outliers in benchmark results (using IQR method)
detect_outliers <- function(times, threshold = 1.5) {
  q1 <- quantile(times, 0.25, na.rm = TRUE)
  q3 <- quantile(times, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  
  lower_bound <- q1 - threshold * iqr
  upper_bound <- q3 + threshold * iqr
  
  list(
    outliers = times < lower_bound | times > upper_bound,
    lower_bound = lower_bound,
    upper_bound = upper_bound,
    n_outliers = sum(times < lower_bound | times > upper_bound, na.rm = TRUE)
  )
}

#' Calculate scaling efficiency
#' 
#' How well does performance scale with data size?
#' Perfect scaling = 1.0 (double data = double time)
calculate_scaling_efficiency <- function(results_df) {
  results_df %>%
    group_by(engine) %>%
    arrange(n_strings) %>%
    mutate(
      time_ratio = median_ms / first(median_ms),
      size_ratio = n_strings / first(n_strings),
      scaling_efficiency = size_ratio / time_ratio  # >1 is superlinear, 1 is perfect
    ) %>%
    ungroup()
}

#' Generate ranking table
#'
#' Rank engines by performance across all scenarios
generate_rankings <- function(all_results) {
  # Extract all metrics
  all_metrics <- map_dfr(all_results, function(result) {
    scenario <- result$scenario
    metrics <- extract_metrics(unlist(result$results, recursive = FALSE))
    
    metrics %>%
      mutate(
        scenario = scenario$name,
        n_strings = scenario$n_strings,
        n_patterns = scenario$n_patterns
      )
  })
  
  # Calculate average speedup
  rankings <- all_metrics %>%
    group_by(scenario) %>%
    mutate(speedup = median_ms[engine == "grepl"] / median_ms) %>%
    ungroup() %>%
    group_by(engine) %>%
    summarise(
      avg_speedup = mean(speedup, na.rm = TRUE),
      min_speedup = min(speedup, na.rm = TRUE),
      max_speedup = max(speedup, na.rm = TRUE),
      n_scenarios = sum(!is.na(speedup)),
      wins = sum(speedup == max(speedup, na.rm = TRUE), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(avg_speedup))
  
  rankings
}

#' Print formatted benchmark summary
print_benchmark_summary <- function(scenario, metrics) {
  cat(sprintf("\n  %s\n", strrep("-", 50)))
  cat(sprintf("  Scenario: %s\n", scenario$name))
  cat(sprintf("  Data: %s strings × %s patterns\n",
              format(scenario$n_strings, big.mark = ","),
              format(scenario$n_patterns, big.mark = ",")))
  cat(sprintf("  %s\n", strrep("-", 50)))
  
  # Sort by median time
  metrics_sorted <- metrics %>%
    filter(success) %>%
    arrange(median_ms)
  
  # Print table
  cat(sprintf("  %-30s %12s %15s %12s\n", 
              "Engine", "Time (ms)", "Throughput", "Speedup"))
  cat(sprintf("  %s\n", strrep("-", 73)))
  
  baseline_time <- metrics_sorted$median_ms[metrics_sorted$engine == "grepl"]
  
  for (i in seq_len(nrow(metrics_sorted))) {
    row <- metrics_sorted[i, ]
    speedup <- if (!is.na(baseline_time) && row$engine != "grepl") {
      sprintf("%.1fx", baseline_time / row$median_ms)
    } else {
      "baseline"
    }
    
    cat(sprintf("  %-30s %12.2f %15s %12s\n",
                row$engine,
                row$median_ms,
                format_throughput(row$throughput),
                speedup))
  }
  
  cat(sprintf("  %s\n", strrep("-", 73)))
}
