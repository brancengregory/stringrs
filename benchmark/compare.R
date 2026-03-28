#!/usr/bin/env Rscript
# Baseline comparison utilities
#
# This file provides functions for comparing current benchmark results
# against historical baselines to detect performance regressions.

suppressPackageStartupMessages({
  library(dplyr)
  library(jsonlite)
})

#' Find the most recent baseline
find_baseline <- function() {
  # Look for previous result directories
  results_dirs <- list.dirs("benchmark/results", recursive = FALSE)
  
  if (length(results_dirs) == 0) {
    return(NULL)
  }
  
  # Sort by date (directory names are timestamps)
  sorted_dirs <- sort(results_dirs, decreasing = TRUE)
  
  # Return the most recent (excluding current if exists)
  sorted_dirs[1]
}

#' Load baseline results
load_baseline <- function(baseline_dir = NULL) {
  if (is.null(baseline_dir)) {
    baseline_dir <- find_baseline()
  }
  
  if (is.null(baseline_dir)) {
    warning("No baseline found")
    return(NULL)
  }
  
  results_file <- file.path(baseline_dir, "benchmark_results.rds")
  
  if (!file.exists(results_file)) {
    warning("No results file found in baseline directory")
    return(NULL)
  }
  
  readRDS(results_file)
}

#' Compare current results to baseline
compare_to_baseline <- function(current_results, output_dir, 
                               baseline_results = NULL,
                               regression_threshold = 1.10) {  # 10% slower = regression
  
  if (is.null(baseline_results)) {
    baseline_results <- load_baseline()
  }
  
  if (is.null(baseline_results)) {
    message("No baseline available for comparison")
    return(NULL)
  }
  
  # Extract metrics from both
  current_metrics <- extract_comparison_metrics(current_results)
  baseline_metrics <- extract_comparison_metrics(baseline_results)
  
  # Join and compare
  comparison <- current_metrics %>%
    inner_join(baseline_metrics, by = c("scenario", "engine"), suffix = c("_current", "_baseline")) %>%
    mutate(
      time_ratio = median_ms_current / median_ms_baseline,
      is_regression = time_ratio > regression_threshold,
      is_improvement = time_ratio < (1 / regression_threshold),
      change_pct = (time_ratio - 1) * 100
    )
  
  # Summarize
  summary <- list(
    n_comparisons = nrow(comparison),
    n_regressions = sum(comparison$is_regression, na.rm = TRUE),
    n_improvements = sum(comparison$is_improvement, na.rm = TRUE),
    regressions = comparison %>% filter(is_regression) %>% arrange(desc(time_ratio)),
    improvements = comparison %>% filter(is_improvement) %>% arrange(time_ratio),
    unchanged = comparison %>% filter(!is_regression, !is_improvement)
  )
  
  # Save comparison
  comparison_file <- file.path(output_dir, "baseline_comparison.json")
  write_json(comparison, comparison_file, pretty = TRUE)
  cat(sprintf("Comparison saved to: %s\n", comparison_file))
  
  # Generate report
  report_file <- file.path(output_dir, "comparison_report.md")
  generate_comparison_report(summary, report_file)
  cat(sprintf("Comparison report saved to: %s\n", report_file))
  
  summary
}

#' Extract metrics for comparison
extract_comparison_metrics <- function(results) {
  map_dfr(results, function(result) {
    scenario <- result$scenario
    metrics <- extract_metrics(unlist(result$results, recursive = FALSE))
    
    metrics %>%
      filter(success) %>%
      mutate(scenario = scenario$name) %>%
      select(scenario, engine, median_ms)
  })
}

#' Generate comparison report
#' 
#' @param summary Comparison summary object
#' @param output_file Output file path
#' @return NULL
#' @keywords internal
generate_comparison_report <- function(summary, output_file) {
  sink(output_file)
  
  cat("# Performance Comparison Report\n\n")
  cat(sprintf("Generated: %s\n\n", format(Sys.time())))
  
  cat("## Summary\n\n")
  cat(sprintf("- Total comparisons: %d\n", summary$n_comparisons))
  cat(sprintf("- Regressions: %d (%.1f%%)\n", 
              summary$n_regressions, 
              100 * summary$n_regressions / summary$n_comparisons))
  cat(sprintf("- Improvements: %d (%.1f%%)\n",
              summary$n_improvements,
              100 * summary$n_improvements / summary$n_comparisons))
  cat(sprintf("- Unchanged: %d (%.1f%%)\n\n",
              nrow(summary$unchanged),
              100 * nrow(summary$unchanged) / summary$n_comparisons))
  
  if (nrow(summary$regressions) > 0) {
    cat("## Regressions\n\n")
    cat("| Scenario | Engine | Baseline (ms) | Current (ms) | Change |\n")
    cat("|----------|--------|---------------|--------------|--------|\n")
    
    summary$regressions %>%
      head(10) %>%
      pwalk(function(scenario, engine, median_ms_baseline, median_ms_current, change_pct, ...) {
        cat(sprintf("| %s | %s | %.2f | %.2f | +%.1f%% |\n",
                    scenario, engine, median_ms_baseline, median_ms_current, change_pct))
      })
    
    cat("\n")
  }
  
  if (nrow(summary$improvements) > 0) {
    cat("## Improvements\n\n")
    cat("| Scenario | Engine | Baseline (ms) | Current (ms) | Change |\n")
    cat("|----------|--------|---------------|--------------|--------|\n")
    
    summary$improvements %>%
      head(10) %>%
      pwalk(function(scenario, engine, median_ms_baseline, median_ms_current, change_pct, ...) {
        cat(sprintf("| %s | %s | %.2f | %.2f | %.1f%% |\n",
                    scenario, engine, median_ms_baseline, median_ms_current, change_pct))
      })
    
    cat("\n")
  }
  
  cat("## Recommendations\n\n")
  
  if (nrow(summary$regressions) > 0) {
    worst <- summary$regressions %>%
      arrange(desc(time_ratio)) %>%
      slice(1)
    
    cat(sprintf("1. **Investigate**: %s in scenario '%s' (%.1fx slower)\n",
                worst$engine, worst$scenario, worst$time_ratio))
  }
  
  if (nrow(summary$improvements) > 0) {
    best <- summary$improvements %>%
      arrange(time_ratio) %>%
      slice(1)
    
    cat(sprintf("2. **Celebrate**: %s improved by %.1f%% in scenario '%s'\n",
                best$engine, abs(best$change_pct), best$scenario))
  }
  
  sink()
}

#' Set current results as new baseline
set_baseline <- function(results_dir = NULL) {
  if (is.null(results_dir)) {
    # Use most recent
    results_dirs <- list.dirs("benchmark/results", recursive = FALSE)
    if (length(results_dirs) == 0) {
      stop("No results directory found")
    }
    results_dir <- sort(results_dirs, decreasing = TRUE)[1]
  }
  
  # Create baseline marker
  baseline_marker <- file.path(results_dir, "BASELINE")
  file.create(baseline_marker)
  
  message(sprintf("Set %s as baseline", results_dir))
  invisible(results_dir)
}
