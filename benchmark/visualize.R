#!/usr/bin/env Rscript
# Visualization utilities for benchmark results
#
# This file provides functions for generating comparison plots and charts.

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(scales)
})

#' Plot speedup comparison across all scenarios
plot_speedup_comparison <- function(all_results, output_dir) {
  # Extract metrics
  data <- map_dfr(all_results, function(result) {
    # Handle case where scenario might be NULL or malformed
    if (is.null(result$scenario) || !is.list(result$scenario)) {
      return(NULL)
    }
    
    sc <- result$scenario
    all_metrics <- unlist(result$results, recursive = FALSE)
    
    if (length(all_metrics) == 0) {
      return(NULL)
    }
    
    metrics <- extract_metrics(all_metrics)
    
    if (nrow(metrics) == 0 || !any(metrics$success)) {
      return(NULL)
    }
    
    # Calculate speedup vs grepl
    baseline <- metrics %>%
      filter(engine == "grepl", success) %>%
      pull(median_ms) %>%
      first()
    
    # Store scenario values in locals to avoid dplyr name collision
    sc_name <- sc$name
    sc_n_strings <- sc$n_strings
    sc_n_patterns <- sc$n_patterns
    
    metrics %>%
      filter(success) %>%
      mutate(
        scenario = sc_name,
        n_strings = sc_n_strings,
        n_patterns = sc_n_patterns,
        workload_size = sc_n_strings * sc_n_patterns,
        speedup = if (!is.na(baseline)) baseline / median_ms else NA_real_,
        workload = sprintf("%s×%s", 
                          format(sc_n_strings, scientific = FALSE, big.mark = ","),
                          format(sc_n_patterns, scientific = FALSE, big.mark = ",")),
        # Ensure engine names are clean for display
        engine_display = factor(engine, 
                                levels = c("grepl", "stringr", "stringi", 
                                          "mirai_string", "mirai_pattern",
                                          "furrr_string", "furrr_pattern",
                                          "stringrs_auto"),
                                labels = c("R grepl", "R stringr", "R stringi",
                                          "mirai (string)", "mirai (pattern)",
                                          "furrr (string)", "furrr (pattern)",
                                          "stringrs"))
      )
  })
  
  if (is.null(data) || nrow(data) == 0) {
    warning("No successful benchmark results to plot")
    return(NULL)
  }
  
  # Order scenarios by workload size for logical display
  scenario_order <- data %>%
    distinct(scenario, workload_size) %>%
    arrange(workload_size) %>%
    pull(scenario)
  
  data$scenario <- factor(data$scenario, levels = scenario_order)
  
  # Create plot with facets by pattern count
  p <- ggplot(data, aes(x = scenario, y = speedup, fill = engine_display)) +
    geom_bar(stat = "identity", position = "dodge", color = "black", size = 0.2) +
    geom_errorbar(aes(ymin = min_ms / median_ms * speedup, 
                      ymax = max_ms / median_ms * speedup),
                  position = position_dodge(width = 0.9),
                  width = 0.25, alpha = 0.5) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red", alpha = 0.7, size = 1) +
    facet_wrap(~sprintf("%d patterns", n_patterns), scales = "free_x") +
    scale_y_log10(labels = comma_format()) +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title = "Speedup vs R base (grepl)",
      subtitle = "Higher is better - red line shows baseline | Error bars show min/max across iterations",
      x = "Scenario",
      y = "Speedup factor (log scale)",
      fill = "Engine"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      legend.text = element_text(size = 9),
      strip.background = element_rect(fill = "lightgray", color = "black"),
      strip.text = element_text(face = "bold", size = 10),
      panel.spacing = unit(1, "lines")
    )
  
  # Save
  output_file <- file.path(output_dir, "01_speedup_comparison.png")
  ggsave(output_file, p, width = 16, height = 10, dpi = 150)
  cat(sprintf("  Saved: %s\n", output_file))
  
  invisible(p)
}

#' Plot throughput comparison
plot_throughput_comparison <- function(all_results, output_dir) {
  # Extract throughput data
  data <- map_dfr(all_results, function(result) {
    if (is.null(result$scenario) || !is.list(result$scenario)) {
      return(NULL)
    }
    
    sc <- result$scenario
    all_metrics <- unlist(result$results, recursive = FALSE)
    
    if (length(all_metrics) == 0) {
      return(NULL)
    }
    
    metrics <- extract_metrics(all_metrics)
    
    if (nrow(metrics) == 0 || !any(metrics$success)) {
      return(NULL)
    }
    
    sc_name <- sc$name
    sc_n_strings <- sc$n_strings
    sc_n_patterns <- sc$n_patterns
    
    metrics %>%
      filter(success) %>%
      mutate(
        scenario = sc_name,
        n_strings = sc_n_strings,
        n_patterns = sc_n_patterns,
        total_ops = sc_n_strings * sc_n_patterns,
        # Clean engine names for display
        engine_display = factor(engine, 
                                levels = c("grepl", "stringr", "stringi", 
                                          "mirai_string", "mirai_pattern",
                                          "furrr_string", "furrr_pattern",
                                          "stringrs_auto"),
                                labels = c("R grepl", "R stringr", "R stringi",
                                          "mirai (string)", "mirai (pattern)",
                                          "furrr (string)", "furrr (pattern)",
                                          "stringrs"))
      )
  })
  
  if (is.null(data) || nrow(data) == 0) {
    warning("No successful benchmark results to plot")
    return(NULL)
  }
  
  # Ensure proper grouping by sorting by n_strings within each facet
  data <- data %>%
    arrange(n_patterns, engine_display, n_strings)
  
  # Create plot with explicit grouping to ensure lines connect properly
  p <- ggplot(data, aes(x = n_strings, y = throughput, color = engine_display, group = engine_display)) +
    geom_line(linewidth = 1.2, alpha = 0.8) +
    geom_point(size = 3, alpha = 0.9) +
    facet_wrap(~sprintf("%d patterns", n_patterns), scales = "free_y") +
    scale_x_log10(labels = comma_format()) +
    scale_y_log10(labels = function(x) {
      ifelse(x < 1000, sprintf("%.0f", x),
             ifelse(x < 1e6, sprintf("%.1fK", x / 1000),
                    sprintf("%.1fM", x / 1e6)))
    }) +
    scale_color_brewer(palette = "Set2") +
    labs(
      title = "Throughput by Data Size",
      subtitle = "Strings processed per second - Points connected by engine within each pattern count",
      x = "Number of strings (log scale)",
      y = "Throughput (strings/sec, log scale)",
      color = "Engine"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      legend.text = element_text(size = 9),
      strip.background = element_rect(fill = "lightgray", color = "black"),
      strip.text = element_text(face = "bold", size = 10),
      panel.spacing = unit(1.5, "lines")
    )
  
  # Save
  output_file <- file.path(output_dir, "02_throughput_comparison.png")
  ggsave(output_file, p, width = 16, height = 12, dpi = 150)
  cat(sprintf("  Saved: %s\n", output_file))
  
  invisible(p)
}

#' Plot scaling analysis
plot_scaling_analysis <- function(all_results, output_dir) {
  # Calculate scaling efficiency
  data <- map_dfr(all_results, function(result) {
    if (is.null(result$scenario) || !is.list(result$scenario)) {
      return(NULL)
    }
    
    sc <- result$scenario
    all_metrics <- unlist(result$results, recursive = FALSE)
    
    if (length(all_metrics) == 0) {
      return(NULL)
    }
    
    metrics <- extract_metrics(all_metrics)
    
    if (nrow(metrics) == 0 || !any(metrics$success)) {
      return(NULL)
    }
    
    sc_name <- sc$name
    sc_n_strings <- sc$n_strings
    sc_n_patterns <- sc$n_patterns
    
    metrics %>%
      filter(success) %>%
      mutate(
        scenario = sc_name,
        n_strings = sc_n_strings,
        n_patterns = sc_n_patterns,
        # Clean engine names for display
        engine_display = factor(engine, 
                                levels = c("grepl", "stringr", "stringi", 
                                          "mirai_string", "mirai_pattern",
                                          "furrr_string", "furrr_pattern",
                                          "stringrs_auto"),
                                labels = c("R grepl", "R stringr", "R stringi",
                                          "mirai (string)", "mirai (pattern)",
                                          "furrr (string)", "furrr (pattern)",
                                          "stringrs"))
      )
  })
  
  if (is.null(data) || nrow(data) == 0) {
    warning("No successful benchmark results to plot")
    return(NULL)
  }
  
  # Sort by n_strings within each pattern/engine group for proper line ordering
  data <- data %>%
    arrange(n_patterns, engine_display, n_strings) %>%
    group_by(engine_display, n_patterns) %>%
    mutate(
      time_ratio = median_ms / first(median_ms),
      size_ratio = n_strings / first(n_strings),
      scaling_efficiency = size_ratio / time_ratio
    ) %>%
    ungroup()
  
  # Create plot with facets by pattern count for clarity
  p <- ggplot(data, aes(x = size_ratio, y = time_ratio, color = engine_display, group = engine_display)) +
    geom_line(linewidth = 1.2, alpha = 0.8) +
    geom_point(size = 3, alpha = 0.9) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", alpha = 0.7, size = 1) +
    geom_abline(intercept = 0, slope = 0.5, linetype = "dotted", color = "gray50", alpha = 0.5) +
    geom_abline(intercept = 0, slope = 2, linetype = "dotted", color = "gray50", alpha = 0.5) +
    facet_wrap(~sprintf("%d patterns", n_patterns), scales = "free") +
    scale_color_brewer(palette = "Set2") +
    labs(
      title = "Scaling Efficiency",
      subtitle = "Red line = perfect linear scaling | Dotted lines = 0.5x and 2x scaling reference",
      x = "Data size ratio (relative to smallest in group)",
      y = "Time ratio (relative to fastest in group)",
      color = "Engine"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      legend.text = element_text(size = 9),
      strip.background = element_rect(fill = "lightgray", color = "black"),
      strip.text = element_text(face = "bold", size = 10),
      panel.spacing = unit(1.5, "lines")
    )
  
  # Save
  output_file <- file.path(output_dir, "03_scaling_efficiency.png")
  ggsave(output_file, p, width = 16, height = 10, dpi = 150)
  cat(sprintf("  Saved: %s\n", output_file))
  
  invisible(p)
}

#' Plot memory usage comparison
plot_memory_comparison <- function(all_results, output_dir) {
  # Extract memory data
  data <- map_dfr(all_results, function(result) {
    if (is.null(result$scenario) || !is.list(result$scenario)) {
      return(NULL)
    }
    
    sc <- result$scenario
    all_metrics <- unlist(result$results, recursive = FALSE)
    
    if (length(all_metrics) == 0) {
      return(NULL)
    }
    
    metrics <- extract_metrics(all_metrics)
    
    if (nrow(metrics) == 0 || !any(metrics$success)) {
      return(NULL)
    }
    
    sc_name <- sc$name
    sc_n_strings <- sc$n_strings
    
    metrics %>%
      filter(success, !is.na(mem_mb)) %>%
      mutate(
        scenario = sc_name,
        n_strings = sc_n_strings,
        mem_per_string_kb = (mem_mb * 1024) / sc_n_strings
      )
  })
  
  if (is.null(data) || nrow(data) == 0) {
    warning("No successful benchmark results to plot")
    return(NULL)
  }
  
  # Create plot
  p <- ggplot(data, aes(x = engine, y = mem_per_string_kb, fill = engine)) +
    geom_bar(stat = "identity") +
    facet_wrap(~n_strings, scales = "free_y", labeller = label_both) +
    labs(
      title = "Memory Usage per String",
      subtitle = "Lower is better",
      x = "Engine",
      y = "Memory per string (KB)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none",
      strip.background = element_rect(fill = "lightgray")
    )
  
  # Save
  output_file <- file.path(output_dir, "04_memory_usage.png")
  ggsave(output_file, p, width = 12, height = 8, dpi = 150)
  cat(sprintf("  Saved: %s\n", output_file))
  
  invisible(p)
}

#' Create heatmap of engine performance across scenarios
plot_performance_heatmap <- function(all_results, output_dir) {
  # Prepare data
  data <- map_dfr(all_results, function(result) {
    if (is.null(result$scenario) || !is.list(result$scenario)) {
      return(NULL)
    }
    
    sc <- result$scenario
    all_metrics <- unlist(result$results, recursive = FALSE)
    
    if (length(all_metrics) == 0) {
      return(NULL)
    }
    
    metrics <- extract_metrics(all_metrics)
    
    if (nrow(metrics) == 0 || !any(metrics$success)) {
      return(NULL)
    }
    
    baseline <- metrics %>%
      filter(engine == "grepl", success) %>%
      pull(median_ms) %>%
      first()
    
    sc_name <- sc$name
    
    metrics %>%
      filter(success) %>%
      mutate(
        scenario = sc_name,
        speedup = if (!is.na(baseline)) baseline / median_ms else NA_real_
      ) %>%
      select(scenario, engine, speedup)
  })
  
  if (is.null(data) || nrow(data) == 0) {
    warning("No successful benchmark results to plot")
    return(NULL)
  }
  
  data <- data %>%
    pivot_wider(names_from = engine, values_from = speedup)
  
  # Convert to long format for heatmap
  data_long <- data %>%
    pivot_longer(-scenario, names_to = "engine", values_to = "speedup")
  
  # Create heatmap
  p <- ggplot(data_long, aes(x = engine, y = scenario, fill = speedup)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.1f", speedup)), size = 3) +
    scale_fill_gradient2(
      low = "red",
      mid = "yellow",
      high = "green",
      midpoint = 1,
      na.value = "gray50"
    ) +
    labs(
      title = "Performance Heatmap",
      subtitle = "Speedup vs R base (grepl) - green is faster",
      x = "Engine",
      y = "Scenario",
      fill = "Speedup"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_text(size = 8)
    )
  
  # Save
  output_file <- file.path(output_dir, "05_performance_heatmap.png")
  ggsave(output_file, p, width = 16, height = 12, dpi = 150)
  cat(sprintf("  Saved: %s\n", output_file))
  
  invisible(p)
}
