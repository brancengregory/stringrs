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
        speedup = if (!is.na(baseline)) baseline / median_ms else NA_real_,
        workload = sprintf("%s×%s", 
                          format(sc_n_strings, scientific = FALSE, big.mark = ","),
                          format(sc_n_patterns, scientific = FALSE, big.mark = ","))
      )
  })
  
  if (is.null(data) || nrow(data) == 0) {
    warning("No successful benchmark results to plot")
    return(NULL)
  }
  
  # Create plot
  p <- ggplot(data, aes(x = workload, y = speedup, fill = engine)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red", alpha = 0.7) +
    scale_y_log10(labels = comma_format()) +
    labs(
      title = "Speedup vs R base (grepl)",
      subtitle = "Higher is better - red line shows baseline",
      x = "Workload (strings × patterns)",
      y = "Speedup factor (log scale)",
      fill = "Engine"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      legend.position = "bottom",
      legend.text = element_text(size = 8)
    )
  
  # Save
  output_file <- file.path(output_dir, "01_speedup_comparison.png")
  ggsave(output_file, p, width = 14, height = 8, dpi = 150)
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
        total_ops = sc_n_strings * sc_n_patterns
      )
  })
  
  if (is.null(data) || nrow(data) == 0) {
    warning("No successful benchmark results to plot")
    return(NULL)
  }
  
  # Create plot
  p <- ggplot(data, aes(x = n_strings, y = throughput, color = engine, group = engine)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    facet_wrap(~n_patterns, scales = "free_y", labeller = label_both) +
    scale_x_log10(labels = comma_format()) +
    scale_y_log10(labels = function(x) {
      ifelse(x < 1000, sprintf("%.0f", x),
             ifelse(x < 1e6, sprintf("%.1fK", x / 1000),
                    sprintf("%.1fM", x / 1e6)))
    }) +
    labs(
      title = "Throughput by Data Size",
      subtitle = "Strings processed per second",
      x = "Number of strings (log scale)",
      y = "Throughput (strings/sec, log scale)",
      color = "Engine"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      strip.background = element_rect(fill = "lightgray"),
      strip.text = element_text(face = "bold")
    )
  
  # Save
  output_file <- file.path(output_dir, "02_throughput_comparison.png")
  ggsave(output_file, p, width = 14, height = 10, dpi = 150)
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
    
    metrics %>%
      filter(success) %>%
      mutate(
        scenario = sc_name,
        n_strings = sc_n_strings
      )
  })
  
  if (is.null(data) || nrow(data) == 0) {
    warning("No successful benchmark results to plot")
    return(NULL)
  }
  
  data <- data %>%
    group_by(engine) %>%
    arrange(n_strings) %>%
    mutate(
      time_ratio = median_ms / first(median_ms),
      size_ratio = n_strings / first(n_strings),
      scaling_efficiency = size_ratio / time_ratio
    ) %>%
    ungroup()
  
  # Create plot
  p <- ggplot(data, aes(x = size_ratio, y = time_ratio, color = engine)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.5) +
    labs(
      title = "Scaling Efficiency",
      subtitle = "Dashed line = perfect linear scaling",
      x = "Data size ratio",
      y = "Time ratio"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Save
  output_file <- file.path(output_dir, "03_scaling_efficiency.png")
  ggsave(output_file, p, width = 10, height = 6, dpi = 150)
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
