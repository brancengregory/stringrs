#!/usr/bin/env Rscript
# Unified Benchmark Suite for stringrs
#
# This script provides comprehensive end-to-end benchmarks comparing
# multiple regex matching implementations:
# - R base: grepl, stringr, stringi
# - R parallel: mirai, furrr
# - Rust: stringrs with various configurations
#
# Usage:
#   Rscript benchmark/run.R                    # Full benchmark suite
#   Rscript benchmark/run.R --quick            # Quick test (reduced scenarios)
#   Rscript benchmark/run.R --profile-memory   # Include memory profiling
#   Rscript benchmark/run.R --compare          # Compare to historical baseline

suppressPackageStartupMessages({
  library(bench)
  library(tibble)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(jsonlite)
})

# Source benchmark components
source("benchmark/scenarios.R")
source("benchmark/engines.R")
source("benchmark/metrics.R")
source("benchmark/visualize.R")
source("benchmark/compare.R")

# Load stringrs package
devtools::load_all()

#' Parse command line arguments
parse_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  list(
    quick = "--quick" %in% args,
    profile_memory = "--profile-memory" %in% args,
    compare = "--compare" %in% args,
    workers = as.numeric(get_arg(args, "--workers", 4)),
    iterations = as.numeric(get_arg(args, "--iterations", 3)),
    output_dir = get_arg(args, "--output", file.path("benchmark", "results", format(Sys.time(), "%Y%m%d_%H%M%S")))
  )
}

get_arg <- function(args, flag, default) {
  idx <- which(args == flag)
  if (length(idx) > 0 && idx < length(args)) {
    return(args[idx + 1])
  }
  return(default)
}

#' Main benchmark execution
run_benchmarks <- function(args) {
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("          stringrs Unified Benchmark Suite\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("\n")
  
  # Create output directory
  dir.create(args$output_dir, recursive = TRUE, showWarnings = FALSE)
  cat(sprintf("Results directory: %s\n", args$output_dir))
  cat(sprintf("Workers: %d | Iterations: %d | Mode: %s\n\n", 
              args$workers, args$iterations,
              if (args$quick) "QUICK" else "FULL"))
  
  # Get scenarios
  scenarios <- if (args$quick) SCENARIOS_QUICK else SCENARIOS_FULL
  cat(sprintf("Running %d benchmark scenarios...\n\n", length(scenarios)))
  
  # Run all scenarios
  all_results <- map(seq_along(scenarios), function(i) {
    scenario <- scenarios[[i]]
    cat(sprintf("[%d/%d] %s (%s strings × %s patterns)\n", 
                i, length(scenarios), scenario$name,
                format(scenario$n_strings, big.mark = ","),
                format(scenario$n_patterns, big.mark = ",")))
    
    # Generate test data
    test_data <- generate_test_data(
      n_strings = scenario$n_strings,
      n_patterns = scenario$n_patterns,
      string_length = scenario$string_length
    )
    
    # Run benchmarks for this scenario
    scenario_results <- run_scenario_benchmarks(
      test_data = test_data,
      workers = args$workers,
      iterations = args$iterations,
      profile_memory = args$profile_memory
    )
    
    # Add metadata
    scenario_results$scenario <- scenario
    scenario_results$timestamp <- Sys.time()
    
    cat(sprintf("    Completed in %.2f seconds\n", scenario_results$elapsed))
    
    scenario_results
  })
  
  # Save raw results
  results_file <- file.path(args$output_dir, "benchmark_results.rds")
  saveRDS(all_results, results_file)
  cat(sprintf("\nRaw results saved to: %s\n", results_file))
  
  # Generate summary
  summary <- generate_summary(all_results)
  summary_file <- file.path(args$output_dir, "summary.json")
  write_json(summary, summary_file, pretty = TRUE)
  cat(sprintf("Summary saved to: %s\n", summary_file))
  
  # Generate visualizations
  cat("\nGenerating visualizations...\n")
  plot_speedup_comparison(all_results, args$output_dir)
  plot_throughput_comparison(all_results, args$output_dir)
  plot_scaling_analysis(all_results, args$output_dir)
  
  # Compare to baseline if requested
  if (args$compare) {
    cat("\nComparing to baseline...\n")
    comparison <- compare_to_baseline(all_results, args$output_dir)
    print_comparison_summary(comparison)
  }
  
  # Print final summary
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("                    BENCHMARK COMPLETE\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  cat(sprintf("Results location: %s\n", args$output_dir))
  cat(sprintf("Total scenarios: %d\n", length(scenarios)))
  cat(sprintf("Total time: %.2f minutes\n", sum(map_dbl(all_results, ~.$elapsed)) / 60))
  cat("\n")
  
  # Print speedup summary table
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("              SPEEDUP SUMMARY (vs Idiomatic R)\n")
  cat("═══════════════════════════════════════════════════════════════\n\n")
  
  cat(sprintf("%-25s %10s %10s %10s\n", "Scenario", "vs stringr", "vs stringi", "vs grepl"))
  cat(sprintf("%s\n", strrep("-", 60)))
  
  for (r in all_results) {
    cat(sprintf("%-25s %9.1fx %9.1fx %9.1fx\n",
                substr(r$scenario$name, 1, 25),
                r$speedup_vs_stringr,
                r$speedup_vs_stringi,
                r$speedup_vs_grepl))
  }
  
  cat(sprintf("%s\n", strrep("-", 60)))
  
  # Calculate averages
  avg_stringr <- mean(map_dbl(all_results, ~.$speedup_vs_stringr), na.rm = TRUE)
  avg_stringi <- mean(map_dbl(all_results, ~.$speedup_vs_stringi), na.rm = TRUE)
  avg_grepl <- mean(map_dbl(all_results, ~.$speedup_vs_grepl), na.rm = TRUE)
  
  cat(sprintf("%-25s %9.1fx %9.1fx %9.1fx\n", "AVERAGE", avg_stringr, avg_stringi, avg_grepl))
  cat("\n")
  cat("Note: Higher is better. stringr is the idiomatic R package most users choose.\n")
  cat("═══════════════════════════════════════════════════════════════\n\n")
  
  invisible(all_results)
}

#' Generate summary statistics
generate_summary <- function(results) {
  list(
    timestamp = format(Sys.time()),
    total_scenarios = length(results),
    total_time_sec = sum(map_dbl(results, ~.$elapsed)),
    scenarios = map(results, function(r) {
      list(
        name = r$scenario$name,
        n_strings = r$scenario$n_strings,
        n_patterns = r$scenario$n_patterns,
        best_engine = r$best_engine,
        best_time_ms = r$best_time_ms,
        # Speedup vs different baselines
        speedup_vs_grepl = r$speedup_vs_grepl,
        speedup_vs_stringr = r$speedup_vs_stringr,
        speedup_vs_stringi = r$speedup_vs_stringi,
        # Backwards compatibility
        speedup_vs_base = r$speedup_vs_grepl
      )
    })
  )
}

#' Print comparison summary
print_comparison_summary <- function(comparison) {
  cat("\n=== Performance vs Baseline ===\n")
  cat(sprintf("Regressions: %d (%.1f%%)\n", 
              comparison$n_regressions,
              100 * comparison$n_regressions / comparison$n_comparisons))
  cat(sprintf("Improvements: %d (%.1f%%)\n",
              comparison$n_improvements,
              100 * comparison$n_improvements / comparison$n_comparisons))
  
  if (nrow(comparison$regressions) > 0) {
    cat("\nWorst regressions:\n")
    comparison$regressions %>%
      head(3) %>%
      walk(~cat(sprintf("  - %s: %.1fx slower\n", .x$scenario, .x$ratio)))
  }
}

# Main execution
if (!interactive()) {
  args <- parse_args()
  results <- run_benchmarks(args)
}
