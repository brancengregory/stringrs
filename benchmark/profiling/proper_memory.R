#!/usr/bin/env Rscript
# Proper Memory Profiling for stringrs
# Uses profmem for accurate allocation tracking
#
# Usage:
#   Rscript benchmark/profiling/proper_memory.R <scenario>

suppressPackageStartupMessages({
  library(profmem)
  library(dplyr)
  library(purrr)
  library(tibble)
})

source("benchmark/scenarios.R")
devtools::load_all()

#' Run memory profile with profmem
profile_memory_proper <- function(scenario_name) {
  # Find scenario
  all_scenarios <- c(SCENARIOS_QUICK, SCENARIOS_FULL)
  scenario_idx <- which(map_chr(all_scenarios, "name") == scenario_name)
  
  if (length(scenario_idx) == 0) {
    stop("Unknown scenario: ", scenario_name)
  }
  
  scenario <- all_scenarios[[scenario_idx]]
  
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  cat(sprintf("  Proper Memory Profile: %s\n", scenario_name))
  cat("═══════════════════════════════════════════════════════════════\n")
  cat(sprintf("  Strings: %s\n", format(scenario$n_strings, big.mark = ",")))
  cat(sprintf("  Patterns: %s\n", format(scenario$n_patterns, big.mark = ",")))
  cat(sprintf("  String length: %d\n", scenario$string_length))
  cat("\n")
  
  # Generate test data
  test_data <- generate_test_data(
    scenario$n_strings,
    scenario$n_patterns,
    scenario$string_length
  )
  
  # Calculate input size
  input_size_mb <- sum(nchar(test_data$strings) + nchar(test_data$patterns)) * 2 / 1024 / 1024
  cat(sprintf("Input data size: %.2f MB\n\n", input_size_mb))
  
  results <- list()
  
  # Test 1: grepl
  cat("Profiling: grepl...\n")
  if (length(test_data$patterns) == 1) {
    p <- profmem({
      res <- grepl(test_data$patterns[1], test_data$strings)
    })
  } else {
    p <- profmem({
      res <- lapply(test_data$patterns, function(pat) grepl(pat, test_data$strings))
      res <- do.call(cbind, res)
    })
  }
  results$grepl <- analyze_profmem(p, "grepl")
  
  # Test 2: stringr
  cat("Profiling: stringr...\n")
  if (length(test_data$patterns) == 1) {
    p <- profmem({
      res <- stringr::str_detect(test_data$strings, test_data$patterns[1])
    })
  } else {
    p <- profmem({
      res <- lapply(test_data$patterns, function(pat) {
        stringr::str_detect(test_data$strings, pat)
      })
      res <- do.call(cbind, res)
    })
  }
  results$stringr <- analyze_profmem(p, "stringr")
  
  # Test 3: stringrs
  cat("Profiling: stringrs (auto)...\n")
  p <- profmem({
    res <- string_detect(test_data$strings, test_data$patterns)
  })
  results$stringrs <- analyze_profmem(p, "stringrs_auto")
  
  # Test 4: stringrs sequential (if applicable)
  if (length(test_data$strings) > 100 || length(test_data$patterns) > 1) {
    cat("Profiling: stringrs (sequential)...\n")
    p <- profmem({
      res <- string_detect(test_data$strings, test_data$patterns,
                          engine = "regex", parallel = "sequential")
    })
    results$stringrs_seq <- analyze_profmem(p, "stringrs_seq")
  }
  
  # Print comparison
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("                    MEMORY ALLOCATION SUMMARY\n")
  cat("═══════════════════════════════════════════════════════════════\n\n")
  
  comparison <- map_dfr(results, function(r) {
    tibble(
      Engine = r$engine,
      `Total Allocations` = r$total_allocations,
      `Total Bytes` = format_bytes(r$total_bytes),
      `Peak Bytes` = format_bytes(r$peak_bytes),
      `Allocations/sec` = round(r$total_allocations / r$time_sec, 0),
      `Time (ms)` = round(r$time_sec * 1000, 2)
    )
  })
  
  print(comparison, n = Inf)
  
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("Analysis:\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  
  # Find best/worst
  best_alloc <- results[[which.min(map_dbl(results, function(r) r$total_allocations))]]
  worst_alloc <- results[[which.max(map_dbl(results, function(r) r$total_allocations))]]
  
  cat(sprintf("✓ Fewest allocations: %s (%s allocations)\n",
              best_alloc$engine, format(best_alloc$total_allocations, big.mark = ",")))
  cat(sprintf("✗ Most allocations: %s (%s allocations)\n",
              worst_alloc$engine, format(worst_alloc$total_allocations, big.mark = ",")))
  cat(sprintf("  Allocation ratio: %.1fx more than best\n",
              worst_alloc$total_allocations / best_alloc$total_allocations))
  
  # Memory efficiency
  cat("\nMemory Efficiency (bytes per string):\n")
  for (name in names(results)) {
    r <- results[[name]]
    bytes_per_string <- r$total_bytes / scenario$n_strings
    cat(sprintf("  %-15s: %s bytes/string\n", r$engine, format(round(bytes_per_string, 0), big.mark = ",")))
  }
  
  cat("\n═══════════════════════════════════════════════════════════════\n")
  
  invisible(results)
}

#' Analyze profmem output
analyze_profmem <- function(p, engine_name) {
  # Filter to actual allocations (exclude gc events)
  allocs <- p %>% filter(!is.na(bytes))
  
  # Calculate metrics
  total_allocations <- nrow(allocs)
  total_bytes <- sum(allocs$bytes, na.rm = TRUE)
  peak_bytes <- if (total_allocations > 0) max(allocs$bytes, na.rm = TRUE) else 0
  
  # Get timing
  time_sec <- as.numeric(p$time[[length(p$time)]]) - as.numeric(p$time[[1]])
  if (is.na(time_sec) || time_sec == 0) time_sec <- 0.001  # Minimum 1ms
  
  # Print summary
  cat(sprintf("  Total allocations: %s\n", format(total_allocations, big.mark = ",")))
  cat(sprintf("  Total bytes: %s\n", format_bytes(total_bytes)))
  cat(sprintf("  Peak allocation: %s\n", format_bytes(peak_bytes)))
  cat(sprintf("  Time: %.2f ms\n", time_sec * 1000))
  cat("\n")
  
  list(
    engine = engine_name,
    total_allocations = total_allocations,
    total_bytes = total_bytes,
    peak_bytes = peak_bytes,
    time_sec = time_sec,
    allocations = allocs
  )
}

#' Format bytes nicely
format_bytes <- function(bytes) {
  if (bytes < 1024) {
    sprintf("%d B", round(bytes))
  } else if (bytes < 1024^2) {
    sprintf("%.2f KB", bytes / 1024)
  } else if (bytes < 1024^3) {
    sprintf("%.2f MB", bytes / 1024^2)
  } else {
    sprintf("%.2f GB", bytes / 1024^3)
  }
}

#' Main
main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args) < 1) {
    cat("Usage: Rscript benchmark/profiling/proper_memory.R <scenario>\n")
    cat("\nExamples:\n")
    cat("  Rscript benchmark/profiling/proper_memory.R quick_single\n")
    cat("  Rscript benchmark/profiling/proper_memory.R medium_multi\n")
    cat("  Rscript benchmark/profiling/proper_memory.R many_patterns_small\n")
    cat("\nAvailable scenarios:\n")
    walk(SCENARIOS_QUICK, ~cat(sprintf("  - %s\n", .x$name)))
    return(NULL)
  }
  
  scenario_name <- args[1]
  
  # Check if profmem is installed
  if (!requireNamespace("profmem", quietly = TRUE)) {
    cat("Installing profmem package...\n")
    install.packages("profmem", repos = "https://cloud.r-project.org")
  }
  
  profile_memory_proper(scenario_name)
}

if (!interactive()) {
  main()
}
