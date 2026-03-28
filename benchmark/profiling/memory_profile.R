#!/usr/bin/env Rscript
# Memory Profiling for stringrs - Using R's built-in memory tracking
#
# This script profiles memory usage using R's gc() and bench memory measurements.
# Note: bench::mark shows NA for memory when GC occurs every iteration.
#
# Usage:
#   Rscript benchmark/profiling/memory_profile.R <scenario> [iterations]
#   Rscript benchmark/profiling/memory_profile.R medium_multi 1

suppressPackageStartupMessages({
  library(dplyr)
  library(bench)
  library(purrr)
  library(tibble)
})

source("benchmark/scenarios.R")
devtools::load_all()

#' Profile memory with multiple methods
profile_memory <- function(scenario_name, iterations = 1) {
  # Find scenario
  all_scenarios <- c(SCENARIOS_QUICK, SCENARIOS_FULL)
  scenario_idx <- which(map_chr(all_scenarios, "name") == scenario_name)
  
  if (length(scenario_idx) == 0) {
    stop("Unknown scenario: ", scenario_name)
  }
  
  scenario <- all_scenarios[[scenario_idx]]
  
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  cat(sprintf("  Memory Profile: %s\n", scenario_name))
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
  
  # Calculate input sizes
  strings_chars <- sum(nchar(test_data$strings))
  patterns_chars <- sum(nchar(test_data$patterns))
  
  cat("Input Data:\n")
  cat(sprintf("  Total characters: %s\n", format(strings_chars + patterns_chars, big.mark = ",")))
  cat(sprintf("  Input size: ~%.2f MB\n", (strings_chars * 2 + patterns_chars * 2) / 1024 / 1024))
  cat("\n")
  
  results <- list()
  
  # Method 1: Track with gc() delta
  cat("Running gc() delta profiling...\n\n")
  
  # Test 1: grepl
  cat("1. grepl...\n")
  gc(reset = TRUE)
  before_gc <- gc()
  
  if (length(test_data$patterns) == 1) {
    res <- grepl(test_data$patterns[1], test_data$strings)
  } else {
    res <- lapply(test_data$patterns, function(p) grepl(p, test_data$strings))
    res <- do.call(cbind, res)
  }
  
  after_gc <- gc()
  
  gc_delta <- after_gc - before_gc
  results$grepl <- list(
    engine = "grepl",
    gc_delta_mb = sum(gc_delta[, 2]),  # MB used
    gc_count = sum(gc_delta[, 1]),
    n_cells = sum(gc_delta[, 2]) * 0.0005  # Approximate cell count
  )
  cat(sprintf("   GC delta: %.2f MB, GC events: %d\n", 
              results$grepl$gc_delta_mb, results$grepl$gc_count))
  
  # Test 2: stringr
  cat("2. stringr...\n")
  gc(reset = TRUE)
  before_gc <- gc()
  
  if (length(test_data$patterns) == 1) {
    res <- stringr::str_detect(test_data$strings, test_data$patterns[1])
  } else {
    res <- lapply(test_data$patterns, function(p) stringr::str_detect(test_data$strings, p))
    res <- do.call(cbind, res)
  }
  
  after_gc <- gc()
  
  gc_delta <- after_gc - before_gc
  results$stringr <- list(
    engine = "stringr",
    gc_delta_mb = sum(gc_delta[, 2]),
    gc_count = sum(gc_delta[, 1]),
    n_cells = sum(gc_delta[, 2]) * 0.0005
  )
  cat(sprintf("   GC delta: %.2f MB, GC events: %d\n",
              results$stringr$gc_delta_mb, results$stringr$gc_count))
  
  # Test 3: stringrs
  cat("3. stringrs (auto)...\n")
  gc(reset = TRUE)
  before_gc <- gc()
  
  res <- string_detect(test_data$strings, test_data$patterns)
  
  after_gc <- gc()
  
  gc_delta <- after_gc - before_gc
  results$stringrs <- list(
    engine = "stringrs_auto",
    gc_delta_mb = sum(gc_delta[, 2]),
    gc_count = sum(gc_delta[, 1]),
    n_cells = sum(gc_delta[, 2]) * 0.0005
  )
  cat(sprintf("   GC delta: %.2f MB, GC events: %d\n",
              results$stringrs$gc_delta_mb, results$stringrs$gc_count))
  
  cat("\n")
  
  # Method 2: Peak memory with gc() trigger
  cat("Running peak memory profiling...\n\n")
  
  peak_mem <- function(fn, label) {
    # Force full GC first
    gc(reset = TRUE)
    
    # Run function and capture peak
    start_mem <- sum(gc()[, 2])
    fn()
    end_mem <- sum(gc()[, 2])
    
    peak_mb <- max(end_mem - start_mem, 0)
    cat(sprintf("  %s: %.2f MB peak\n", label, peak_mb))
    return(peak_mb)
  }
  
  # Measure each
  results$grepl$peak_mb <- peak_mem(function() {
    if (length(test_data$patterns) == 1) {
      grepl(test_data$patterns[1], test_data$strings)
    } else {
      res <- lapply(test_data$patterns, function(p) grepl(p, test_data$strings))
      do.call(cbind, res)
    }
  }, "grepl")
  
  results$stringr$peak_mb <- peak_mem(function() {
    if (length(test_data$patterns) == 1) {
      stringr::str_detect(test_data$strings, test_data$patterns[1])
    } else {
      res <- lapply(test_data$patterns, function(p) stringr::str_detect(test_data$strings, p))
      do.call(cbind, res)
    }
  }, "stringr")
  
  results$stringrs$peak_mb <- peak_mem(function() {
    string_detect(test_data$strings, test_data$patterns)
  }, "stringrs")
  
  # Summary
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("                    MEMORY PROFILE SUMMARY\n")
  cat("═══════════════════════════════════════════════════════════════\n\n")
  
  # Create comparison table
  comparison <- tibble(
    Engine = c("grepl", "stringr", "stringrs_auto"),
    `GC Delta (MB)` = c(results$grepl$gc_delta_mb, 
                        results$stringr$gc_delta_mb, 
                        results$stringrs$gc_delta_mb),
    `Peak Memory (MB)` = c(results$grepl$peak_mb,
                           results$stringr$peak_mb,
                           results$stringrs$peak_mb),
    `GC Events` = c(results$grepl$gc_count,
                    results$stringr$gc_count,
                    results$stringrs$gc_count)
  )
  
  print(comparison, n = Inf)
  
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("Key Insights:\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  
  # Find most efficient
  best_gc <- comparison$Engine[which.min(comparison$`GC Delta (MB)`)]
  best_peak <- comparison$Engine[which.min(comparison$`Peak Memory (MB)`)]
  best_gc_count <- comparison$Engine[which.min(comparison$`GC Events`)]
  
  cat(sprintf("✓ Lowest GC pressure: %s (%.2f MB delta)\n", best_gc, 
              comparison$`GC Delta (MB)`[which.min(comparison$`GC Delta (MB)`)]))
  cat(sprintf("✓ Lowest peak memory: %s (%.2f MB)\n", best_peak,
              comparison$`Peak Memory (MB)`[which.min(comparison$`Peak Memory (MB)`)]))
  cat(sprintf("✓ Fewest GC events: %s (%d events)\n", best_gc_count,
              comparison$`GC Events`[which.min(comparison$`GC Events`)]))
  
  cat("\n")
  cat("Note: Lower GC delta and fewer GC events = better memory efficiency\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  
  invisible(results)
}

#' Main
main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args) < 1) {
    cat("Usage: Rscript benchmark/profiling/memory_profile.R <scenario> [iterations]\n")
    cat("\nQuick scenarios:\n")
    walk(SCENARIOS_QUICK, ~cat(sprintf("  - %s (%d strings x %d patterns)\n", 
                                        .x$name, .x$n_strings, .x$n_patterns)))
    cat("\nFull scenarios:\n")
    walk(head(SCENARIOS_FULL, 5), ~cat(sprintf("  - %s (%d strings x %d patterns)\n", 
                                                .x$name, .x$n_strings, .x$n_patterns)))
    return(NULL)
  }
  
  scenario_name <- args[1]
  profile_memory(scenario_name)
}

if (!interactive()) {
  main()
}
