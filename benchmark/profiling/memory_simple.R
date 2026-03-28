#!/usr/bin/env Rscript
# Memory Profiling for stringrs
#
# This script profiles memory usage using multiple methods:
# 1. R's built-in gc() tracking
# 2. bench::mark memory measurements
# 3. System memory monitoring
#
# Usage:
#   Rscript benchmark/profiling/memory_simple.R <scenario>
#   Rscript benchmark/profiling/memory_simple.R medium_multi

suppressPackageStartupMessages({
  library(dplyr)
  library(bench)
  library(purrr)
})

source("benchmark/scenarios.R")
devtools::load_all()

#' Profile memory for a scenario
profile_scenario_memory <- function(scenario_name, iterations = 3) {
  # Find scenario
  scenario_idx <- which(map_chr(SCENARIOS_FULL, "name") == scenario_name)
  if (length(scenario_idx) == 0) {
    # Try quick scenarios
    scenario_idx <- which(map_chr(SCENARIOS_QUICK, "name") == scenario_name)
    if (length(scenario_idx) == 0) {
      stop("Unknown scenario: ", scenario_name)
    }
    scenario <- SCENARIOS_QUICK[[scenario_idx]]
  } else {
    scenario <- SCENARIOS_FULL[[scenario_idx]]
  }
  
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  cat(sprintf("  Memory Profile: %s\n", scenario_name))
  cat("═══════════════════════════════════════════════════════════════\n")
  cat(sprintf("  Strings: %s\n", format(scenario$n_strings, big.mark = ",")))
  cat(sprintf("  Patterns: %s\n", format(scenario$n_patterns, big.mark = ",")))
  cat(sprintf("  String length: %s\n", scenario$string_length))
  cat("\n")
  
  # Generate test data
  test_data <- generate_test_data(
    scenario$n_strings,
    scenario$n_patterns,
    scenario$string_length
  )
  
  # Calculate data size
  strings_size <- sum(nchar(test_data$strings)) * 2  # UTF-16 ~2 bytes per char
  patterns_size <- sum(nchar(test_data$patterns)) * 2
  total_input_mb <- (strings_size + patterns_size) / 1024 / 1024
  
  cat(sprintf("Input data size: %.2f MB\n", total_input_mb))
  cat("\n")
  
  # Results storage
  results <- list()
  
  # Test 1: R base grepl
  if (length(test_data$patterns) == 1) {
    cat("Profiling: R base grepl...\n")
    gc(reset = TRUE)
    before_gc <- gc()
    
    bm <- bench::mark(
      grepl(test_data$patterns[1], test_data$strings),
      iterations = iterations,
      check = FALSE
    )
    
    after_gc <- gc()
    
    results$r_base <- list(
      engine = "grepl",
      time_ms = as.numeric(bm$median) * 1000,
      mem_alloc_mb = as.numeric(bm$mem_alloc) / 1024^2,
      gc_count = sum(after_gc[, 1] - before_gc[, 1]),
      throughput = scenario$n_strings / as.numeric(bm$median)
    )
  } else {
    cat("Profiling: R base grepl loop...\n")
    gc(reset = TRUE)
    before_gc <- gc()
    
    bm <- bench::mark(
      {
        res <- lapply(test_data$patterns, function(p) grepl(p, test_data$strings))
        do.call(cbind, res)
      },
      iterations = iterations,
      check = FALSE
    )
    
    after_gc <- gc()
    
    results$r_base <- list(
      engine = "grepl_loop",
      time_ms = as.numeric(bm$median) * 1000,
      mem_alloc_mb = as.numeric(bm$mem_alloc) / 1024^2,
      gc_count = sum(after_gc[, 1] - before_gc[, 1]),
      throughput = scenario$n_strings / as.numeric(bm$median)
    )
  }
  
  cat(sprintf("  Time: %.2f ms\n", results$r_base$time_ms))
  cat(sprintf("  Memory: %.2f MB\n", results$r_base$mem_alloc_mb))
  cat(sprintf("  GC events: %d\n", results$r_base$gc_count))
  cat("\n")
  
  # Test 2: stringr
  cat("Profiling: stringr::str_detect...\n")
  gc(reset = TRUE)
  before_gc <- gc()
  
  if (length(test_data$patterns) == 1) {
    bm <- bench::mark(
      stringr::str_detect(test_data$strings, test_data$patterns[1]),
      iterations = iterations,
      check = FALSE
    )
  } else {
    bm <- bench::mark(
      {
        res <- lapply(test_data$patterns, function(p) stringr::str_detect(test_data$strings, p))
        do.call(cbind, res)
      },
      iterations = iterations,
      check = FALSE
    )
  }
  
  after_gc <- gc()
  
  results$stringr <- list(
    engine = "stringr",
    time_ms = as.numeric(bm$median) * 1000,
    mem_alloc_mb = as.numeric(bm$mem_alloc) / 1024^2,
    gc_count = sum(after_gc[, 1] - before_gc[, 1]),
    throughput = scenario$n_strings / as.numeric(bm$median)
  )
  
  cat(sprintf("  Time: %.2f ms\n", results$stringr$time_ms))
  cat(sprintf("  Memory: %.2f MB\n", results$stringr$mem_alloc_mb))
  cat(sprintf("  GC events: %d\n", results$stringr$gc_count))
  cat("\n")
  
  # Test 3: stringrs (default auto mode)
  cat("Profiling: stringrs (auto mode)...\n")
  gc(reset = TRUE)
  before_gc <- gc()
  
  bm <- bench::mark(
    string_detect(test_data$strings, test_data$patterns),
    iterations = iterations,
    check = FALSE
  )
  
  after_gc <- gc()
  
  results$stringrs <- list(
    engine = "stringrs_auto",
    time_ms = as.numeric(bm$median) * 1000,
    mem_alloc_mb = as.numeric(bm$mem_alloc) / 1024^2,
    gc_count = sum(after_gc[, 1] - before_gc[, 1]),
    throughput = scenario$n_strings / as.numeric(bm$median)
  )
  
  cat(sprintf("  Time: %.2f ms\n", results$stringrs$time_ms))
  cat(sprintf("  Memory: %.2f MB\n", results$stringrs$mem_alloc_mb))
  cat(sprintf("  GC events: %d\n", results$stringrs$gc_count))
  cat("\n")
  
  # Test 4: stringrs (sequential - minimal overhead)
  if (length(test_data$strings) > 1000 || length(test_data$patterns) > 5) {
    cat("Profiling: stringrs (sequential mode)...\n")
    gc(reset = TRUE)
    before_gc <- gc()
    
    bm <- bench::mark(
      string_detect(test_data$strings, test_data$patterns, 
                    engine = "regex", parallel = "sequential"),
      iterations = iterations,
      check = FALSE
    )
    
    after_gc <- gc()
    
    results$stringrs_seq <- list(
      engine = "stringrs_seq",
      time_ms = as.numeric(bm$median) * 1000,
      mem_alloc_mb = as.numeric(bm$mem_alloc) / 1024^2,
      gc_count = sum(after_gc[, 1] - before_gc[, 1]),
      throughput = scenario$n_strings / as.numeric(bm$median)
    )
    
    cat(sprintf("  Time: %.2f ms\n", results$stringrs_seq$time_ms))
    cat(sprintf("  Memory: %.2f MB\n", results$stringrs_seq$mem_alloc_mb))
    cat(sprintf("  GC events: %d\n", results$stringrs_seq$gc_count))
    cat("\n")
  }
  
  # Calculate memory efficiency
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("                    MEMORY EFFICIENCY ANALYSIS\n")
  cat("═══════════════════════════════════════════════════════════════\n\n")
  
  base_mem <- results$r_base$mem_alloc_mb
  
  for (name in names(results)) {
    r <- results[[name]]
    mem_ratio <- r$mem_alloc_mb / base_mem
    time_ratio <- r$time_ms / results$r_base$time_ms
    efficiency <- base_mem / (r$mem_alloc_mb * (r$time_ms / results$r_base$time_ms))
    
    cat(sprintf("%-20s: %.2f MB (%.1f%% of grepl) | %.2f ms | Efficiency: %.2f\n",
                r$engine,
                r$mem_alloc_mb,
                mem_ratio * 100,
                r$time_ms,
                efficiency))
  }
  
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  cat(sprintf("  Input data: %.2f MB\n", total_input_mb))
  cat(sprintf("  Best memory: %s (%.2f MB)\n", 
              results[[which.min(map_dbl(results, function(r) r$mem_alloc_mb))]]$engine,
              min(map_dbl(results, function(r) r$mem_alloc_mb))))
  cat(sprintf("  Best speed: %s (%.2f ms)\n",
              results[[which.min(map_dbl(results, function(r) r$time_ms))]]$engine,
              min(map_dbl(results, function(r) r$time_ms))))
  cat("═══════════════════════════════════════════════════════════════\n")
  
  # Return results
  invisible(list(
    scenario = scenario,
    results = results,
    input_size_mb = total_input_mb
  ))
}

#' Main execution
main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args) < 1) {
    cat("Usage: Rscript benchmark/profiling/memory_simple.R <scenario>\n")
    cat("\nQuick scenarios:\n")
    walk(SCENARIOS_QUICK, ~cat(sprintf("  - %s\n", .x$name)))
    cat("\nFull scenarios:\n")
    walk(SCENARIOS_FULL, ~cat(sprintf("  - %s\n", .x$name)))
    return(NULL)
  }
  
  scenario_name <- args[1]
  iterations <- if (length(args) >= 2) as.numeric(args[2]) else 3
  
  profile_scenario_memory(scenario_name, iterations)
}

if (!interactive()) {
  main()
}
