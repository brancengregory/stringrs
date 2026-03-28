#!/usr/bin/env Rscript
# Memory Performance Test Suite for DashMap Implementation
#
# Tests cache memory efficiency, parallel memory usage, and peak memory consumption
#
# Usage:
#   Rscript benchmark/profiling/test_dashmap_memory.R [test_name]

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(tibble)
  library(bench)
})

devtools::load_all()

#' Test 1: Cache Memory Growth with Many Patterns
#' Measures how memory grows as we compile more patterns
test_cache_memory_growth <- function() {
  cat("\n═══════════════════════════════════════════════════════════════\n")
  cat("TEST 1: Cache Memory Growth with Many Patterns\n")
  cat("═══════════════════════════════════════════════════════════════\n\n")
  
  # Generate test strings
  strings <- replicate(1000, paste(sample(letters, 50, TRUE), collapse = ""))
  
  # Test with increasing numbers of patterns
  pattern_counts <- c(10, 50, 100, 250, 500, 1000)
  results <- list()
  
  for (n_patterns in pattern_counts) {
    # Generate unique patterns
    set.seed(42)
    patterns <- replicate(n_patterns, {
      paste(sample(letters, sample(3:8, 1)), collapse = "")
    })
    
    # Force GC before test
    gc(reset = TRUE)
    
    # Run detection to populate cache
    result <- string_detect(strings, patterns)
    
    # Measure memory after
    gc_info <- gc()
    mem_used_mb <- sum(gc_info[, 2])  # MB used
    
    results[[as.character(n_patterns)]] <- list(
      n_patterns = n_patterns,
      mem_mb = mem_used_mb,
      strings_matched = sum(as.matrix(result[, -1]))
    )
    
    cat(sprintf("  %4d patterns: %6.2f MB used\n", n_patterns, mem_used_mb))
  }
  
  # Calculate memory per pattern
  mem_100 <- results[["100"]]$mem_mb
  mem_1000 <- results[["1000"]]$mem_mb
  mem_per_pattern <- (mem_1000 - mem_100) / 900
  
  cat(sprintf("\n📊 Memory per additional pattern: %.3f MB\n", mem_per_pattern))
  
  if (mem_per_pattern < 0.1) {
    cat("✓ EXCELLENT: Low memory overhead per pattern\n")
  } else if (mem_per_pattern < 0.5) {
    cat("✓ GOOD: Moderate memory overhead per pattern\n")
  } else {
    cat("⚠ WARNING: High memory overhead per pattern\n")
  }
  
  invisible(results)
}

#' Test 2: Parallel Memory Contention
#' Tests memory usage during parallel operations vs sequential
test_parallel_memory <- function() {
  cat("\n═══════════════════════════════════════════════════════════════\n")
  cat("TEST 2: Parallel Memory Contention\n")
  cat("═══════════════════════════════════════════════════════════════\n\n")
  
  # Large dataset to trigger parallel processing
  strings <- replicate(50000, paste(sample(letters, 100, TRUE), collapse = ""))
  patterns <- replicate(50, paste(sample(letters, 4), collapse = ""))
  
  # Run multiple parallel operations to stress cache
  n_runs <- 5
  mem_results <- numeric(n_runs)
  
  cat(sprintf("  Running %d parallel operations...\n", n_runs))
  
  for (i in 1:n_runs) {
    gc(reset = TRUE)
    
    # This will use parallel processing (auto-detected)
    result <- string_detect(strings, patterns)
    
    gc_info <- gc()
    mem_results[i] <- sum(gc_info[, 2])
    
    cat(sprintf("    Run %d: %.2f MB\n", i, mem_results[i]))
  }
  
  # Check for memory stability
  mem_variance <- var(mem_results)
  mem_mean <- mean(mem_results)
  cv <- sqrt(mem_variance) / mem_mean  # Coefficient of variation
  
  cat(sprintf("\n📊 Memory usage statistics:\n"))
  cat(sprintf("  Mean: %.2f MB\n", mem_mean))
  cat(sprintf("  StdDev: %.2f MB\n", sqrt(mem_variance)))
  cat(sprintf("  Coefficient of Variation: %.2f%%\n", cv * 100))
  
  if (cv < 0.05) {
    cat("✓ EXCELLENT: Stable memory across parallel runs\n")
  } else if (cv < 0.15) {
    cat("✓ GOOD: Acceptable memory variance\n")
  } else {
    cat("⚠ WARNING: High memory variance (possible leak)\n")
  }
  
  invisible(mem_results)
}

#' Test 3: Cache Hit Rate
#' Measures cache efficiency by repeatedly using same patterns
test_cache_hit_rate <- function() {
  cat("\n═══════════════════════════════════════════════════════════════\n")
  cat("TEST 3: Cache Hit Rate & Reuse Efficiency\n")
  cat("═══════════════════════════════════════════════════════════════\n\n")
  
  strings <- replicate(5000, paste(sample(letters, 50, TRUE), collapse = ""))
  
  # Fixed set of patterns we'll reuse
  patterns <- replicate(20, paste(sample(letters, 4), collapse = ""))
  
  # Time multiple runs (cache should improve subsequent runs)
  times <- numeric(5)
  
  for (i in 1:5) {
    gc(reset = TRUE)
    
    bm <- bench::mark(
      string_detect(strings, patterns),
      iterations = 1,
      check = FALSE
    )
    
    times[i] <- as.numeric(bm$median) * 1000  # Convert to ms
    cat(sprintf("  Run %d: %.2f ms\n", i, times[i]))
  }
  
  # Calculate speedup from first to subsequent runs
  speedup <- times[1] / mean(times[2:5])
  
  cat(sprintf("\n📊 Cache performance:\n"))
  cat(sprintf("  First run (cold cache): %.2f ms\n", times[1]))
  cat(sprintf("  Avg subsequent runs: %.2f ms\n", mean(times[2:5])))
  cat(sprintf("  Speedup: %.2fx\n", speedup))
  
  if (speedup > 1.5) {
    cat("✓ EXCELLENT: Cache provides significant speedup\n")
  } else if (speedup > 1.1) {
    cat("✓ GOOD: Moderate cache benefit\n")
  } else {
    cat("⚠ Cache may not be working effectively\n")
  }
  
  invisible(times)
}

#' Test 4: Memory Efficiency Comparison
#' Compares memory usage vs other implementations
test_memory_comparison <- function() {
  cat("\n═══════════════════════════════════════════════════════════════\n")
  cat("TEST 4: Memory Efficiency Comparison\n")
  cat("═══════════════════════════════════════════════════════════════\n\n")
  
  strings <- replicate(10000, paste(sample(letters, 100, TRUE), collapse = ""))
  patterns <- replicate(10, paste(sample(letters, 5), collapse = ""))
  
  results <- list()
  
  # Test grepl (baseline)
  gc(reset = TRUE)
  res_grepl <- lapply(patterns, function(p) grepl(p, strings))
  gc_info <- gc()
  results$grepl <- sum(gc_info[, 2])
  cat(sprintf("  grepl:        %.2f MB\n", results$grepl))
  
  # Test stringr
  gc(reset = TRUE)
  res_stringr <- lapply(patterns, function(p) stringr::str_detect(strings, p))
  gc_info <- gc()
  results$stringr <- sum(gc_info[, 2])
  cat(sprintf("  stringr:      %.2f MB\n", results$stringr))
  
  # Test stringrs
  gc(reset = TRUE)
  res_stringrs <- string_detect(strings, patterns)
  gc_info <- gc()
  results$stringrs <- sum(gc_info[, 2])
  cat(sprintf("  stringrs:     %.2f MB\n", results$stringrs))
  
  # Analysis
  cat(sprintf("\n📊 Memory efficiency:\n"))
  
  ratio_vs_grepl <- results$stringrs / results$grepl
  ratio_vs_stringr <- results$stringrs / results$stringr
  
  cat(sprintf("  vs grepl:   %.2fx (%.2f MB vs %.2f MB)\n", 
              ratio_vs_grepl, results$stringrs, results$grepl))
  cat(sprintf("  vs stringr: %.2fx (%.2f MB vs %.2f MB)\n",
              ratio_vs_stringr, results$stringrs, results$stringr))
  
  if (ratio_vs_stringr < 1.5) {
    cat("✓ EXCELLENT: Memory competitive with stringr\n")
  } else if (ratio_vs_stringr < 3) {
    cat("✓ GOOD: Reasonable memory overhead for speed\n")
  } else {
    cat("⚠ WARNING: High memory overhead\n")
  }
  
  invisible(results)
}

#' Test 5: Peak Memory Under Load
#' Tests maximum memory usage with large datasets
test_peak_memory <- function() {
  cat("\n═══════════════════════════════════════════════════════════════\n")
  cat("TEST 5: Peak Memory Under Load\n")
  cat("═══════════════════════════════════════════════════════════════\n\n")
  
  # Progressively larger datasets
  test_sizes <- list(
    list(n = 10000, patterns = 10, name = "Medium"),
    list(n = 50000, patterns = 20, name = "Large"),
    list(n = 100000, patterns = 50, name = "Very Large")
  )
  
  peak_memories <- numeric(length(test_sizes))
  
  for (i in seq_along(test_sizes)) {
    size <- test_sizes[[i]]
    
    cat(sprintf("  Testing %s (%s strings × %s patterns)...\n",
                size$name, format(size$n, big.mark = ","),
                format(size$patterns, big.mark = ",")))
    
    # Generate data
    strings <- replicate(size$n, paste(sample(letters, 50, TRUE), collapse = ""))
    patterns <- replicate(size$patterns, paste(sample(letters, 4), collapse = ""))
    
    # Clear memory before test
    gc(reset = TRUE)
    base_mem <- sum(gc()[, 2])
    
    # Run operation
    result <- string_detect(strings, strings[1:size$patterns])
    
    # Measure peak
    gc_info <- gc()
    peak_mem <- sum(gc_info[, 2])
    peak_memories[i] <- peak_mem
    
    result_size_mb <- (size$n * size$patterns * 4) / 1024 / 1024  # 4 bytes per bool
    
    cat(sprintf("    Peak memory: %.2f MB\n", peak_mem))
    cat(sprintf("    Result size: %.2f MB\n", result_size_mb))
    cat(sprintf("    Overhead: %.2fx result size\n", peak_mem / result_size_mb))
  }
  
  cat(sprintf("\n📊 Peak memory summary:\n"))
  for (i in seq_along(test_sizes)) {
    cat(sprintf("  %s: %.2f MB\n", test_sizes[[i]]$name, peak_memories[i]))
  }
  
  invisible(peak_memories)
}

#' Run all tests
run_all_tests <- function() {
  cat("\n")
  cat("╔═══════════════════════════════════════════════════════════════╗\n")
  cat("║    DASHMAP MEMORY PERFORMANCE TEST SUITE                    ║\n")
  cat("╚═══════════════════════════════════════════════════════════════╝\n")
  cat("\n")
  cat("This suite tests memory efficiency of the DashMap cache implementation:\n")
  cat("  • Cache memory growth per pattern\n")
  cat("  • Parallel memory contention\n")
  cat("  • Cache hit rate and reuse\n")
  cat("  • Memory efficiency vs alternatives\n")
  cat("  • Peak memory under load\n")
  cat("\n")
  
  test_cache_memory_growth()
  test_parallel_memory()
  test_cache_hit_rate()
  test_memory_comparison()
  test_peak_memory()
  
  cat("\n╔═══════════════════════════════════════════════════════════════╗\n")
  cat("║                   TESTS COMPLETE                            ║\n")
  cat("╚═══════════════════════════════════════════════════════════════╝\n")
  cat("\n✅ All memory performance tests completed\n")
  cat("\nTo revert to Mutex implementation:\n")
  cat("  git checkout HEAD~1 -- src/rust/Cargo.toml src/rust/src/lib.rs\n")
  cat("  Rscript -e 'devtools::load_all()'\n\n")
}

#' Run specific test
run_single_test <- function(test_name) {
  switch(test_name,
    "cache_growth" = test_cache_memory_growth(),
    "parallel" = test_parallel_memory(),
    "hit_rate" = test_cache_hit_rate(),
    "comparison" = test_memory_comparison(),
    "peak" = test_peak_memory(),
    {
      cat("Unknown test: ", test_name, "\n")
      cat("Available tests: cache_growth, parallel, hit_rate, comparison, peak\n")
    }
  )
}

# Main
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args) == 0) {
    run_all_tests()
  } else {
    run_single_test(args[1])
  }
}
