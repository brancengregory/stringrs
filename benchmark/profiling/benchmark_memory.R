#!/usr/bin/env Rscript
# Universal Memory Benchmark for stringrs
# Works on any R installation using multiple fallback methods
#
# Usage:
#   Rscript benchmark/profiling/benchmark_memory.R <scenario>

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(tibble)
  library(bench)
})

source("benchmark/scenarios.R")
devtools::load_all()

#' Detect available memory profiling capabilities
detect_mem_capabilities <- function() {
  caps <- list(
    profmem = capabilities("profmem"),
    rprofmem = FALSE  # Will check by trying
  )
  
  # Try Rprofmem
  tryCatch({
    tmp <- tempfile()
    Rprofmem(tmp)
    Rprofmem(NULL)
    unlink(tmp)
    caps$rprofmem <- TRUE
  }, error = function(e) {
    caps$rprofmem <- FALSE
  })
  
  caps
}

#' Run comprehensive memory benchmark
benchmark_memory <- function(scenario_name, iterations = 3) {
  # Find scenario
  all_scenarios <- c(SCENARIOS_QUICK, SCENARIOS_FULL)
  scenario_idx <- which(map_chr(all_scenarios, "name") == scenario_name)
  
  if (length(scenario_idx) == 0) {
    stop("Unknown scenario: ", scenario_name)
  }
  
  scenario <- all_scenarios[[scenario_idx]]
  
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  cat(sprintf("  Memory Benchmark: %s\n", scenario_name))
  cat("═══════════════════════════════════════════════════════════════\n")
  cat(sprintf("  Configuration: %s strings × %s patterns (%d chars each)\n",
              format(scenario$n_strings, big.mark = ","),
              format(scenario$n_patterns, big.mark = ","),
              scenario$string_length))
  cat("\n")
  
  # Generate test data
  test_data <- generate_test_data(
    scenario$n_strings,
    scenario$n_patterns,
    scenario$string_length
  )
  
  input_size_mb <- sum(nchar(test_data$strings) + nchar(test_data$patterns)) * 2 / 1024 / 1024
  cat(sprintf("Input data: %.2f MB\n\n", input_size_mb))
  
  results <- list()
  
  # Benchmark each engine
  engines <- list(
    list(name = "grepl", single = function() {
      grepl(test_data$patterns[1], test_data$strings)
    }, multi = function() {
      res <- lapply(test_data$patterns, function(p) grepl(p, test_data$strings))
      do.call(cbind, res)
    }),
    list(name = "stringr", single = function() {
      stringr::str_detect(test_data$strings, test_data$patterns[1])
    }, multi = function() {
      res <- lapply(test_data$patterns, function(p) {
        stringr::str_detect(test_data$strings, p)
      })
      do.call(cbind, res)
    }),
    list(name = "stringrs_auto", single = function() {
      string_detect(test_data$strings, test_data$patterns[1])
    }, multi = function() {
      string_detect(test_data$strings, test_data$patterns)
    }),
    list(name = "stringrs_seq", single = function() {
      string_detect(test_data$strings, test_data$patterns[1],
                   engine = "regex", parallel = "sequential")
    }, multi = function() {
      string_detect(test_data$strings, test_data$patterns,
                   engine = "regex", parallel = "sequential")
    })
  )
  
  for (engine in engines) {
    cat(sprintf("Benchmarking: %s...\n", engine$name))
    
    # Run warmup first
    if (length(test_data$patterns) == 1) {
      engine$single()
    } else {
      engine$multi()
    }
    
    # Force GC before measurement
    gc(reset = TRUE)
    
    # Run benchmark with memory tracking
    if (length(test_data$patterns) == 1) {
      bm <- bench::mark(
        engine$single(),
        iterations = iterations,
        check = FALSE,
        filter_gc = FALSE  # Don't filter GC runs
      )
    } else {
      bm <- bench::mark(
        engine$multi(),
        iterations = iterations,
        check = FALSE,
        filter_gc = FALSE
      )
    }
    
    # Calculate results
    # bench::mark stores memory in mem_alloc column
    mem_alloc_bytes <- as.numeric(bm$mem_alloc[[1]])
    
    # If mem_alloc is NA, estimate from gc() delta
    if (is.na(mem_alloc_bytes) || mem_alloc_bytes == 0) {
      gc_after <- gc()
      mem_alloc_bytes <- sum(gc_after[, 2]) * 1024 * 1024  # Convert MB to bytes
    }
    
    results[[engine$name]] <- list(
      engine = engine$name,
      time_ms = as.numeric(bm$median) * 1000,
      mem_bytes = mem_alloc_bytes,
      mem_mb = mem_alloc_bytes / 1024 / 1024,
      throughput = scenario$n_strings / as.numeric(bm$median),
      n_gc = nrow(bm$gc[[1]])
    )
    
    cat(sprintf("  ✓ Time: %.2f ms\n", results[[engine$name]]$time_ms))
    cat(sprintf("  ✓ Memory: %.2f MB\n", results[[engine$name]]$mem_mb))
    cat(sprintf("  ✓ Throughput: %s strings/sec\n",
                format(results[[engine$name]]$throughput, scientific = FALSE, big.mark = ",")))
    cat("\n")
  }
  
  # Comparison table
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("                    BENCHMARK RESULTS\n")
  cat("═══════════════════════════════════════════════════════════════\n\n")
  
  comparison <- map_dfr(results, function(r) {
    tibble(
      Engine = r$engine,
      `Time (ms)` = sprintf("%.2f", r$time_ms),
      `Memory (MB)` = sprintf("%.2f", r$mem_mb),
      `Throughput` = sprintf("%s/s", format(r$throughput, scientific = FALSE, big.mark = ",")),
      `GC Events` = r$n_gc
    )
  })
  
  print(comparison, n = Inf)
  
  # Speedup analysis
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("                    SPEEDUP ANALYSIS\n")
  cat("═══════════════════════════════════════════════════════════════\n\n")
  
  baseline_time <- results$grepl$time_ms
  baseline_mem <- results$grepl$mem_mb
  
  for (name in names(results)) {
    if (name != "grepl") {
      r <- results[[name]]
      time_speedup <- baseline_time / r$time_ms
      mem_ratio <- r$mem_mb / baseline_mem
      
      cat(sprintf("%s:\n", r$engine))
      cat(sprintf("  • %.1fx faster than grepl\n", time_speedup))
      cat(sprintf("  • Memory ratio: %.2fx (%.2f MB vs %.2f MB)\n",
                  mem_ratio, r$mem_mb, baseline_mem))
      
      if (mem_ratio <= 1.5) {
        cat(sprintf("  • ✓ Memory efficient (only %.1fx more)\n", mem_ratio))
      } else if (mem_ratio <= 5) {
        cat(sprintf("  • ⚠ Moderate memory overhead (%.1fx more)\n", mem_ratio))
      } else {
        cat(sprintf("  • ✗ High memory overhead (%.1fx more)\n", mem_ratio))
      }
      cat("\n")
    }
  }
  
  # Best engine recommendations
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("                    RECOMMENDATIONS\n")
  cat("═══════════════════════════════════════════════════════════════\n\n")
  
  fastest <- results[[which.min(map_dbl(results, function(r) r$time_ms))]]
  most_efficient <- results[[which.min(map_dbl(results, function(r) r$mem_mb))]]
  best_throughput <- results[[which.max(map_dbl(results, function(r) r$throughput))]]
  
  cat(sprintf("🚀 Fastest: %s (%.2f ms)\n", fastest$engine, fastest$time_ms))
  cat(sprintf("💾 Most memory efficient: %s (%.2f MB)\n", most_efficient$engine, most_efficient$mem_mb))
  cat(sprintf("⚡ Best throughput: %s (%s strings/sec)\n",
              best_throughput$engine,
              format(best_throughput$throughput, scientific = FALSE, big.mark = ",")))
  
  cat("\n")
  
  # Context-aware recommendation
  if (fastest$engine == most_efficient$engine) {
    cat(sprintf("✨ %s is the clear winner - fastest AND most memory efficient!\n", fastest$engine))
  } else if (fastest$time_ms / most_efficient$time_ms < 2) {
    cat(sprintf("💡 %s is only slightly slower but much more memory efficient.\n",
                most_efficient$engine))
  } else {
    cat(sprintf("🎯 Trade-off: Use %s for speed, %s for memory efficiency\n",
                fastest$engine, most_efficient$engine))
  }
  
  cat("\n═══════════════════════════════════════════════════════════════\n")
  
  invisible(results)
}

#' Main
main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args) < 1) {
    cat("Usage: Rscript benchmark/profiling/benchmark_memory.R <scenario> [iterations]\n\n")
    cat("Examples:\n")
    cat("  Rscript benchmark/profiling/benchmark_memory.R quick_single\n")
    cat("  Rscript benchmark/profiling/benchmark_memory.R medium_multi 5\n")
    cat("  Rscript benchmark/profiling/benchmark_memory.R many_patterns_small\n\n")
    cat("Quick scenarios:\n")
    walk(SCENARIOS_QUICK, ~cat(sprintf("  - %s\n", .x$name)))
    cat("\nFull scenarios (partial list):\n")
    walk(head(SCENARIOS_FULL, 5), ~cat(sprintf("  - %s\n", .x$name)))
    return(NULL)
  }
  
  scenario_name <- args[1]
  iterations <- if (length(args) >= 2) as.numeric(args[2]) else 3
  
  benchmark_memory(scenario_name, iterations)
}

if (!interactive()) {
  main()
}
