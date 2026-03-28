#!/usr/bin/env Rscript
# Benchmark engine definitions
#
# This file defines all benchmark implementations with a unified interface.
# Each engine function takes strings and patterns and returns benchmark results.

#' Run all benchmarks for a scenario
run_scenario_benchmarks <- function(test_data, workers = 4, iterations = 3, 
                                    profile_memory = FALSE) {
  results <- list()
  start_time <- Sys.time()
  
  # R Base implementations
  cat("  Running R base implementations...\n")
  results$r_base <- list(
    grepl = benchmark_engine("grepl", test_data, iterations),
    stringr = benchmark_engine("stringr", test_data, iterations),
    stringi = benchmark_engine("stringi", test_data, iterations)
  )
  
  # R Parallel implementations
  cat("  Running parallel implementations...\n")
  if (requireNamespace("mirai", quietly = TRUE)) {
    results$mirai <- list(
      string_parallel = benchmark_engine("mirai_string", test_data, iterations, workers = workers),
      pattern_parallel = benchmark_engine("mirai_pattern", test_data, iterations, workers = workers)
    )
  }
  
  if (requireNamespace("furrr", quietly = TRUE) && requireNamespace("future", quietly = TRUE)) {
    results$furrr <- list(
      string_parallel = benchmark_engine("furrr_string", test_data, iterations, workers = workers),
      pattern_parallel = benchmark_engine("furrr_pattern", test_data, iterations, workers = workers)
    )
  }
  
  # stringrs implementation (single function with auto-optimization)
  cat("  Running stringrs implementation...\n")
  results$stringrs <- list(
    auto = benchmark_engine("stringrs_auto", test_data, iterations)
  )
  
  # Calculate elapsed time
  end_time <- Sys.time()
  elapsed <- as.numeric(end_time - start_time, units = "secs")
  
  # Extract best result
  all_times <- map_dbl(unlist(results, recursive = FALSE), ~.$median_time_ms)
  best_idx <- which.min(all_times)
  best_engine <- names(all_times)[best_idx]
  best_time_ms <- all_times[best_idx]
  
  # Calculate speedups vs different baselines
  # Base R (grepl) - reference point
  base_time_grepl <- results$r_base$grepl$median_time_ms
  speedup_vs_grepl <- base_time_grepl / best_time_ms
  
  # stringr - idiomatic R (what most users actually use)
  base_time_stringr <- results$r_base$stringr$median_time_ms
  speedup_vs_stringr <- base_time_stringr / best_time_ms
  
  # stringi - fastest pure R implementation
  base_time_stringi <- results$r_base$stringi$median_time_ms
  speedup_vs_stringi <- base_time_stringi / best_time_ms
  
  list(
    results = results,
    elapsed = elapsed,
    best_engine = best_engine,
    best_time_ms = best_time_ms,
    speedup_vs_grepl = speedup_vs_grepl,
    speedup_vs_stringr = speedup_vs_stringr,
    speedup_vs_stringi = speedup_vs_stringi,
    # Keep old name for backwards compatibility
    speedup_vs_base = speedup_vs_grepl
  )
}

#' Benchmark a single engine
benchmark_engine <- function(engine_name, test_data, iterations, workers = NULL) {
  tryCatch({
    result <- run_engine_benchmark(engine_name, test_data, iterations, workers)
    list(
      engine = engine_name,
      success = TRUE,
      median_time_ms = result$median,
      min_time_ms = result$min,
      max_time_ms = result$max,
      mem_alloc_mb = result$mem_alloc / 1024^2,
      throughput = test_data$n_strings / (result$median / 1000),  # strings/sec
      error = NULL
    )
  }, error = function(e) {
    list(
      engine = engine_name,
      success = FALSE,
      median_time_ms = NA,
      min_time_ms = NA,
      max_time_ms = NA,
      mem_alloc_mb = NA,
      throughput = NA,
      error = conditionMessage(e)
    )
  })
}

#' Run actual benchmark for an engine
run_engine_benchmark <- function(engine, test_data, iterations, workers = NULL) {
  # Create a local environment with the test data
  bench_env <- new.env()
  bench_env$strings <- test_data$strings
  bench_env$patterns <- test_data$patterns
  bench_env$workers <- workers
  
  # Get the expression for this engine
  expr <- get_engine_expression(engine)
  
  # Run benchmark in the local environment
  bench_result <- bench::mark(
    { eval(expr, bench_env) },
    iterations = iterations,
    check = FALSE
  )
  
  # bench stores times in a list column - need to unlist and convert
  times <- as.numeric(unlist(bench_result$time))  # Convert all iteration times
  mem_allocs <- as.numeric(unlist(bench_result$mem_alloc))
  
  list(
    median = median(times) * 1000,  # Convert to ms
    min = min(times) * 1000,
    max = max(times) * 1000,
    mem_alloc = sum(mem_allocs, na.rm = TRUE)
  )
}

#' Get benchmark expression for an engine
#' Returns an expression that references 'strings', 'patterns', and 'workers'
get_engine_expression <- function(engine) {
  switch(engine,
    # R Base
    "grepl" = quote({
      if (length(patterns) == 1) {
        grepl(patterns[1], strings)
      } else {
        res <- lapply(patterns, function(p) grepl(p, strings))
        do.call(cbind, res)
      }
    }),
    
    "stringr" = quote({
      if (length(patterns) == 1) {
        stringr::str_detect(strings, patterns[1])
      } else {
        res <- lapply(patterns, function(p) stringr::str_detect(strings, p))
        do.call(cbind, res)
      }
    }),
    
    "stringi" = quote({
      if (length(patterns) == 1) {
        stringi::stri_detect_regex(strings, patterns[1])
      } else {
        res <- lapply(patterns, function(p) stringi::stri_detect_regex(strings, p))
        do.call(cbind, res)
      }
    }),
    
    # Parallel - mirai
    "mirai_string" = quote({
      mirai::daemons(workers)
      on.exit(mirai::daemons(0), add = TRUE)
      
      if (length(patterns) == 1) {
        res <- mirai::mirai_map(strings, function(s, p) {
          grepl(p, s)
        }, .args = list(p = patterns[1]))[]
        unlist(res)
      } else {
        res <- mirai::mirai_map(strings, function(s, patterns) {
          sapply(patterns, function(p) grepl(p, s))
        }, .args = list(patterns = patterns))[]
        do.call(rbind, res)
      }
    }),
    
    "mirai_pattern" = quote({
      mirai::daemons(workers)
      on.exit(mirai::daemons(0), add = TRUE)
      
      res <- mirai::mirai_map(patterns, function(p, strings) {
        grepl(p, strings)
      }, .args = list(strings = strings))[]
      do.call(cbind, res)
    }),
    
    # Parallel - furrr
    "furrr_string" = quote({
      future::plan(future::multisession, workers = workers)
      on.exit(future::plan(future::sequential), add = TRUE)
      
      if (length(patterns) == 1) {
        furrr::future_map_lgl(strings, ~grepl(patterns[1], .x))
      } else {
        res <- furrr::future_map(strings, function(s) {
          sapply(patterns, function(p) grepl(p, s))
        })
        do.call(rbind, res)
      }
    }),
    
    "furrr_pattern" = quote({
      future::plan(future::multisession, workers = workers)
      on.exit(future::plan(future::sequential), add = TRUE)
      
      res <- furrr::future_map(patterns, ~grepl(.x, strings))
      do.call(cbind, res)
    }),
    
    # stringrs - single function with auto-optimization
    "stringrs_auto" = quote(
      stringrs::string_detect(strings, patterns)
    ),
    
    stop("Unknown engine: ", engine)
  )
}
