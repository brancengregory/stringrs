#!/usr/bin/env Rscript
# Memory profiling for stringrs
#
# This script profiles memory allocation patterns in the Rust code
# using dhat (heap profiling) or heaptrack (external tool).
#
# Usage:
#   Rscript benchmark/profiling/profile-memory.R <scenario>
#   Rscript benchmark/profiling/profile-memory.R medium_multi

suppressPackageStartupMessages({
  library(dplyr)
})

source("benchmark/scenarios.R")
devtools::load_all()

#' Profile memory with dhat (compile-time profiler)
profile_with_dhat <- function(scenario_name) {
  cat("Building with dhat profiling enabled...\n")
  
  # Set environment variable to enable dhat
  Sys.setenv(DHAT_ENABLED = "1")
  
  # Rebuild Rust code with dhat feature
  cmd <- "cd src/rust && cargo build --release --features dhat-heap 2>&1"
  output <- system(cmd, intern = TRUE)
  
  if (any(grepl("error", output, ignore.case = TRUE))) {
    stop("Build failed: ", paste(output, collapse = "\n"))
  }
  
  cat("Build complete. Running scenario...\n")
  
  # Load scenario
  scenario <- SCENARIOS_FULL[[which(map_chr(SCENARIOS_FULL, "name") == scenario_name)]]
  if (is.null(scenario)) {
    stop("Unknown scenario: ", scenario_name)
  }
  
  # Generate test data
  test_data <- generate_test_data(
    scenario$n_strings,
    scenario$n_patterns,
    scenario$string_length
  )
  
  # Run the function (dhat will capture allocations)
  cat(sprintf("Running %s scenario...\n", scenario_name))
  result <- string_detect(test_data$strings, test_data$patterns)
  
  # dhat generates a .json file - parse it
  dhat_files <- list.files("src/rust", pattern = "dhat.*\\.json$", recursive = TRUE)
  if (length(dhat_files) > 0) {
    newest <- sort(file.path("src/rust", dhat_files), decreasing = TRUE)[1]
    cat(sprintf("dhat output: %s\n", newest))
    
    # Parse and report
    parse_dhat_results(newest, scenario)
  } else {
    cat("No dhat output file found\n")
  }
  
  # Cleanup
  Sys.unsetenv("DHAT_ENABLED")
  
  invisible(NULL)
}

#' Parse dhat JSON output
parse_dhat_results <- function(dhat_file, scenario) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    cat("Install jsonlite to parse dhat results\n")
    return(NULL)
  }
  
  data <- jsonlite::fromJSON(dhat_file)
  
  # Extract key metrics
  total_bytes <- data$info$total_bytes
  total_blocks <- data$info$total_blocks
  peak_bytes <- max(map_dbl(data$phases, "total_bytes"), na.rm = TRUE)
  
  cat("\n=== Memory Profile ===\n")
  cat(sprintf("Scenario: %s (%s strings × %s patterns)\n",
              scenario$name,
              format(scenario$n_strings, big.mark = ","),
              format(scenario$n_patterns, big.mark = ",")))
  cat(sprintf("Total allocations: %s bytes (%s blocks)\n",
              format(total_bytes, big.mark = ","),
              format(total_blocks, big.mark = ",")))
  cat(sprintf("Peak memory: %s bytes\n", format(peak_bytes, big.mark = ",")))
  cat(sprintf("Bytes per string: %.2f\n", total_bytes / scenario$n_strings))
  cat(sprintf("Bytes per operation: %.2f\n", 
              total_bytes / (scenario$n_strings * scenario$n_patterns)))
  
  # Identify top allocators
  if (!is.null(data$top_stacks)) {
    cat("\nTop allocation sites:\n")
    for (i in seq_len(min(5, length(data$top_stacks)))) {
      stack <- data$top_stacks[[i]]
      cat(sprintf("  %d. %s: %s bytes\n",
                  i, stack$frame, format(stack$bytes, big.mark = ",")))
    }
  }
}

#' Profile with heaptrack (external tool)
profile_with_heaptrack <- function(scenario_name) {
  cat("Note: heaptrack requires external installation\n")
  cat("On Ubuntu/Debian: sudo apt install heaptrack\n")
  cat("On macOS: brew install heaptrack (if available)\n\n")
  
  # Check if heaptrack is available
  if (system("which heaptrack", ignore.stdout = TRUE, ignore.stderr = TRUE) != 0) {
    stop("heaptrack not found. Install it or use --dhat option.")
  }
  
  # Build
  cmd <- "cd src/rust && cargo build --release 2>&1"
  system(cmd)
  
  # Run with heaptrack
  output_dir <- file.path("benchmark/results", format(Sys.time(), "%Y%m%d_%H%M%S"))
  dir.create(output_dir, recursive = TRUE)
  
  cat(sprintf("Running heaptrack, output to %s\n", output_dir))
  
  # This would need a standalone Rust binary to profile effectively
  # For now, suggest manual profiling
  cat("\nFor detailed heaptrack profiling, consider:\n")
  cat("1. Create a standalone Rust binary for the scenario\n")
  cat("2. Run: heaptrack ./target/release/benchmark_binary\n")
  cat("3. Analyze with: heaptrack_gui heaptrack.*.gz\n")
}

#' Main execution
main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args) < 1) {
    cat("Usage: Rscript benchmark/profiling/profile-memory.R <scenario> [--dhat|--heaptrack]\n")
    cat("\nAvailable scenarios:\n")
    walk(SCENARIOS_FULL, ~cat(sprintf("  - %s\n", .x$name)))
    return(NULL)
  }
  
  scenario_name <- args[1]
  method <- if ("--heaptrack" %in% args) "heaptrack" else "dhat"
  
  if (method == "dhat") {
    profile_with_dhat(scenario_name)
  } else {
    profile_with_heaptrack(scenario_name)
  }
}

if (!interactive()) {
  main()
}
