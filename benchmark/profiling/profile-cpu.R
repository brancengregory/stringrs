#!/usr/bin/env Rscript
# CPU profiling for stringrs
#
# This script generates flamegraphs to visualize hot code paths
# using pprof or cargo-flamegraph.
#
# Usage:
#   Rscript benchmark/profiling/profile-cpu.R <scenario>
#   Rscript benchmark/profiling/profile-cpu.R medium_multi

suppressPackageStartupMessages({
  library(dplyr)
})

source("benchmark/scenarios.R")
devtools::load_all()

#' Profile with pprof (integrated with criterion)
profile_with_pprof <- function(scenario_name) {
  cat("Running CPU profile with pprof...\n")
  
  # Build with profiling enabled
  cmd <- "cd src/rust && cargo build --profile=bench 2>&1"
  output <- system(cmd, intern = TRUE)
  
  if (any(grepl("error", output, ignore.case = TRUE))) {
    stop("Build failed: ", paste(output, collapse = "\n"))
  }
  
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
  
  # Run with pprof profiler attached
  output_file <- file.path("benchmark/results", 
                          sprintf("profile_%s_%s.pb", scenario_name, 
                                  format(Sys.time(), "%Y%m%d_%H%M%S")))
  
  cat(sprintf("Profiling %s scenario...\n", scenario_name))
  cat(sprintf("Output: %s\n", output_file))
  
  # Use pprof::Profiler in Rust code
  # This requires modifying the lib.rs to add profiling hooks
  # For now, provide instructions
  
  cat("\nTo enable profiling in the code, add to lib.rs:\n")
  cat("\n#[cfg(feature = \"pprof\")]\n")
  cat("use pprof;\n\n")
  cat("Then wrap the function to profile:\n")
  cat("let guard = pprof::Profiler::new(100).unwrap();\n")
  cat("// ... run code ...\n")
  cat("let report = guard.report().build().unwrap();\n")
  cat("\nFor now, use cargo-flamegraph (below) for easier profiling.\n")
}

#' Profile with cargo-flamegraph
profile_with_flamegraph <- function(scenario_name) {
  cat("Setting up flamegraph profiling...\n\n")
  
  # Check for cargo-flamegraph
  if (system("cargo flamegraph --version", ignore.stdout = TRUE, ignore.stderr = TRUE) != 0) {
    cat("cargo-flamegraph not installed. Install with:\n")
    cat("  cargo install flamegraph\n\n")
    cat("Note: On macOS, you may need sudo for DTrace:\n")
    cat("  sudo cargo flamegraph ...\n\n")
    
    # Continue with manual instructions
  }
  
  # Load scenario
  scenario <- SCENARIOS_FULL[[which(map_chr(SCENARIOS_FULL, "name") == scenario_name)]]
  if (is.null(scenario)) {
    stop("Unknown scenario: ", scenario_name)
  }
  
  # Create output directory
  output_dir <- file.path("benchmark/results", 
                         sprintf("flamegraph_%s_%s", scenario_name,
                                 format(Sys.time(), "%Y%m%d_%H%M%S")))
  dir.create(output_dir, recursive = TRUE)
  
  cat("=== Manual Flamegraph Instructions ===\n\n")
  cat(sprintf("Scenario: %s\n", scenario_name))
  cat(sprintf("  Strings: %s\n", format(scenario$n_strings, big.mark = ",")))
  cat(sprintf("  Patterns: %s\n", format(scenario$n_patterns, big.mark = ",")))
  cat("\n")
  
  cat("Step 1: Create a standalone benchmark binary\n")
  cat("  Create: src/rust/examples/profile_scenario.rs\n")
  cat("\n")
  
  cat("Step 2: Run with cargo-flamegraph\n")
  cat(sprintf("  cd src/rust && cargo flamegraph --example profile_scenario --output %s/flamegraph.svg\n",
              output_dir))
  cat("\n")
  
  cat("Step 3: View the flamegraph\n")
  cat(sprintf("  open %s/flamegraph.svg\n", output_dir))
  cat("\n")
  
  # Create example template
  create_profile_template(scenario, output_dir)
  
  cat("=== Template Created ===\n")
  cat(sprintf("See: %s/profile_scenario.rs\n", output_dir))
  cat("Copy this to src/rust/examples/ and customize\n")
}

#' Create a template Rust file for profiling
create_profile_template <- function(scenario, output_dir) {
  template <- sprintf('
// Example: src/rust/examples/profile_scenario.rs
// Profile the %s scenario for flamegraph generation

use stringrs::*;

fn main() {
    // Generate test data
    let strings: Vec<String> = (0..%d)
        .map(|i| format!("test_string_{}_with_some_content_here", i))
        .collect();
    
    let patterns = vec![
        %s
    ];
    
    // Run the detection
    let _result = r_string_detect_multi_regex_optimized(
        strings,
        patterns,
        "auto".to_string(),
        -1  // Auto chunk size
    ).unwrap();
    
    println!("Completed {} strings x {} patterns", %d, %d);
}
', 
    scenario$name,
    scenario$n_strings,
    paste(map_chr(scenario$n_patterns, ~sprintf('"[a-z]+".to_string()')), collapse = ",\n        "),
    scenario$n_strings,
    scenario$n_patterns
  )
  
  writeLines(template, file.path(output_dir, "profile_scenario.rs"))
}

#' Profile specific function with criterion profiling
profile_function_detailed <- function(function_name, scenario_name) {
  cat(sprintf("Profiling %s in scenario %s...\n", function_name, scenario_name))
  
  # Run specific benchmark with profiling
  cmd <- sprintf("cd src/rust && cargo bench --bench regex_ops %s -- --profile-time 10 2>&1",
                 function_name)
  
  cat("Running:\n", cmd, "\n")
  system(cmd)
  
  # Find generated flamegraph
  flamegraph_files <- list.files("src/rust/target/criterion", 
                                 pattern = "flamegraph.svg$",
                                 recursive = TRUE,
                                 full.names = TRUE)
  
  if (length(flamegraph_files) > 0) {
    newest <- sort(flamegraph_files, decreasing = TRUE)[1]
    cat(sprintf("\nFlamegraph generated: %s\n", newest))
    
    # Copy to results
    output_file <- file.path("benchmark/results",
                            sprintf("flamegraph_%s_%s.svg",
                                    function_name,
                                    format(Sys.time(), "%Y%m%d_%H%M%S")))
    file.copy(newest, output_file)
    cat(sprintf("Copied to: %s\n", output_file))
  }
}

#' Main execution
main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args) < 1) {
    cat("Usage: Rscript benchmark/profiling/profile-cpu.R <scenario> [options]\n")
    cat("\nOptions:\n")
    cat("  --flamegraph     Use cargo-flamegraph (default)\n")
    cat("  --pprof          Use pprof integration\n")
    cat("  --function=<fn>  Profile specific function\n")
    cat("\nAvailable scenarios:\n")
    walk(SCENARIOS_FULL, ~cat(sprintf("  - %s\n", .x$name)))
    return(NULL)
  }
  
  scenario_name <- args[1]
  use_flamegraph <- "--pprof" %!in% args
  function_name <- NULL
  
  # Check for --function argument
  func_arg <- args[grepl("^--function=", args)]
  if (length(func_arg) > 0) {
    function_name <- sub("^--function=", "", func_arg[1])
  }
  
  if (!is.null(function_name)) {
    profile_function_detailed(function_name, scenario_name)
  } else if (use_flamegraph) {
    profile_with_flamegraph(scenario_name)
  } else {
    profile_with_pprof(scenario_name)
  }
}

if (!interactive()) {
  main()
}
