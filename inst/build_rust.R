#!/usr/bin/env Rscript
# Build script for stringrs Rust code

# Clean old build artifacts
cat("Cleaning old build artifacts...\n")
unlink("src/rust/target/release/build/libR-sys-*", recursive = TRUE)

# Build the package
cat("Building package...\n")
if (requireNamespace("rextendr", quietly = TRUE)) {
  rextendr::document()
} else {
  # Fallback to devtools
  if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::install(quick = TRUE, build = FALSE)
  } else {
    stop("Please install rextendr or devtools")
  }
}

cat("Build complete!\n")
