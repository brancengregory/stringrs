#!/usr/bin/env Rscript
# Memory Comparison: Chunked vs Normal Mode
# Demonstrates memory efficiency of string_detect_chunked

suppressPackageStartupMessages({
  library(dplyr)
  library(bench)
})

devtools::load_all()

cat("\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("        MEMORY EFFICIENCY: CHUNKED vs NORMAL MODE\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

# Test with medium dataset
n_strings <- 50000
n_patterns <- 10

strings <- replicate(n_strings, paste(sample(letters, 100, replace = TRUE), collapse = ""))
patterns <- c("a[aeiou]", "[bcdfghjklmnpqrstvwxyz]{3}", "^a", "z$", 
              "e.*e", "[0-9]", "test", "[aeiou]{3}", "xyz", "^t")

input_size_mb <- sum(nchar(strings) + nchar(patterns)) * 2 / 1024 / 1024
result_size_mb <- n_strings * n_patterns * 4 / 1024 / 1024

cat(sprintf("Dataset: %s strings × %s patterns\n", 
            format(n_strings, big.mark = ","),
            format(n_patterns, big.mark = ",")))
cat(sprintf("Input size: %.2f MB\n", input_size_mb))
cat(sprintf("Full result matrix: %.2f MB (%s × %s × 4 bytes)\n", 
            result_size_mb, n_strings, n_patterns))
cat("\n")

cat("--- Testing Normal Mode (all at once) ---\n")
gc(reset = TRUE)
bm_normal <- bench::mark(
  result_normal <- string_detect(strings, patterns, parallel = "sequential"),
  iterations = 1,
  check = FALSE
)
gc_after_normal <- gc()

cat(sprintf("Time: %.2f ms\n", as.numeric(bm_normal$median) * 1000))
cat(sprintf("Memory (bench): %.2f MB\n", as.numeric(bm_normal$mem_alloc) / 1024^2))
cat("\n")

cat("--- Testing Chunked Mode (chunk_size = 5000) ---\n")
gc(reset = TRUE)
bm_chunked <- bench::mark(
  result_chunked <- string_detect_chunked(strings, patterns, chunk_size = 5000),
  iterations = 1,
  check = FALSE
)
gc_after_chunked <- gc()

cat(sprintf("Time: %.2f ms\n", as.numeric(bm_chunked$median) * 1000))
cat(sprintf("Memory (bench): %.2f MB\n", as.numeric(bm_chunked$mem_alloc) / 1024^2))
cat("\n")

cat("--- Results Match? ---\n")
identical_results <- all(result_normal == result_chunked)
cat(sprintf("Results identical: %s\n", ifelse(identical_results, "YES ✓", "NO ✗")))
cat("\n")

cat("═══════════════════════════════════════════════════════════════\n")
cat("                    MEMORY ANALYSIS\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

peak_memory_normal <- result_size_mb + input_size_mb + 10  # + overhead
peak_memory_chunked <- (5000 * n_patterns * 4 / 1024 / 1024) + input_size_mb + 10

cat(sprintf("Normal mode peak memory: ~%.0f MB\n", peak_memory_normal))
cat(sprintf("Chunked mode peak memory: ~%.0f MB (chunk_size=5000)\n", peak_memory_chunked))
cat(sprintf("Memory reduction: %.1fx less\n", peak_memory_normal / peak_memory_chunked))
cat("\n")

cat("When to use chunked mode:\n")
cat("  • Large datasets (> 100K strings with many patterns)\n")
cat("  • Limited RAM available\n")
cat("  • Processing streaming/batch data\n")
cat("  • Avoiding swap/thrashing on memory-constrained systems\n")
cat("\n")

cat("Trade-offs:\n")
cat("  • Slightly slower due to chunking overhead\n")
cat("  • Sequential processing (no parallel mode in chunked)\n")
cat("  • Better cache locality\n")
cat("\n")

cat("═══════════════════════════════════════════════════════════════\n")
