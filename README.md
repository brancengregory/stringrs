# stringrs: Fast Regex Matching with Rust

High-performance regex matching for R using Rust via extendr. Compares multiple implementations including base R, parallel R (mirai/furrr), and Rust (regex and fancy-regex engines).

## Features

- **10-80x faster than stringr**: High-performance Rust implementation
- **Auto-optimization**: Engine selection, parallel strategy, and memory chunking happen automatically
- **Simple API**: Single `string_detect()` function with smart defaults
- **Fixed matching mode**: Fast literal string matching with `fixed = TRUE`
- **Ecosystem compatible**: Drop-in replacement for `stringr::str_detect()`

## Installation

```r
# Install Rust first: https://rustup.rs/
# Then install the package
devtools::install()
```

## Quick Start

```r
library(stringrs)

# Simple detection with smart defaults
strings <- c("apple pie", "banana bread", "cherry tart")
pattern <- "a[aeiou]"
string_detect(strings, pattern)

# Multiple patterns - returns wide tibble
patterns <- c("a[aeiou]", "[bt]read", "^ch")
result <- string_detect(strings, patterns)
# Returns: tibble with columns: string, a.aeiou., X.bt.read., X.ch

# Fixed (literal) matching - bypasses regex for speed
strings <- c("Hello world", "hello there", "HELLO")
string_detect(strings, "hello", fixed = TRUE)  # case-sensitive literal match
```

## API Reference

### string_detect()

```r
string_detect(
    strings,           # Character vector of strings to search
    pattern,           # Character vector of patterns (single or multiple)
    fixed = FALSE      # Use literal matching (bypasses regex)
)
```

**Parameters:**

- `strings`: Character vector of strings to search
- `pattern`: Character vector of patterns (single pattern or multiple patterns)
- `fixed`: Logical. If `TRUE`, use literal string matching (bypasses regex engine for speed). Default is `FALSE` (regex matching).

**Returns:**

- Single pattern: Logical vector (same length as `strings`)
- Multiple patterns: Wide tibble with columns for each pattern plus `string` column

**Auto-optimization (all transparent):**

- **Engine selection**: Auto-detects fancy regex features (backrefs, lookaheads) and chooses appropriate engine
- **Parallel strategy**: Auto-selects optimal parallelization based on data dimensions
- **Memory chunking**: Auto-enabled when result would exceed memory threshold

## Benchmarking

### Run Benchmarks

```r
# Quick test (reduced test cases, single iteration)
Rscript benchmark/run.R --quick

# Full benchmark suite (19 scenarios)
Rscript benchmark/run.R

# Custom workers and iterations
Rscript benchmark/run.R --workers 8 --iterations 5
```

### Compare to Historical Baseline

```r
# Include comparison to previous runs
Rscript benchmark/run.R --compare
```

### Test Configurations

The benchmark suite tests various scales:

| Test | Strings | Patterns | Description |
|------|---------|----------|-------------|
| 1Kx1 | 1,000 | 1 | Small single pattern |
| 10Kx1 | 10,000 | 1 | Medium single pattern |
| 100Kx1 | 100,000 | 1 | Large single pattern |
| 1Kx10 | 1,000 | 10 | Small multi-pattern |
| 10Kx10 | 10,000 | 10 | Medium multi-pattern |
| 100Kx10 | 100,000 | 10 | Large multi-pattern |
| 10Kx50 | 10,000 | 50 | Many patterns |
| 10Kx100 | 10,000 | 100 | Very many patterns |

### Implementations Tested

1. **R Base**: `grepl()`, `stringr::str_detect()`, `stringi::stri_detect_regex()`
2. **R Parallel (mirai)**: Parallel over strings or patterns
3. **R Parallel (furrr)**: `future_map()` over strings or patterns
4. **Rust (regex)**: Standard regex crate, single and parallel
5. **Rust (fancy-regex)**: Supports backrefs/lookaheads, single and parallel

## Performance Tips

### When to Use Fixed Matching

Set `fixed = TRUE` when:
- You're searching for literal strings (not regex patterns)
- You need exact substring matching
- You want maximum speed (bypasses regex engine entirely)

Example: `string_detect(text, "ERROR", fixed = TRUE)`

### When Auto-Optimization Helps

The package automatically optimizes based on your data:
- **Engine selection**: Backreferences `(.)\1` or lookaheads `(?=...)` auto-detect fancy-regex need
- **Parallel strategy**: Large datasets (>10K strings or >50 patterns) auto-enable parallel processing
- **Memory chunking**: Results >100MB automatically use chunked processing

### Power User Configuration

Advanced users can tune via R options:

```r
# Adjust chunking threshold (in bytes)
options(stringrs.chunk_threshold = 50 * 1024 * 1024)  # 50MB

# Reset to default
options(stringrs.chunk_threshold = NULL)
```

## Architecture

### Auto-Optimization Strategy

All performance decisions happen transparently:

**Engine Selection**
- Byte-level pattern analysis detects fancy regex features
- Standard regex: Fast DFA engine for 90% of patterns
- Fancy-regex: Backtracking engine for backrefs/lookaheads only when needed

**Parallel Strategy**
- Data-driven heuristics based on string count × pattern count
- String-parallel: Best for large string vectors with few patterns
- Pattern-parallel: Best for many patterns with smaller string vectors
- Sequential: Small datasets where overhead isn't worth it

**Memory Chunking**
- Result size estimation triggers automatic chunking
- Default threshold: 100MB (configurable via options)
- Balances overhead vs cache locality for optimal throughput

## Development

### Project Structure

```
stringrs/
├── R/
│   ├── extendr-wrappers.R    # Auto-generated Rust bindings
│   └── string_detect.R        # Main R API (single function)
├── src/
│   └── rust/
│       ├── src/lib.rs          # Rust implementation
│       └── Cargo.toml          # Rust dependencies
├── benchmark/
│   ├── run.R                   # Benchmark runner
│   ├── engines.R               # Engine implementations
│   ├── scenarios.R             # Test scenarios
│   ├── metrics.R               # Performance metrics
│   ├── visualize.R             # Result visualization
│   └── compare.R               # Baseline comparison
└── tests/
    └── testthat/
        └── test-detect.R       # Unit tests
```

### Adding New Implementations

To add a new regex implementation:

1. Add function to `src/rust/src/lib.rs`
2. Export via `extendr_module!`
3. Add R wrapper in `R/extendr-wrappers.R`
4. Add benchmark function in `inst/benchmark_functions.R`
5. Include in benchmark runner

## License

MIT + file LICENSE
