# Benchmark Results

This directory stores benchmark results for performance tracking and regression detection.

## Directory Structure

```
benchmark/results/
├── YYYYMMDD_HHMMSS/          # Timestamped result directory
│   ├── benchmark_results.rds  # Raw benchmark data
│   ├── summary.json            # Summary statistics
│   ├── baseline_comparison.json  # Comparison to baseline (if --compare)
│   ├── comparison_report.md    # Human-readable comparison
│   ├── 01_speedup_comparison.png
│   ├── 02_throughput_comparison.png
│   ├── 03_scaling_efficiency.png
│   ├── 04_memory_usage.png
│   └── 05_performance_heatmap.png
└── BASELINE                  # Marker file indicating baseline
```

## File Formats

### benchmark_results.rds
R serialized list containing:
- `results`: Nested list of all benchmark results by scenario
- `scenario`: Test configuration
- `timestamp`: When benchmark ran
- `elapsed`: Total time for scenario
- `best_engine`: Fastest engine for this scenario
- `speedup_vs_base`: Speedup factor vs R base

### summary.json
JSON summary with:
- Total scenarios tested
- Total execution time
- Per-scenario best engines and speedups

### baseline_comparison.json
JSON comparison when `--compare` flag used:
- Time ratios (current/baseline)
- Regression flags
- Change percentages

## Usage

### Set a baseline
```r
source("benchmark/compare.R")
set_baseline("benchmark/results/20240327_120000/")
```

### Compare to baseline
```bash
Rscript benchmark/run.R --compare
```

### View historical trends
```r
source("benchmark/visualize.R")
# Load multiple result sets and plot trends
```

## Git

- Result directories are **NOT** committed to git
- Only this README and .gitkeep are tracked
- Run benchmarks locally and compare before/after changes
