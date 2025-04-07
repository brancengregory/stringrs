library(bench)
library(ggbeeswarm)

devtools::load_all()

# Define parameters for the benchmark
string_lengths <- c(20, 100, 1000)    # Different lengths of strings
num_strings <- c(10, 1000, 100000) # Different numbers of strings
patterns <- c("abc", "[A-Za-z0-9]{3}", "(?i)\\b\\w+\\b") # Various pattern complexities

# Function to generate random strings
generate_strings <- function(n, length) {
  replicate(n, paste0(sample(letters, length, replace = TRUE), collapse = ""))
}

# Running the benchmark using bench::press
benchmark_results <- press(
  string_length = string_lengths,
  num_strings = num_strings,
  pattern = patterns,
  {
    # Generate test data
    test_strings <- generate_strings(num_strings, string_length)

    bench::mark(
      min_iterations = 10,  # Minimum number of iterations per benchmark
      stringi = stringi::stri_detect_regex(test_strings, pattern),
      stringr = stringr::str_detect(test_strings, pattern),
      stringrs = stringrs::string_detect(test_strings, pattern),
    )
  }
)

plot(benchmark_results)
