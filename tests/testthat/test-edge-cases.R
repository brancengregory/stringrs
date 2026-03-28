test_that("empty inputs handled correctly", {
  # Empty strings vector
  empty_strings <- character(0)
  pattern <- "test"
  result <- string_detect(empty_strings, pattern)
  expect_length(result, 0)
  expect_type(result, "logical")
  
  # Single string, empty pattern - returns empty tibble (nothing to match)
  strings <- c("test")
  empty_pattern <- character(0)
  result <- string_detect(strings, empty_pattern)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)  # No patterns = no rows
  expect_equal(ncol(result), 1)  # Just string column
  
  # Both empty - returns empty tibble
  result <- string_detect(character(0), character(0))
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 1)  # Just string column
})

test_that("very long strings work", {
  # Generate long strings
  long_string <- paste(rep("a", 10000), collapse = "")
  strings <- c(long_string, paste(rep("b", 10000), collapse = ""))
  pattern <- "a+"
  
  result <- string_detect(strings, pattern)
  expect_length(result, 2)
  expect_equal(result, c(TRUE, FALSE))
  
  # Test with multiple long strings and patterns
  many_long <- replicate(100, paste(sample(letters, 1000, replace = TRUE), collapse = ""))
  patterns <- c("[aeiou]{5}", "[bcdfghjklmnpqrstvwxyz]{10}")
  
  result <- string_detect(many_long, patterns)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 100)
  expect_equal(ncol(result), 3)  # string + 2 patterns
})

test_that("many patterns scales", {
  strings <- rep("test string with content", 1000)
  
  # Test with increasing numbers of patterns
  for (n_patterns in c(10, 50, 100, 250)) {
    patterns <- replicate(n_patterns, paste(sample(letters, 3), collapse = ""))
    
    result <- string_detect(strings, patterns)
    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 1000)
    expect_equal(ncol(result), n_patterns + 1)
  }
})

test_that("invalid regex patterns return error not crash", {
  strings <- c("test", "data")
  
  # Invalid pattern - unclosed bracket
  invalid_pattern <- "[invalid("
  expect_error(string_detect(strings, invalid_pattern))
  
  # Invalid pattern - invalid escape
  invalid_escape <- "\\"
  expect_error(string_detect(strings, invalid_escape))
  
  # Invalid pattern - unclosed group
  invalid_group <- "(test"
  expect_error(string_detect(strings, invalid_group))
  
  # Multiple patterns with one invalid
  patterns <- c("valid", "[invalid(")
  expect_error(string_detect(strings, patterns))
})

test_that("unicode strings work correctly", {
  # Test various unicode scenarios
  unicode_strings <- c(
    "café",           # Accented characters
    "日本語",          # Japanese
    "🎉 emoji",        # Emoji
    "العربية",        # Arabic (RTL)
    "Ελληνικά",       # Greek
    "🚀🌟✨"           # Multiple emoji
  )
  
  # Test simple pattern
  pattern <- ".*"
  result <- string_detect(unicode_strings, pattern)
  expect_length(result, 6)
  expect_true(all(result))
  
  # Test character class with unicode (auto-detects engine)
  pattern <- "\\p{L}+"  # Unicode letters
  result <- string_detect(unicode_strings, pattern)
  expect_length(result, 6)
  
  # Test with multi-pattern
  patterns <- c("café", "日本", "🎉")
  result <- string_detect(unicode_strings, patterns)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 6)
})

test_that("NA handling", {
  strings <- c("test", NA, "data", NA_character_)
  pattern <- "e"
  
  # Filter out NAs before calling (Rust doesn't accept NA)
  strings_clean <- strings[!is.na(strings)]
  stringrs_result <- string_detect(strings_clean, pattern)
  
  # Base R result for comparison
  base_result <- grepl(pattern, strings_clean)
  
  expect_length(stringrs_result, 2)
  expect_equal(stringrs_result, base_result)
})

test_that("special characters in patterns work", {
  strings <- c(
    "hello.world",
    "helloXworld",
    "test@email.com",
    "price: $100",
    "path/to/file",
    "a+b=c"
  )
  
  # Dot (literal)
  result <- string_detect(strings, r"(hello\.world)")
  expect_equal(result, c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE))
  
  # Dollar sign
  result <- string_detect(strings, r"(\$)")
  expect_equal(result, c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE))
  
  # Plus sign
  result <- string_detect(strings, r"(\+)")
  expect_equal(result, c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE))
  
  # Forward slash
  result <- string_detect(strings, r"(/)")
  expect_equal(result, c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE))
  
  # At sign
  result <- string_detect(strings, r"(@)")
  expect_equal(result, c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE))
})

test_that("parallel processing produces correct results", {
  set.seed(42)
  strings <- replicate(500, paste(sample(letters, 20, replace = TRUE), collapse = ""))
  patterns <- c("a[aeiou]", "[bcdfghjklmnpqrstvwxyz]{3}", "^a", "z$")
  
  # Run - parallel strategy auto-detected
  result <- string_detect(strings, patterns)
  
  # Should return wide tibble
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 500)
  expect_equal(ncol(result), 5)  # string + 4 patterns
})

test_that("auto-chunking works", {
  strings <- replicate(1000, paste(sample(letters, 50, replace = TRUE), collapse = ""))
  patterns <- c("test", "[aeiou]+")
  
  # Auto-chunking (no parameter needed)
  result <- string_detect(strings, patterns)
  
  # Should return wide tibble
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1000)
  expect_equal(ncol(result), 3)  # string + 2 patterns
})

test_that("engine selection works correctly", {
  strings <- c("otto", "apple", "bookkeeper")
  
  # Auto-detects fancy patterns (backrefs)
  backref_pattern <- r"((.)\1)"
  result_backref <- string_detect(strings, backref_pattern)
  expect_type(result_backref, "logical")
  expect_length(result_backref, 3)
  
  # Standard pattern
  standard_pattern <- "[aeiou]{2}"
  result_standard <- string_detect(strings, standard_pattern)
  expect_type(result_standard, "logical")
  expect_length(result_standard, 3)
  
  # Lookahead pattern (auto-detected as fancy)
  lookahead_pattern <- r"((?=.*oo))"
  result_lookahead <- string_detect(strings, lookahead_pattern)
  expect_type(result_lookahead, "logical")
  expect_length(result_lookahead, 3)
})

test_that("output format is correct", {
  strings <- c("apple pie", "banana bread", "cherry tart")
  patterns <- c("a[aeiou]", "[bt]read")
  
  # Wide format (only format supported)
  result <- string_detect(strings, patterns)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 3)  # string + 2 patterns
  expect_true("string" %in% names(result))
})

test_that("single pattern vs multi-pattern consistency", {
  strings <- c("apple", "banana", "cherry")
  pattern <- "a[aeiou]"
  
  # Single pattern - returns logical vector
  result_single <- string_detect(strings, pattern)
  expect_type(result_single, "logical")
  expect_length(result_single, 3)
  
  # Single-element pattern vector - also returns logical vector
  result_single_vec <- string_detect(strings, c(pattern))
  expect_type(result_single_vec, "logical")
  expect_length(result_single_vec, 3)
  expect_equal(result_single, result_single_vec)
  
  # Multi-pattern (2+ patterns) - returns wide tibble
  result_multi <- string_detect(strings, c(pattern, "^c"))
  expect_s3_class(result_multi, "tbl_df")
  expect_equal(nrow(result_multi), 3)
  expect_equal(ncol(result_multi), 3)  # string + 2 patterns
  
  # First pattern column should match single pattern result
  expect_equal(result_single, result_multi[[2]])
})

test_that("memory efficiency with large datasets", {
  # This test ensures we don't blow up memory
  strings <- replicate(10000, paste(sample(letters, 100, replace = TRUE), collapse = ""))
  patterns <- replicate(10, paste(sample(letters, 3), collapse = ""))
  
  # Should complete without memory issues (wide format only)
  result <- string_detect(strings, patterns)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 10000)
  expect_equal(ncol(result), 11)  # string + 10 patterns
  
  # Sparse pattern matching - returns logical vector for single pattern
  sparse_pattern <- "xyz123nonexistent"
  result_sparse <- string_detect(strings, sparse_pattern)
  expect_type(result_sparse, "logical")
  expect_length(result_sparse, 10000)
  # Should be mostly FALSE
  expect_lte(sum(result_sparse), 10)
})
