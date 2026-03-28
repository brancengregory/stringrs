test_that("empty inputs handled correctly", {
  # Empty strings vector
  empty_strings <- character(0)
  pattern <- "test"
  result <- string_detect(empty_strings, pattern)
  expect_length(result, 0)
  expect_type(result, "logical")
  
  # Single string, empty pattern should error or handle gracefully
  strings <- c("test")
  empty_pattern <- character(0)
  result <- string_detect(strings, empty_pattern)
  # Should return empty tibble for multi-pattern
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  
  # Both empty
  result <- string_detect(character(0), character(0))
  expect_length(result, 0)
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
  
  # Test character class with unicode
  pattern <- "\\p{L}+"  # Unicode letters
  result <- string_detect(unicode_strings, pattern, engine = "regex")
  expect_length(result, 6)
  
  # Test with multi-pattern
  patterns <- c("café", "日本", "🎉")
  result <- string_detect(unicode_strings, patterns)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 6)
})

test_that("NA/NULL handling matches base R behavior", {
  strings <- c("test", NA, "data", NA_character_)
  pattern <- "e"
  
  # Should handle NA like base R grepl
  base_result <- grepl(pattern, strings)
  stringrs_result <- string_detect(strings, pattern)
  
  expect_length(stringrs_result, 4)
  expect_equal(stringrs_result[!is.na(strings)], base_result[!is.na(strings)])
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
  result <- string_detect(strings, r"(hello\.world)", engine = "regex")
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

test_that("parallel strategies produce identical results", {
  set.seed(42)
  strings <- replicate(500, paste(sample(letters, 20, replace = TRUE), collapse = ""))
  patterns <- c("a[aeiou]", "[bcdfghjklmnpqrstvwxyz]{3}", "^a", "z$")
  
  # Run with different strategies
  result_seq <- string_detect(strings, patterns, parallel = "sequential")
  result_string <- string_detect(strings, patterns, parallel = "string_parallel")
  result_pattern <- string_detect(strings, patterns, parallel = "pattern_parallel")
  result_auto <- string_detect(strings, patterns, parallel = "auto")
  
  # All should produce identical results
  expect_equal(result_seq, result_string)
  expect_equal(result_seq, result_pattern)
  expect_equal(result_seq, result_auto)
})

test_that("chunk size parameter works", {
  strings <- replicate(1000, paste(sample(letters, 50, replace = TRUE), collapse = ""))
  patterns <- c("test", "[aeiou]+")
  
  # Different chunk sizes
  result_auto <- string_detect(strings, patterns, chunk_size = NULL)
  result_1000 <- string_detect(strings, patterns, chunk_size = 1000)
  result_5000 <- string_detect(strings, patterns, chunk_size = 5000)
  
  # Results should be identical regardless of chunk size
  expect_equal(result_auto, result_1000)
  expect_equal(result_auto, result_5000)
})

test_that("engine selection works correctly", {
  strings <- c("otto", "apple", "bookkeeper")
  
  # Auto should detect fancy patterns
  backref_pattern <- r"((.)\1)"
  result_auto <- string_detect(strings, backref_pattern, engine = "auto")
  result_fancy <- string_detect(strings, backref_pattern, engine = "fancy_regex")
  expect_equal(result_auto, result_fancy)
  
  # Standard pattern should work with both
  standard_pattern <- "[aeiou]{2}"
  result_auto <- string_detect(strings, standard_pattern, engine = "auto")
  result_regex <- string_detect(strings, standard_pattern, engine = "regex")
  expect_equal(result_auto, result_regex)
  
  # Lookahead pattern
  lookahead_pattern <- r"((?=.*oo))"
  result_auto <- string_detect(strings, lookahead_pattern, engine = "auto")
  result_fancy <- string_detect(strings, lookahead_pattern, engine = "fancy_regex")
  expect_equal(result_auto, result_fancy)
})

test_that("output formats work correctly", {
  strings <- c("apple pie", "banana bread", "cherry tart")
  patterns <- c("a[aeiou]", "[bt]read")
  
  # Wide format (default)
  result_wide <- string_detect(strings, patterns, output = "wide")
  expect_s3_class(result_wide, "tbl_df")
  expect_equal(nrow(result_wide), 3)
  expect_equal(ncol(result_wide), 3)  # string + 2 patterns
  expect_true("string" %in% names(result_wide))
  
  # Long format
  result_long <- string_detect(strings, patterns, output = "long")
  expect_s3_class(result_long, "tbl_df")
  expect_true(all(c("string_id", "pattern_id", "string", "pattern") %in% names(result_long)))
  
  # Long format should only contain matches
  expect_true(all(result_long$string_id >= 1 & result_long$string_id <= length(strings)))
  expect_true(all(result_long$pattern_id >= 1 & result_long$pattern_id <= length(patterns)))
})

test_that("single pattern vs multi-pattern consistency", {
  strings <- c("apple", "banana", "cherry")
  pattern <- "a[aeiou]"
  
  # Single pattern
  result_single <- string_detect(strings, pattern)
  expect_type(result_single, "logical")
  expect_length(result_single, 3)
  
  # Multi-pattern with single element
  result_multi <- string_detect(strings, c(pattern))
  expect_s3_class(result_multi, "tbl_df")
  expect_equal(nrow(result_multi), 3)
  
  # Results should be consistent
  expect_equal(result_single, result_multi[[2]])  # First pattern column
})

test_that("memory efficiency with large datasets", {
  # This test ensures we don't blow up memory
  strings <- replicate(10000, paste(sample(letters, 100, replace = TRUE), collapse = ""))
  patterns <- replicate(10, paste(sample(letters, 3), collapse = ""))
  
  # Should complete without memory issues
  result <- string_detect(strings, patterns, output = "wide")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 10000)
  expect_equal(ncol(result), 11)  # string + 10 patterns
  
  # Long format with sparse matches
  sparse_pattern <- "xyz123nonexistent"
  result_long <- string_detect(strings, sparse_pattern, output = "long")
  expect_s3_class(result_long, "tbl_df")
  # Should be empty or nearly empty
  expect_lte(nrow(result_long), 10)
})
