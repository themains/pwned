withr::with_dir(testthat::test_path("..", ".."), {
  source("scripts/prepare_data.R", local = TRUE)
  source("scripts/analyze.R", local = TRUE)
})
