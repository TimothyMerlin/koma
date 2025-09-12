test_that("check_dots_used issues a warning for unused args", {
  expect_warning(
    check_dots_used(extra_arg = 1),
    "extra_arg"
  )
})
