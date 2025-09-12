test_that("koma_ts class operations", {
  # object of class koma_ts with no additional attributes
  v <- ets(data = 1:10, start = c(2019, 1), frequency = 4)
  print(v)

  w <- ets(
    data = 1:10, start = c(2019, 1), frequency = 4,
    series_type = "level", value_type = "real", method = "diff_log"
  )
  print(w)

  ts_obj <- stats::ts(1:10, start = c(2019, 1), frequency = 4)

  x <- as_ets(
    ts_obj,
    series_type = "level",
    value_type = "real",
    method = "diff_log"
  )

  expect_equal(w, x)
  expect_output(print(x), "level, real")
  expect_equal(format(x), format(ts_obj))

  w1 <- stats::window(x, start = c(2019, 4))
  expect_equal(stats::start(w1), c(2019, 4))

  w2 <- stats::window(x, start = 2018, extend = TRUE)
  expect_equal(stats::start(w2), c(2018, 1))

  w2 <- stats::window(rate(x), start = 2018, extend = TRUE)
  expect_equal(attr(w2, "tsp"), c(2018, 2021.25, 4))
  expect_equal(attr(w2, "anker"), c(1, 2019))
  expect_equal(level(w2), x)

  w3 <- na.omit(window(x, start = 2018, extend = TRUE))
  expect_equal(stats::start(w3), c(2019, 1))

  w4 <- tempdisagg::ta(x, conversion = "sum", to = "annual")
  expected_w4 <- ets(c(10, 26),
    start = 2019, frequency = 1,
    series_type = "level", value_type = "real", method = "diff_log"
  )
  expect_equal(w4, expected_w4)

  w5 <- stats::window(rate(x), start = 2020)
  expect_equal(attr(w5, "tsp"), c(2020, 2021.25, 4))
  expect_equal(attr(w5, "anker"), c(4, 2019.75))

  w6 <- lag(rate(x))
  expect_equal(attr(w6, "tsp"), c(2019, 2021, 4))
  expect_equal(attr(w6, "anker"), c(1, 2018.75))

  w7 <- lag(rate(x), k = -1)
  expect_equal(attr(w7, "tsp"), c(2019.5, 2021.5, 4))
  expect_equal(attr(w7, "anker"), c(1, 2019.25))

  w8 <- lag(rate(x), -1)
  expect_equal(w8, w7)

  expect_silent(log(x))
  expect_silent(diff(x))
  expect_silent(100 * x)
  expect_silent(x * 100)
  expect_silent(x[1:2])
  expect_silent(x[3:5])
  expect_silent(x[4:3])
  expect_silent(x / x)
  expect_silent(x * x)
  expect_silent(x + x)
  expect_silent(x - x)

  y_ts_obj <- stats::ts(1:12, start = c(2020, 1), frequency = 4)
  y <- as_ets(
    y_ts_obj,
    series_type = "level", value_type = "real", method = "diff_log"
  )

  expect_silent(x + y)
  expect_silent(ts_obj + y_ts_obj)

  r1 <- rate(
    as_ets(
      ts_obj,
      series_type = "level",
      value_type = "nominal",
      method = "percentage"
    )
  )
  expect_equal(attr(r1, "series_type"), "rate")

  r2 <- rate(
    as_ets(ts_obj,
      series_type = "level",
      value_type = "nominal",
      method = "diff_log"
    )
  )
  expect_equal(attr(r2, "series_type"), "rate")

  # rate preserved
  r3 <- rate(
    as_ets(
      ts_obj,
      series_type = "rate", value_type = "real", method = "percentage"
    )
  )
  expect_equal(attr(r3, "series_type"), "rate")

  expr <- expression(diff(x) / stats::lag(x, -1) * 100)
  ts_obj_expr <- as_ets(
    ts_obj,
    series_type = "level",
    value_type = "real",
    method = expr
  )

  expect_equal(attr(ts_obj_expr, "method"), expr)

  r4 <- rate(ts_obj_expr)
  expect_equal(attr(r4, "series_type"), "rate")

  # rate calculation with NAs on edge
  r5 <- rate(
    ets(
      data = c(NA, NA, NA, 1:10), start = c(2019, 1), frequency = 4,
      series_type = "level", value_type = "real", method = "percentage"
    )
  )


  # test stats::as.ts conversion
  ts_conv <- stats::as.ts(
    as_ets(
      ts_obj,
      series_type = "rate", value_type = "real", method = "percentage"
    )
  )
  expect_true(stats::is.ts(ts_conv))

  # arguments in ... must be used
  expect_error(stats::as.ts(r5, unused = TRUE))

  # test as.matrix conversion
  mat_conv <- as.matrix(
    as_ets(
      ts_obj,
      series_type = "rate", value_type = "real", method = "percentage"
    )
  )

  expect_true(is.matrix(mat_conv))
})

test_that("ets as ets", {
  # ts
  x <- ets(1:10, start = c(2019, 1), frequency = 4, series_type = "level")

  y <- as_ets(x, series_type = "rate", value_type = "real", method = "percentage")

  expected_attr <- list(
    tsp = c(2019, 2021.25, 4),
    class = c("koma_ts", "ts"),
    series_type = "rate",
    ets_attributes = c("series_type", "value_type", "method"),
    value_type = "real",
    method = "percentage"
  )
  expect_equal(attributes(y), expected_attr)

  # mts
  x <- ets(
    data = matrix(1:10, ncol = 2),
    start = c(2019, 1), frequency = 4,
    series_type = "level", value_type = "nominal", method = "percentage"
  )

  y <- as_ets(x, series_type = "rate", value_type = "real", method = "percentage")

  expected_attr <- list(
    dim = c(5L, 2L),
    tsp = c(2019, 2020, 4),
    class = c("koma_ts", "mts", "ts", "matrix", "array"),
    series_type = list("rate", "rate"),
    value_type = list("real", "real"),
    method = list("percentage", "percentage"),
    ets_attributes = c("series_type", "value_type", "method"),
    dimnames = list(NULL, c("Series 1", "Series 2"))
  )
  expect_equal(attributes(y), expected_attr)
})

test_that("ts rate and level", {
  # Create a multivariate time series in growth rate form
  ts_obj <- stats::ts(c(2, -1, 3, -2), frequency = 4, start = c(2019, 1))
  x <- as_ets(
    ts_obj,
    series_type = "rate", value_type = "real", method = "percentage"
  )

  y <- level(x)

  # arguments in ... must be used
  expect_error(rate(y, unused = TRUE))
  expect_error(level(x, unused = TRUE))

  # Manually compute the expected level form
  expected_y <- c(
    100,
    100 * 1.02,
    100 * 1.02 * 0.99,
    100 * 1.02 * 0.99 * 1.03,
    100 * 1.02 * 0.99 * 1.03 * 0.98
  )

  expect_true(inherits(y, "koma_ts"))
  expect_equal(as.vector(y), expected_y)

  ts_obj_2 <- stats::ts(
    c(100, 102, 101, 99, 104),
    start = c(2019, 1), frequency = 4
  )
  x_2 <- as_ets(
    ts_obj_2,
    series_type = "level", value_type = "real", method = "diff_log"
  )
  y_2 <- rate(x_2)
  expected_attr_y_2 <- list(
    tsp = c(2019.25, 2020, 4),
    class = c("koma_ts", "ts"),
    series_type = "rate",
    value_type = "real",
    method = "diff_log",
    ets_attributes = c("series_type", "value_type", "method", "anker"),
    anker = c(100, 2019)
  )
  expect_equal(attributes(y_2), expected_attr_y_2)
  # unchanged attributes when converting rate to rate
  expect_equal(attributes(rate(y_2)), expected_attr_y_2)

  expect_equal(level(y_2), x_2)

  ts_obj_3 <- 3000 * stats::ts(
    c(100, 102, 101, 99, 104),
    start = c(2019, 1), frequency = 4
  )
  x_3 <- as_ets(
    ts_obj_3,
    series_type = "level", value_type = "real", method = "diff_log"
  )
  y_3 <- rate(x_3)
  expect_equal(level(y_3), x_3)

  # ts already in level stays level
  x_4 <- as_ets(
    ts_obj_3,
    series_type = "level", value_type = "nominal", method = "percentage"
  )
  y_4 <- level(x_4)
  expect_equal(x_4, y_4)

  # ts level with 0 at end
  ts_obj_4 <- stats::ts(c(100, 101, 103, 102, 0, 0, 0),
    frequency = 4, start = c(2019, 1)
  )
  x_5 <- as_ets(
    ts_obj_4,
    series_type = "level", value_type = "real", method = "percentage"
  )

  y_5 <- rate(x_5)

  expect_equal(x_5, level(y_5))

  # method is expression
  x_6 <- as_ets(
    ts_obj,
    series_type = "rate", method = expression(x)
  )

  y_6 <- level(x_6)
  expect_equal(c(y_6), c(x_6))
  expect_equal(stats::time(y_6), stats::time(x_6))
  # because there is no conversion, there can't be an anker
  # providing anker is users responisbility when working with a custom method
  expect_true(is.null(attr(x_6, "anker")))
  expect_true(is.null(attr(y_6, "anker")))


  y_7 <- ets(c(
    100, 99, 101, 102, 103, 101, 104, 106, 105, 104, 107, 108, 109, 108
  ), start = c(2019, 1), frequency = 4, method = "diff_log", series_type = "level")

  rate(y_7)

  y_7_a <- tempdisagg::ta(y_7, conversion = "sum", to = "annual")
  rate(y_7_a)
})

test_that("pass expression as method for level or rate calculations", {
  ts_obj <- stats::ts(c(2, -1, 3, -2), frequency = 4, start = c(2019, 1))
  # example as to expression that can be provided
  # needs to handle cases when series type rate or level
  log_transform <- function(x) {
    type <- attr(x, "series_type")
    epsilon <- 1e-9 # Small number to replace zeros

    if (type == "level") {
      x <- ifelse(x == 0, epsilon, x)
      out <- diff(log(x)) * 100
      new_type <- "rate"
    } else if (type == "rate") {
      out <- exp(cumsum(x / 100)) * 100

      out <- ets(
        c(100, out),
        end = stats::end(out),
        frequency = stats::frequency(out)
      )
      new_type <- "level"
    }

    attributes(out) <- c(attributes(out), get_custom_attributes(x))
    attr(out, "series_type") <- new_type

    out
  }

  # make function available globally for test env
  assign("log_transform", log_transform, envir = .GlobalEnv)

  x_7 <- as_ets(
    ts_obj,
    series_type = "rate", method = expression(log_transform(x))
  )
  y_7 <- level(x_7)
  expect_equal(y_7, log_transform(x_7))
  expect_equal(c(rate(y_7)), c(x_7))
})

test_that("mets", {
  # create mets
  w <- ets(
    matrix(c(100, 102, 104, 99, 98, 101, 104, 105, 99, 100), ncol = 2),
    start = c(2020, 4), frequency = 4,
    series_type = "level", value_type = "real", method = "percentage",
    bla = "x"
  )
  print(w)

  # create mets with as_ets
  mts_obj <- stats::ts(
    matrix(c(100, 102, 104, 99, 98, 101, 104, 105, 99, 100), ncol = 2),
    start = c(2020, 4), frequency = 4
  )
  x <- as_ets(
    mts_obj,
    series_type = "level", value_type = "real", method = "percentage",
    bla = "x"
  )

  expect_equal(w, x)

  # operations
  print(x)
  format(x)
  window(x, start = c(2021, 3))
  tempdisagg::ta(x, conversion = "sum", to = "annual")
  expect_silent(x + x)
  expect_silent(x - x)
  expect_silent(x * x)
  expect_silent(x / x)

  # convert mets to list
  x_ls <- as_list(x)
  print(x_ls)

  expected_x_attr <-
    list(
      tsp = c(2020.75, 2021.75, 4),
      class = c("koma_ts", "ts"),
      series_type = "level", value_type = "real", method = "percentage",
      bla = "x",
      ets_attributes = c("series_type", "value_type", "method", "bla")
    )
  expect_identical(attributes(x_ls[[1]]), expected_x_attr)

  x_mets <- as_mets(x_ls)
  expect_identical(x, x_mets)

  # arguments in ... must be used
  expect_error(as_mets(x_ls, unused = TRUE))

  x_2 <- rate(x)
  x_3 <- level(x_2)
  expect_equal(x, x_3)

  x_els <- append(
    x_ls,
    list(`Series 3` = ets(c(100, 102, 99, 98, 100),
      start = c(2021, 1), frequency = 4,
      series_type = "level", method = expression(x)
    ))
  )

  # value_type missing in Series 3
  expect_error(as_mets(x_els), "Provide the same attributes")

  # when using expressions user needs to provide / ensure correct attributes
  x_els <- append(
    x_ls,
    list(`Series 3` = ets(c(100, 102, 99, 98, 100),
      start = c(2021, 1), frequency = 4,
      series_type = "level", value_type = "real",
      method = expression({
        out <- x
        attr(out, "anker") <- NA
        attr(out, "ets_attributes") <- c(attr(out, "ets_attributes"), "anker")
        out
      }),
      bla = 1
    ))
  )

  x_emets <- as_mets(x_els)
  expect_equal(as_list(rate(x_emets)), rate(x_els))

  # preserve types of series attributes
  expect_type(attr(x_emets, "bla")[[1]], "character")
  expect_type(attr(x_emets, "bla")[[3]], "double")

  # to list of ets
  y <- as_list(x)
  # rate works on list of ets
  y_2 <- rate(y)
  y_3 <- level(y_2)
  expect_equal(y, y_3)

  expect_error(ets(
    mts_obj,
    series_type = c("level", "rate", "level"),
    value_type = c("nominal", "nominal"),
    method = "percentage"
  ), "level")

  #
  x_emets
  x_annual <- tempdisagg::ta(x_emets, conversion = "sum", to = "annual")
  expect_s3_class(x_annual, "koma_ts")
})

test_that("as_list.mts throws error", {
  x <- ets(
    matrix(c(100, 102, 104, 99, 98, 101, 104, 105, NA, 100), ncol = 2),
    start = c(2020, 4), frequency = 4,
    series_type = "level", value_type = "real", method = "percentage",
    bla = "x"
  )

  expect_error(as_list(x), "Failed to process column Series 2")

  x <- stats::ts(
    matrix(c(100, 102, 104, 99, 98, 101, 104, 105), ncol = 2),
    start = c(2020, 4), frequency = 4
  )

  # type mismatch
  expect_error(as_list(x), "must be of type")
})

test_that("rate.list throws error", {
  x <- list(
    ts1 = ets(c(100, 102, 104, 99, 98),
      start = c(2020, 4), frequency = 4,
      series_type = "level", value_type = "real", method = "percentage"
    ),
    ts2 = ets(c(100, 102, 104, 99, 98, 101, 104, 105, NA, 100),
      start = c(2020, 4), frequency = 4,
      series_type = "level", value_type = "real", method = "percentage"
    )
  )

  expect_error(rate(x), "Rate calculation failed for element: ts2")
})

test_that("level.list throws error", {
  x <- list(
    ts1 = ets(c(0, 2, 4, -5, -1),
      start = c(2020, 4), frequency = 4,
      series_type = "rate", value_type = "real", method = "percentage"
    ),
    ts2 = ets(c(4, 1, NA, -5),
      start = c(2020, 4), frequency = 4,
      series_type = "rate", value_type = "real", method = "percentage"
    )
  )

  expect_error(level(x), "Level calculation failed for element")
})

test_that("concatenate koma time series", {
  ## concat.ts
  x <- ets(
    data = 1:10, start = c(2019, 1), frequency = 4, series_type = "level"
  )
  y <- ets(
    data = 11:20, start = c(2021, 3), frequency = 4, series_type = "level"
  )
  result <- concat(x, y)
  expected_result <- ets(
    data = 1:20, start = c(2019, 1), frequency = 4,
    series_type = "level"
  )
  expect_equal(result, expected_result)

  # arguments in ... must be used
  expect_error(concat(x, y, unused = TRUE))

  # error when not of class koma_ts
  expect_error(concat(as.ts(x), y))
  expect_error(concat(x, as.ts(y)))

  # Case when ts do not align
  x <- ets(
    data = 1:9, start = c(2019, 1), frequency = 4, series_type = "level"
  )
  y <- ets(
    data = 11:20, start = c(2021, 3), frequency = 4, series_type = "level"
  )
  result <- suppressWarnings(concat(x, y))
  expected_result <- ets(
    data = c(1:9, NA, 11:20), start = c(2019, 1),
    frequency = 4, series_type = "level"
  )
  expect_equal(result, expected_result)
  expect_warning(concat(x, y), "Time series did not align")

  # start cannot be after end
  expect_error(concat(y, x))

  # Case x end and y start match but are not equal
  x <- ets(
    data = 1:11, start = c(2019, 1), frequency = 4, series_type = "level"
  )
  y <- ets(
    data = 1:10, start = c(2021, 3), frequency = 4, series_type = "level"
  )
  expect_error(concat(x, y))
  # Case x end and y start match and are equal
  y <- ets(
    data = 11:20, start = c(2021, 3), frequency = 4, series_type = "level"
  )
  result <- concat(x, y)
  expected_result <- ets(
    data = c(1:10, 11:20), start = c(2019, 1),
    frequency = 4, series_type = "level"
  )
  expect_equal(result, expected_result)

  # Case x end is after y start
  x <- ets(
    data = 1:12, start = c(2019, 1), frequency = 4, series_type = "level"
  )
  y <- ets(
    data = 1:10, start = c(2021, 3), frequency = 4, series_type = "level"
  )
  expect_error(concat(x, y))

  ## concat.list
  # Case x and y are lists of ets
  x <- list(
    x = ets(
      data = 1:10, start = c(2019, 1), frequency = 4, series_type = "level"
    ),
    y = ets(
      data = 1:10, start = c(2018, 1), frequency = 4, series_type = "level"
    )
  )
  y <- list(
    x = ets(
      data = 1:10, start = c(2021, 3), frequency = 4, series_type = "level"
    ),
    y = ets(
      data = 1:10, start = c(2020, 3), frequency = 4, series_type = "level"
    )
  )
  result <- concat(x, y)
  expected_result <- list(
    x = ets(c(1:10, 1:10),
      start = c(2019, 1), frequency = 4, series_type = "level"
    ),
    y = ets(c(1:10, 1:10),
      start = c(2018, 1), frequency = 4, series_type = "level"
    )
  )
  expect_equal(result, expected_result)

  # error when type mismatch
  expect_error(concat(list(x = x), list(x = as.ts(y))), "Not all elements in")

  # eror when missing names
  expect_error(
    concat(list(x = y$x, z = y$y, w = y$y), x),
    "elements are missing in `y`"
  )

  ## concat.mts
  # Case x and y are mets
  result <- concat(as_mets(x), as_mets(y))
  expect_equal(result, as_mets(expected_result))

  result <- concat(as_mets(x), y)
  expect_equal(result, as_mets(expected_result))

  # error on type mismatch
  expect_error(concat(as_mets(x), as.ts(y)))
})

test_that("rebase", {
  x <- ets(1:10, start = c(2000, 1), frequency = 4)
  start <- c(2001, 1)
  end <- c(2001, 4)

  result <- rebase(x, start, end)

  base_x <- mean(stats::window(x, start = start, end = end))
  expect_equal(result, x / base_x * 100)

  # Case rebase list
  x <- list(
    var1 = ets(1:10, start = c(2000, 1), frequency = 4),
    var2 = ets(11:20, start = c(2000, 1), frequency = 4)
  )

  start <- c(2001, 1)
  end <- c(2001, 4)
  # Test 1: Using specified date window
  result <- rebase(x, start = start, end = end)

  base_var1 <- mean(stats::window(x$var1, start = start, end = end))
  base_var2 <- mean(stats::window(x$var2, start = start, end = end))

  expect_equal(result$var1, x$var1 / base_var1 * 100)
  expect_equal(result$var2, x$var2 / base_var2 * 100)

  # Test 2: dates is NULL
  suppressWarnings(
    expect_error(rebase(x, NULL))
  )
  suppressWarnings(
    expect_error(rebase(x, start = 1))
  )
  expect_error(rebase(x, start = NA, end = 4))

  # Case rebase mts
  x <- ets(cbind(var1 = c(1:10), var2 = c(11:20)),
    start = c(2000, 1), frequency = 4
  )

  start <- c(2001, 1)
  end <- c(2001, 4)

  result <- rebase(x, start, end)

  base_var1 <- mean(stats::window(x[, "var1"], start = start, end = end))
  base_var2 <- mean(stats::window(x[, "var2"], start = start, end = end))

  expect_equal(result[, "var1"], x[, "var1"] / base_var1 * 100)
  expect_equal(result[, "var2"], x[, "var2"] / base_var2 * 100)

  # arguments in ... must be used
  expect_error(rebase(x, start = start, end = end, unused = TRUE))
})

test_that("rebase.list throws error when elements not of type koma_ts", {
  # Case rebase list
  x <- list(
    var1 = stats::ts(1:10, start = c(2000, 1), frequency = 4),
    var2 = ets(11:20, start = c(2000, 1), frequency = 4)
  )

  expect_error(
    rebase(x, start = c(2001, 1), end = c(2001, 4)),
    "Elements of list"
  )
})

test_that("get type", {
  mts_obj <- stats::ts(
    matrix(
      c(100, 102, 104, 99, 98, 1, 4, 5, 9, 10, 98, 97, 95, 101, 102),
      ncol = 3
    ),
    start = c(2020, 4), frequency = 4
  )
  x <- as_ets(
    mts_obj,
    series_type = c("level", "rate", "level"),
    value_type = c("real", "nominal", "nominal"),
    method = "percentage"
  )

  x_2 <- rate(x)
  x_3 <- level(x_2)

  expect_equal(names(type(x, type = "nominal")), c("Series 2", "Series 3"))
  expect_equal(
    names(type(x, type = "nominal", var = c("Series 2"))),
    c("Series 2")
  )

  y <- as_list(x)
  expect_equal(names(type(y, type = "nominal")), c("Series 2", "Series 3"))
  expect_equal(
    names(type(y, type = "nominal", var = c("Series 2"))),
    c("Series 2")
  )
  expect_equal(
    names(type(y, type = "nominal", var = c("Series 2", "Series 3"))),
    c("Series 2", "Series 3")
  )
  expect_equal(names(type(y, type = "rate")), "Series 2")

  expect_error(type(y, type = c("level", "nominal")))

  z <- type(y, type = "level")
  expect_equal(names(type(z, type = "nominal")), "Series 3")

  # arguments in ... must be used
  expect_error(type(x, type = "level", unused = TRUE))
})

test_that("rate and level throw error on missing type and method", {
  x <- ets(
    c(2, -1, 3, -2),
    frequency = 4, start = c(2019, 1),
    series_type = "level", value_type = "real"
  )
  expect_error(rate(x), "Method cannot be NULL")

  x <- ets(
    c(2, -1, 3, -2),
    frequency = 4, start = c(2019, 1),
    series_type = "rate", value_type = "real"
  )
  expect_error(level(x), "Method cannot be NULL")

  x <- ets(
    c(2, -1, 3, -2),
    frequency = 4, start = c(2019, 1),
    method = "percentage"
  )

  expect_error(rate(x), "Type cannot be NULL")
  expect_error(level(x), "Type cannot be NULL")
})

test_that("anker date has to match timeseries start", {
  y <- ets(
    c(100, 101, 99, 103),
    frequency = 4, start = c(2019, 1),
    series_type = "level", method = "percentage"
  )

  x <- rate(y)
  attr(x, "anker") <- c(100, 2018.75)

  expect_error(level(x), "anker date")
})

test_that("rate, uses correct anker despite NAs", {
  y <- ets(
    c(NA, NA, 100, 101, 99, 103),
    frequency = 4, start = c(2019, 1),
    series_type = "level", method = "percentage"
  )

  x <- rate(y)
  attr(x, "anker") <- c(100, 2019.5)

  expect_equal(level(x), window(y, start = c(2019.5)))
})

test_that("rate and level, with method transform none", {
  x <- ets(
    c(1, 2, 1, 2),
    frequency = 4, start = c(2019, 1),
    series_type = "rate", method = "none"
  )
  y <- rate(x)
  expected_y <- structure(
    c(1, 2, 1, 2),
    tsp = c(2019, 2019.75, 4),
    class = c("koma_ts", "ts"),
    series_type = "rate",
    method = "none",
    ets_attributes = c("series_type", "method", "anker"),
    anker = NA
  )

  expect_equal(y, expected_y)

  attr(x, "anker") <- c(1, 2018.75)
  attr(x, "ets_attributes") <- c(attr(x, "ets_attributes"), "anker")

  y <- rate(x)

  expect_equal(y, x)

  y <- level(x)
  expect_equal(attr(y, "series_type"), "level")
  expect_equal(c(y), c(1, 2, 1, 2))
})
