test_that("plotli", {
  ## Case 1: no previous figure
  ts_growth <- ets(
    c(5, 5.4, -4.3, 3.8, -4.8, -3.4, 4.5, 5.1, -3.4, 2.5, 4.5, -6.7),
    start = 2020, frequency = 4,
    method = "percentage", series_type = "rate"
  )
  ts_level <- level(ts_growth)
  ts_growth_annual <- rate(
    tempdisagg::ta(ts_level, conversion = "sum", to = "annual")
  )

  variables <- c("consumption")

  df_long <- structure(list(
    dates = c(
      stats::time(ts_growth),
      stats::time(ts_growth_annual),
      stats::time(ts_level)
    ),
    sample_status = factor(
      c(
        rep("in_sample", length(ts_growth) - 4),
        rep("base_forecast", 4),
        rep("in_sample", length(ts_growth_annual) - 1),
        rep("base_forecast", 1),
        rep("in_sample", length(ts_level) - 4),
        rep("base_forecast", 4)
      ),
      levels = c("base_forecast", "in_sample", "conditional_forecast")
    ),
    frames = rep("2023-Q2", 27),
    variable = rep("consumption", 27),
    value = c(ts_growth, ts_growth_annual, ts_level),
    data_type = factor(
      c(
        rep("growth", length(ts_growth)),
        rep("growth_annual", length(ts_growth_annual)),
        rep("level", length(ts_level))
      ),
      levels = c("growth", "growth_annual", "level")
    )
  ), row.names = c(NA, -27L), class = c("tbl_df", "tbl", "data.frame"))
  # Only keep the variable(s) that we want to plot
  df_long <- subset(df_long, df_long$variable %in% variables)

  theme <- init_koma_theme(
    legend = list(bordercolor = "#8d1414", borderwidth = 2),
    title = list(text = "Overridden Title"),
    index = list(start = NULL, end = NULL),
    trace_name = NULL,
    xaxis = list(
      range = list(start = c(2021, 4)),
      tickfont = list(family = "Arial", size = 17)
    ),
    yaxis = list(
      y = list(title = list(text = "QoQ, in %")),
      y2 = list(title = list(text = "Level")),
      tickfont = list(
        color = c("rgba(197,134,197,0.9)"),
        family = "Arial",
        size = 17
      )
    ),
    color = list(marker = list(
      in_sample = "rgba(197,197,197,0.6)",
      base_forecast = "rgba(166,190,223,1)",
      conditional_forecast = "rgba(166,190,223,1)"
    ), bar_textfont = list(
      in_sample = "black",
      out_of_sample = "white"
    ))
  )
  fig <- NULL

  fig <- plotli(df_long, fig, theme)

  # Check if the returned object is a plotly object
  expect_true(inherits(fig, "plotly"))

  data <- plotly::plotly_build(fig)
  # Check if correct axis titles are set
  layout <- data$x$layout
  expect_equal(layout$yaxis$title$text, theme$yaxis$y$title$text)
  expect_equal(layout$yaxis2$title$text, theme$yaxis$y2$title$text)
  expect_equal(layout$title$text, "Overridden Title")
  # range adjustment works with extra padding
  expect_equal(layout$xaxis$range, c(2021.625, 2023.125))

  # Check in_sample bar data
  in_sample_bar_data <- data$x$data[[1]] # assuming first trace is in-sample
  expect_equal(
    unique(in_sample_bar_data$marker$color),
    theme$color$marker$in_sample
  )

  # Check base_forecast bar data
  base_forecast_data <- data$x$data[[2]]
  expect_equal(
    unique(base_forecast_data$marker$color),
    theme$color$marker$base_forecast
  )

  # Extract expected x and y values
  expected_x <- time(ts_growth)
  expected_y <- as.numeric(ts_growth)

  # Extracting the actual values from the in_sample_data object
  actual_x <- as.vector(in_sample_bar_data$x)
  actual_y <- as.vector(in_sample_bar_data$y)

  # Check if the x and y values match
  expect_equal(actual_x, expected_x[1:8], tolerance = 1e-8)
  expect_equal(actual_y, expected_y[1:8], tolerance = 1e-8)

  # Check xaxis font tickfont adjustment
  expect_equal(layout$xaxis$tickfont, list(family = "Arial", size = 17))
  # Check yaxis font tickfont adjustment
  expect_equal(layout$yaxis$tickfont, list(
    color = "rgba(197,134,197,0.9)", family = "Arial", size = 17
  ))

  # Check xaxis annotations tickfont adjustment
  # family and size are determined by x$axis$tickfont

  # in_sample
  expect_equal(
    layout$annotations[[1]]$font,
    list(
      color = c(in_sample = "rgba(197,197,197,0.6)"),
      family = "Arial",
      size = 17
    )
  )
  # oos
  expect_equal(
    layout$annotations[[2]]$font,
    list(
      color = c(base_forecast = "rgba(166,190,223,1)"),
      family = "Arial",
      size = 17
    )
  )

  ## Case 2: with previous figure
  ts_growth <- ets(
    c(
      2, 5.2, -3.3, 4.3, -4.9, -3.6, 4.5, -5.1, 3.8, 3.5, -2.5, 4.5, 3.5,
      5.5, 4.1, 5.6
    ),
    start = 2020, frequency = 4, method = "percentage", series_type = "rate"
  )
  ts_level <- level(ts_growth)
  ts_growth_annual <- rate(
    tempdisagg::ta(ts_level, conversion = "sum", to = "annual")
  )

  df_long <- structure(list(
    dates = c(
      stats::time(ts_growth),
      stats::time(ts_growth_annual),
      stats::time(ts_level)
    ),
    sample_status = factor(
      c(
        rep("in_sample", length(ts_growth) - 4),
        rep("base_forecast", 4),
        rep("in_sample", length(ts_growth_annual) - 1),
        rep("base_forecast", 1),
        rep("in_sample", length(ts_level) - 4),
        rep("base_forecast", 4)
      ),
      levels = c("base_forecast", "in_sample", "conditional_forecast")
    ),
    frames = rep("2023-Q3", 36),
    variable = rep("consumption", 36),
    value = c(ts_growth, ts_growth_annual, ts_level),
    data_type = factor(
      c(
        rep("growth", length(ts_growth)),
        rep("growth_annual", length(ts_growth_annual)),
        rep("level", length(ts_level))
      ),
      levels = c("growth", "growth_annual", "level")
    )
  ), row.names = c(NA, -36), class = c("tbl_df", "tbl", "data.frame"))

  # Only keep the variable(s) that we want to plot
  df_long <- subset(df_long, df_long$variable %in% variables)

  theme <- init_koma_theme(
    index = list(start = NULL, end = NULL),
    trace_name = NULL,
    color = list(marker = list(
      in_sample = "rgba(111,111,111,0.6)",
      base_forecast = "rgba(33,92,175,1)",
      conditional_forecast = "rgba(33,92,175,1)",
      bar_textfont = list(
        in_sample = "black",
        out_of_sample = "white"
      )
    ))
  )
  fig <- plotli(df_long, fig, theme)

  data <- plotly::plotly_build(fig)

  # Test Type Checks
  expect_true(inherits(fig, "plotly"))

  # Test that trace names are set using the first out of sample date as a name
  # if trace name has not been set in options.
  trace_names <- sapply(data$x$data, function(x) {
    x$name
  })

  # Traces 1 to 4 are named 2022-Q1 and 2 to 8 are named 2023-Q1
  expect_true(all(grepl(pattern = "2022-Q1", trace_names[1:4])))
  expect_true(all(grepl(pattern = "2023-Q1", trace_names[5:8])))
})
