test_that("get_x_ticks", {
  ts_growth_annual <- stats::ts(
    c(5, 5.4, 4.3, 3.8, 4.8, 3.4, 4.5, 5.1, 3.4, 2.5, 4.5, 6.7),
    start = 2020, frequency = 1
  )
  df_long <- structure(
    list(
      dates = stats::time(ts_growth_annual),
      sample_status = factor(
        c(
          rep("in_sample", length(ts_growth_annual) - 4),
          rep("base_forecast", 4)
        ),
        levels = c("base_forecast", "in_sample", "conditional_forecast")
      ),
      frames = rep("2023-Q2", length(ts_growth_annual)),
      variable = rep("consumption", length(ts_growth_annual)),
      value = ts_growth_annual,
      data_type = factor(rep("growth_annual", length(ts_growth_annual)),
        levels = c("growth", "growth_annual", "level")
      )
    ),
    row.names = c(NA, -length(ts_growth_annual)),
    class = c("tbl_df", "tbl", "data.frame")
  )

  marker_color <- list(
    in_sample = "grey",
    base_forecast = "blue"
  )
  df_long <- attach_color_code(df_long, marker_color, "sample_status")

  # Case 1: no previous figure supplied
  result <- get_x_ticks(df_long)

  expected_ticks <- list(
    ticktext = c(
      "2019", "2020<br><br><span style='color:grey;'><b>5%</b></span>",
      "2021<br><br><span style='color:grey;'><b>5.4%</b></span>",
      "2022<br><br><span style='color:grey;'><b>4.3%</b></span>",
      "2023<br><br><span style='color:grey;'><b>3.8%</b></span>",
      "2024<br><br><span style='color:grey;'><b>4.8%</b></span>",
      "2025<br><br><span style='color:grey;'><b>3.4%</b></span>",
      "2026<br><br><span style='color:grey;'><b>4.5%</b></span>",
      "2027<br><br><span style='color:grey;'><b>5.1%</b></span>",
      "2028<br><br><span style='color:blue;'><b>3.4%</b></span>",
      "2029<br><br><span style='color:blue;'><b>2.5%</b></span>",
      "2030<br><br><span style='color:blue;'><b>4.5%</b></span>",
      "2031<br><br><span style='color:blue;'><b>6.7%</b></span>"
    ),
    tickvals = 2019:2031
  )

  expect_identical(result$ticktext, expected_ticks$ticktext)
  expect_identical(result$tickvals, expected_ticks$tickvals)

  # Case 2: previous figure supplied
  prev_fig_ticks <- expected_ticks

  # populate a previous figure
  fig <- plotly::plot_ly()
  fig$x$layoutAttrs[[1]]$xaxis$ticktext <- prev_fig_ticks$ticktext
  fig$x$layoutAttrs[[1]]$xaxis$tickvals <- prev_fig_ticks$tickvals

  ts_growth_annual <- stats::ts(
    c(5.4, 5.2, 4.5, 2.8, 4.2, 3.2, 3.5, 4.2, 3.3, 3.2, 3.4, 4.6),
    start = 2021, frequency = 1
  )
  df_long <- structure(
    list(
      dates = stats::time(ts_growth_annual),
      sample_status = factor(
        c(
          rep("in_sample", length(ts_growth_annual) - 2),
          rep("base_forecast", 2)
        ),
        levels = c("base_forecast", "in_sample", "conditional_forecast")
      ),
      frames = rep("2023-Q3", length(ts_growth_annual)),
      variable = rep("consumption", length(ts_growth_annual)),
      value = ts_growth_annual,
      data_type = factor(rep("growth_annual", length(ts_growth_annual)),
        levels = c("growth", "growth_annual", "level")
      )
    ),
    row.names = c(NA, -length(ts_growth_annual)),
    class = c("tbl_df", "tbl", "data.frame")
  )

  marker_color <- list(
    in_sample = "red",
    base_forecast = "green"
  )

  df_long <- attach_color_code(df_long, marker_color, "sample_status")

  result <- get_x_ticks(df_long, fig)

  expected_ticks <- list(
    ticktext = c(
      # nolint start
      "2019", "2020<br><br><span style='color:grey;'><b>5%</b></span>",
      "2021<br><br><span style='color:grey;'><b>5.4%</b></span><br><span style='color:red;'><b>5.4%</b></span>",
      "2022<br><br><span style='color:grey;'><b>4.3%</b></span><br><span style='color:red;'><b>5.2%</b></span>",
      "2023<br><br><span style='color:grey;'><b>3.8%</b></span><br><span style='color:red;'><b>4.5%</b></span>",
      "2024<br><br><span style='color:grey;'><b>4.8%</b></span><br><span style='color:red;'><b>2.8%</b></span>",
      "2025<br><br><span style='color:grey;'><b>3.4%</b></span><br><span style='color:red;'><b>4.2%</b></span>",
      "2026<br><br><span style='color:grey;'><b>4.5%</b></span><br><span style='color:red;'><b>3.2%</b></span>",
      "2027<br><br><span style='color:grey;'><b>5.1%</b></span><br><span style='color:red;'><b>3.5%</b></span>",
      "2028<br><br><span style='color:blue;'><b>3.4%</b></span><br><span style='color:red;'><b>4.2%</b></span>",
      "2029<br><br><span style='color:blue;'><b>2.5%</b></span><br><span style='color:red;'><b>3.3%</b></span>",
      "2030<br><br><span style='color:blue;'><b>4.5%</b></span><br><span style='color:red;'><b>3.2%</b></span>",
      "2031<br><br><span style='color:blue;'><b>6.7%</b></span><br><span style='color:green;'><b>3.4%</b></span>",
      "2032<br><br><span style='color:green;'><b>4.6%</b></span>"
      # nolint end
    ),
    tickvals = 2019:2032
  )

  expect_identical(result$ticktext, expected_ticks$ticktext)
  expect_identical(result$tickvals, expected_ticks$tickvals)
})

test_that("get_y_ticks", {
  # Case 1: Negative and positive values
  x_axis <- c(1, 2, 3, 4, 5, 6)
  y_axis <- c(0, 2, -3, 1, 4, 3)
  y2_axis <- c(85.45, 95.12, 90.59, 110.02, 115.23, 110.98)

  extremas <- list(
    y = list(min = min(y_axis), max = max(y_axis)),
    y2 = list(min = min(y2_axis), max = max(y2_axis))
  )
  num_ticks <- 5
  center_around <- list(y = 0, y2 = 100)

  ticks <- get_y_ticks(extremas, num_ticks, center_around)
  expected_ticks <- list(
    y = list(dtick = 1.4, min = -5.2325, max = 6.096),
    y2 = list(dtick = 4, min = 85.05, max = 117.417142857143)
  )
  expect_equal(ticks, expected_ticks)

  fig <- plotly::plot_ly()
  fig <- plotly::add_trace(fig, x = x_axis, y = y_axis, type = "bar")
  fig <- plotly::add_trace(fig,
    x = x_axis, y = y2_axis, type = "scatter", mode = "lines", yaxis = "y2"
  )
  fig <- plotly::layout(fig,
    yaxis = list(
      title = "Growth",
      side = "left",
      range = c(ticks$y$min, ticks$y$max),
      dtick = ticks$y$dtick
    ),
    yaxis2 = list(
      title = "Level",
      side = "right",
      range = c(ticks$y2$min, ticks$y2$max),
      overlaying = "y",
      dtick = ticks$y2$dtick,
      zeroline = FALSE,
      showgrid = FALSE
    )
  )

  y_axis_dtick <- fig$x$layoutAttrs[[1]]$yaxis$dtick
  y2_axis_dtick <- fig$x$layoutAttrs[[1]]$yaxis2$dtick

  expect_equal(y_axis_dtick, expected_ticks$y$dtick)
  expect_equal(y2_axis_dtick, expected_ticks$y2$dtick)

  y_axis_range <- fig$x$layoutAttrs[[1]]$yaxis$range
  y2_axis_range <- fig$x$layoutAttrs[[1]]$yaxis2$range

  expect_equal(y_axis_range[1], expected_ticks$y$min)
  expect_equal(y_axis_range[2], expected_ticks$y$max)
  expect_equal(y2_axis_range[1], expected_ticks$y2$min)
  expect_equal(y2_axis_range[2], expected_ticks$y2$max)

  # Case 2: All positive
  x_axis <- c(1, 2, 3, 4, 5, 6)
  y_axis_2 <- c(232, 2206, 37, 1629, 190, 800)
  y2_axis_2 <- c(141.21, 365.24, 265.21, 204.34, 129, 320.31)

  extremas <- list(
    y = list(min = min(y_axis_2), max = max(y_axis_2)),
    y2 = list(min = min(y2_axis_2), max = max(y2_axis_2))
  )
  num_ticks <- 5
  center_around <- list(y = 0, y2 = 0)

  ticks <- get_y_ticks(extremas, num_ticks, center_around)
  expected_ticks <- list(
    y = list(dtick = 400, min = 0, max = 2474.93333333333),
    y2 = list(dtick = 60, min = 0, max = 371.24)
  )
  expect_equal(ticks, expected_ticks)
})

test_that("get_y_position_for_annotations with annotations", {
  fig <- plotly::plot_ly()
  fig <- plotly::add_trace(
    fig,
    x = ~ c(1, 2, 3), y = ~ c(3, 1, 2), type = "scatter", mode = "lines+markers"
  )
  fig <- plotly::layout(fig,
    annotations = list(
      list(x = 1, y = 3, text = "Point A", showarrow = TRUE),
      list(x = 2, y = 1, text = "Point B", showarrow = TRUE),
      list(x = 3, y = 2, text = "Point C", showarrow = TRUE)
    )
  )

  annotation_position <- get_y_position_for_annotations(fig)

  expect_identical(annotation_position, -0.1 + (-0.04 * 1))
})

test_that("get_y_position_for_annotations with multiple annotations", {
  fig <- plotly::plot_ly()
  fig <- plotly::add_trace(
    fig,
    x = ~ c(1, 2, 3), y = ~ c(3, 1, 2), type = "scatter", mode = "lines+markers"
  )
  fig <- plotly::add_annotations(fig,
    x = 1, y = 3, text = "Point A", showarrow = TRUE
  )
  fig <- plotly::add_annotations(fig,
    x = 2, y = 1, text = "Point B", showarrow = TRUE
  )

  annotation_position <- get_y_position_for_annotations(fig)

  expect_identical(annotation_position, -0.1 + (-0.04 * 2))
})

test_that("get_y_poistion_for_annotations no previous fig", {
  fig <- NULL

  annotation_position <- get_y_position_for_annotations(fig)

  expect_identical(annotation_position, -0.1)
})


test_that("get_y_position_for_annotations no annotations", {
  # no previous annotations but other layout attributes in figure
  fig <- plotly::plot_ly()
  fig <- plotly::add_trace(
    fig,
    x = ~ c(1, 2, 3), y = ~ c(3, 1, 2), type = "scatter", mode = "lines+markers"
  )

  fig <-
    plotly::layout(
      fig,
      title = list(text = "Overridden Title")
    )

  annotation_position <- get_y_position_for_annotations(fig)

  expect_identical(annotation_position, -0.1)
})

test_that("create_annotations", {
  x_ticks_sample <- data.frame(
    year = c(2020, 2021),
    value = c(2.5, 3.0),
    color_code = c("#FF0000", "#0000FF")
  )
  y_position_for_annotations <- 0.5

  annotations <- create_annotations(x_ticks_sample, y_position_for_annotations)

  # Check if the output is a list
  expect_true(is.list(annotations))

  # Check the length of the output list matches the number of rows in
  # x_ticks_sample
  expect_equal(length(annotations), nrow(x_ticks_sample))

  # Check the structure of the first annotation
  expect_equal(annotations[[1]]$x, 2020)
  expect_equal(annotations[[1]]$y, y_position_for_annotations)
  expect_equal(annotations[[1]]$text, "<b>2.5</b>%")
  expect_false(annotations[[1]]$showarrow)
  expect_equal(annotations[[1]]$xref, "x")
  expect_equal(annotations[[1]]$yref, "paper")
  expect_equal(annotations[[1]]$xanchor, "center")
  expect_equal(annotations[[1]]$yanchor, "top")
  expect_equal(annotations[[1]]$font$color, "#FF0000")

  # Case 2: Specify font (i.e. size and family)
  font <- list(family = "Arial", size = 17)
  annotations <- create_annotations(
    x_ticks_sample,
    y_position_for_annotations,
    font = font
  )

  expect_equal(annotations[[1]]$font$family, font$family)
  expect_equal(annotations[[1]]$font$size, font$size)
})

test_that("optimal_ticks", {
  # Case 1:
  ts_growth <- ets(
    c(5, 5.4, -4.3, 3.8, -4.8, -3.4, 4.5, 5.1, -3.4, 2.5, 4.5, -6.7),
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
  df_long
  x_range <- list(end = 2026, start = 2021)

  result <- get_optimal_ticks(df_long, x_range)

  expected_result <- list(
    y1_tickvals =
      c(
        -6.968, -3.884, -0.800000000000001, 2.284, 5.368
      ),
    y2_tickvals = c(
      97.0564738775031, 102.664259946746, 108.27204601599,
      113.879832085233, 119.487618154476
    ), y2_ticktext = c(
      "97", "103", "108", "114", "119"
    ), extremas = list(growth = list(
      min = -6.968,
      max = 5.368
    ), level = list(min = 97.0564738775031, max = 119.487618154476))
  )
  expect_equal(result, expected_result)

  # Case 2: df_long with NAs
  new_row <- list(
    dates = 2023,
    sample_status = "base_forecast",
    frames = "2023-Q2",
    variable = "consumption",
    value = NA,
    data_type = "level"
  )
  df_long <- rbind(df_long, new_row)

  result <- get_optimal_ticks(df_long, x_range)

  expect_equal(result, expected_result)
})
