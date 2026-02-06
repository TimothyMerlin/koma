#' Plotting Function
#'
#' This function processes the data and creates a plot using Plotly and the
#' theme provided.
#'
#' @param df_long A data frame in long format, containing the data to be
#' plotted.
#' @param fig Optional. A Plotly figure object to update, otherwise creates a
#' new one.
#' @param theme List of default plot output parameters. Defaults to NULL, which
#' leads to \code{\link{init_koma_theme}} being called.
#' Please see the vignette for details about tweaking themes.
#' @param ... Directly pass additional arguments to plotly::layout. For
#' documentation, see
#' \url{https://plotly.com/r/reference/#Layout_and_layout_style_objects}
#'
#' @return
#' A Plotly figure object displaying the data with the specified formatting.
#' @references
#' \url{https://plotly.com/r/reference/#Layout_and_layout_style_objects}
#' @keywords internal
plotli <- function(df_long, fig = NULL, theme = NULL, fan_data = NULL,
                   whisker_data = NULL, ...) {
  if (is.null(theme)) {
    theme <- init_koma_theme()
  }

  df_long <- attach_color_code(df_long, theme$color$marker, "sample_status")

  dates <- sapply(df_long$dates, num_to_dates, 4)
  df_long$dates_formatted <- paste0(dates[1, ], " Q", dates[2, ])
  df_long$year <- dates[1, ]
  df_long$quarter <- paste0(" Q", dates[2, ])

  # Specify trace names used in legend if not overwritten in theme
  if (is.null(theme$trace_name)) {
    df_sub <- subset(
      df_long,
      df_long$sample_status != "in_sample" & df_long$data_type == "growth"
    )
    date_start <- min(df_sub$dates)

    forecast_start <- num_to_dates(date_start, frequency = 4)
    date_str <- paste0(forecast_start[1], "-Q", forecast_start[2])

    theme$trace_name <- list(
      in_sample_growth = paste(date_str, "Data (Growth Rate)", sep = " "),
      in_sample_level = paste(date_str, "Data (Level)", sep = " "),
      forecast_growth = paste(date_str, "Forecast (Growth Rate)", sep = " "),
      forecast_level = paste(date_str, "Forecast (Level)", sep = " ")
    )
  }

  # Set x range to display if not set by user
  theme$xaxis$range$end <- if (is.null(theme$xaxis$range$end)) {
    max(df_long$dates) + 0.25 # add one quarter
  } else {
    dates_to_num(theme$xaxis$range$end, frequency = 4)
  }
  theme$xaxis$range$start <- if (is.null(theme$xaxis$range$start)) {
    theme$xaxis$range$end - 5 # show 5 years
  } else {
    dates_to_num(theme$xaxis$range$start, frequency = 4)
  }

  x_range_padding <- 0.125 # extend x_range by half a quarter

  if (!inherits(fig, "plotly")) {
    fig <- plotly::plot_ly()
    bar_off_set_group <- 1
  } else {
    # if fig is passed then get number of unique frames in figure to determine
    # number of bar off set groups
    existing_frames <- sum(sapply(plotly::plotly_build(fig)$x$data, function(tr) tr$type == "bar"))
    bar_off_set_group <- 1 + existing_frames
  }

  # Get x ticks (years)
  x_axis_ticks <- subset(
    df_long,
    df_long$data_type == "growth_annual"
  )

  # Get x and y positions for annual growth rate annotations (below x ticks)
  x_ticks_in_sample <- subset(
    x_axis_ticks,
    x_axis_ticks$sample_status == "in_sample"
  )
  x_ticks_oos <- subset(
    x_axis_ticks,
    x_axis_ticks$sample_status != "in_sample"
  )
  y_position_for_annotations <- get_y_position_for_annotations(fig)
  annotations_in_sample <- create_annotations(
    x_ticks = x_ticks_in_sample,
    y_position = y_position_for_annotations,
    font = theme$xaxis$tickfont
  )
  annotations_oos <- create_annotations(
    x_ticks = x_ticks_oos,
    y_position = y_position_for_annotations,
    font = theme$xaxis$tickfont
  )
  annotations <- c(annotations_in_sample, annotations_oos)

  # Get optimal ticks
  optimal_y_ticks <- get_optimal_ticks(df_long, theme$xaxis$range, whisker_data)
  ## plot
  # add growth bars for in sample data
  fig <-
    plotly::add_trace(
      fig,
      data = subset(
        df_long,
        df_long$data_type == "growth" & df_long$sample_status == "in_sample"
      ),
      x = ~dates,
      y = ~value,
      customdata = ~dates_formatted,
      color = ~data_type,
      text = ~quarter,
      textfont = list(color = theme$color$bar_textfont$in_sample),
      hovertemplate = "%{customdata}: %{y:.2f}",
      name = theme$trace_name$in_sample_growth,
      type = "bar",
      marker = list(color = ~color_code, width = 4),
      # Set 'offsetgroup' to 'bar_off_set_group' to group bars together at
      # each x-value.
      offsetgroup = bar_off_set_group
    )

  growth_forecast <- subset(
    df_long,
    df_long$data_type == "growth" & df_long$sample_status != "in_sample"
  )
  error_y <- NULL
  if (!is.null(whisker_data)) {
    whisker_growth <- subset(
      whisker_data,
      whisker_data$data_type == "growth"
    )
    if (nrow(whisker_growth) > 0) {
      key <- paste(growth_forecast$dates, growth_forecast$variable)
      whisker_key <- paste(whisker_growth$dates, whisker_growth$variable)
      idx <- match(key, whisker_key)
      growth_forecast$lower <- whisker_growth$lower[idx]
      growth_forecast$upper <- whisker_growth$upper[idx]
      growth_forecast$error_plus <-
        pmax(growth_forecast$upper - growth_forecast$value, 0)
      growth_forecast$error_minus <-
        pmax(growth_forecast$value - growth_forecast$lower, 0)
      if (any(!is.na(growth_forecast$error_plus)) ||
        any(!is.na(growth_forecast$error_minus))) {
        error_y <- list(
          type = "data",
          symmetric = FALSE,
          array = growth_forecast$error_plus,
          arrayminus = growth_forecast$error_minus,
          color = set_alpha(theme$color$marker$forecast, 0.8),
          thickness = 1.5,
          width = 3
        )
      }
    }
  }

  fig <-
    plotly::add_trace(
      fig,
      data = growth_forecast,
      x = ~dates,
      y = ~value,
      customdata = ~dates_formatted,
      color = ~data_type,
      text = ~quarter,
      textfont = list(color = theme$color$bar_textfont$forecast),
      hovertemplate = "%{customdata}: %{y:.2f}",
      name = theme$trace_name$forecast_growth,
      type = "bar",
      marker = list(color = ~color_code, width = 4),
      offsetgroup = bar_off_set_group,
      error_y = error_y
    )

  if (!is.null(fan_data)) {
    fan_data <- subset(fan_data, fan_data$data_type == "level")
    if (nrow(fan_data) > 0) {
      fig <- add_fan_chart(fig, fan_data, theme$color$marker$forecast)
    }
  }

  # add level scatter for in sample data
  fig <-
    plotly::add_trace(
      fig,
      data = subset(
        df_long,
        df_long$data_type == "level" & df_long$sample_status == "in_sample"
      ),
      x = ~dates,
      y = ~value,
      color = ~data_type,
      text = ~dates_formatted,
      hovertemplate = "%{text}: %{y:.2f}",
      name = theme$trace_name$in_sample_level,
      yaxis = "y2",
      type = "scatter",
      mode = "lines",
      line = list(color = ~color_code, width = 4)
    )

  # add level scatter for in sample data
  fig <-
    plotly::add_trace(
      fig,
      data = subset(
        df_long,
        df_long$data_type == "level" & df_long$sample_status != "in_sample"
      ),
      x = ~dates,
      y = ~value,
      color = ~data_type,
      text = ~dates_formatted,
      hovertemplate = "%{text}: %{y:.2f}",
      name = theme$trace_name$forecast_level,
      yaxis = "y2",
      type = "scatter",
      mode = "lines",
      line = list(color = ~color_code, width = 4)
    )

  fig <-
    plotly::layout(
      fig,
      title = list(
        text = ifelse(
          !is.null(theme$title$text), theme$title$text,
          unique(df_long$variable)
        ),
        font = theme$title$font
      ),
      font = theme$font,
      barmode = "group",
      xaxis = list(
        showgrid = TRUE,
        tickvals = x_axis_ticks$year,
        tickfont = theme$xaxis$tickfont,
        tickmode = "array",
        title = "",
        range = c(
          theme$xaxis$range$start - x_range_padding,
          theme$xaxis$range$end + x_range_padding
        )
      ),
      annotations = annotations,
      yaxis = list(
        title = theme$yaxis$y$title,
        tickfont = theme$yaxis$tickfont,
        side = "left",
        range = c(
          optimal_y_ticks$extremas$growth$min,
          optimal_y_ticks$extremas$growth$max
        ),
        tickvals = optimal_y_ticks$y1_tickvals,
        ticktext = round(optimal_y_ticks$y1_tickvals, 1)
      ),
      yaxis2 = list(
        title = theme$yaxis$y2$title,
        tickfont = theme$yaxis$tickfont,
        side = "right",
        range = c(
          optimal_y_ticks$extremas$level$min,
          optimal_y_ticks$extremas$level$max
        ),
        overlaying = "y",
        automargin = TRUE,
        tickvals = optimal_y_ticks$y2_tickvals,
        ticktext = optimal_y_ticks$y2_ticktext
      ),
      legend = list(
        font = theme$legend$font,
        bgcolor = "rgba(0,0,0,0)",
        orientation = "h", # horizontal orientation
        x = 0, # center the legend
        y = -0.35, # position below the x-axis
        xanchor = "left", # anchor at the center
        yanchor = "top", # anchor at the top (bottom of the plot)
        traceorder = "normal", # order as they appear in the traces
        tracegroupgap = 0, # gap between trace groups
        itemsizing = "constant", # all items same size
        itemwidth = 30, # width of each legend item
        itemclick = "toggleothers", # only one item active at a time
        valign = "top", # align vertically at the top
        roworder = "top to bottom", # order of legend items
        ncol = 3 # number of columns
      ),
      hovermode = "closest"
    )

  fig <-
    plotly::layout(
      fig,
      ...
    )

  return(fig)
}

add_fan_chart <- function(fig, fan_data, base_color) {
  band_ids <- unique(fan_data$band)
  band_orders <- sapply(band_ids, function(band) {
    min(fan_data$band_order[fan_data$band == band])
  })
  band_ids <- band_ids[order(band_orders)]
  band_alphas <- seq(0.25, 0.08, length.out = length(band_ids))

  for (band_idx in seq_along(band_ids)) {
    band <- band_ids[band_idx]
    alpha <- band_alphas[band_idx]
    fill_color <- set_alpha(base_color, alpha)

    band_data <- fan_data[fan_data$band == band, , drop = FALSE]
    vars <- unique(band_data$variable)

    for (var in vars) {
      df_band <- band_data[band_data$variable == var, , drop = FALSE]
      df_band <- df_band[order(df_band$dates), , drop = FALSE]

      fig <- plotly::add_trace(
        fig,
        data = df_band,
        x = ~dates,
        y = ~lower,
        type = "scatter",
        mode = "lines",
        legendgroup = "fan",
        line = list(color = fill_color, width = 0),
        hoverinfo = "skip",
        showlegend = FALSE,
        yaxis = "y2"
      )

      fig <- plotly::add_trace(
        fig,
        data = df_band,
        x = ~dates,
        y = ~upper,
        type = "scatter",
        mode = "lines",
        legendgroup = "fan",
        line = list(color = fill_color, width = 0),
        fill = "tonexty",
        fillcolor = fill_color,
        hoverinfo = "skip",
        showlegend = FALSE,
        yaxis = "y2"
      )
    }
  }

  fig
}
