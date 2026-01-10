#' Generate Tick Text and Values for a Plot
#'
#' This function generates tick text and values based on a provided data frame
#' and an optional existing plot figure. The tick text and values are
#' specifically structured for showcasing annual growth rates on a timeline.
#'
#' @param df_long A data frame in a long format, containing at least the columns
#' `data_type`, `color_code`, `value`, and `dates`. This data frame should
#' have a row for each date and data type, along with the corresponding value
#' and color code.
#' @param fig An optional existing plotly figure object to which new ticks will
#' be added. If provided, the function will integrate new tick text and values
#' with the existing ticks on the figure. Default is NULL, indicating no
#' existing figure.
#'
#' @return A list containing two elements: `ticktext` and `tickvals`. The
#' `ticktext` element is a character vector of HTML formatted strings for
#' each tick, showcasing the year and annual growth rate. The `tickvals` element
#' is an integer vector of years corresponding to each tick.
#' @keywords internal
get_x_ticks <- function(df_long, fig = NULL) {
  get_ticktext <- function(df_long) {
    df_long <- subset(df_long, df_long$data_type == "growth_annual")

    ticktext <- c(
      paste0(
        "<br><span style='color:",
        df_long$color_code,
        ";'><b>",
        round(df_long$value, 1),
        "%</b></span>"
      )
    )
    return(list(ticktext = ticktext, tickvals = df_long$dates))
  }

  current_ticks <- get_ticktext(df_long)

  if (inherits(fig, "plotly")) {
    prev_ticks <- (list(
      ticktext = fig$x$layoutAttrs[[1]]$xaxis$ticktext,
      tickvals = fig$x$layoutAttrs[[1]]$xaxis$tickvals
    ))
    # Remove first element
    prev_ticks$ticktext <- prev_ticks$ticktext[-1]
    prev_ticks$tickvals <- prev_ticks$tickvals[-1]
  } else {
    prev_ticks <- NULL
  }
  # Add line break if there is a current value but no previous
  years <- unique(sort(c(prev_ticks$tickvals, current_ticks$tickvals)))
  years_diff <- setdiff(years, prev_ticks$tickvals)
  extend_prev_ticktext <- sapply(years_diff, function(year) {
    return(paste0(year, "<br>"))
  })
  prev_ticks$ticktext <- c(prev_ticks$ticktext, extend_prev_ticktext)


  ticktext <- sapply(years, function(year) {
    which_prev_tick <- which(prev_ticks$tickvals == year)
    which_tick <- which(current_ticks$tickvals == year)

    if (length(which_prev_tick) > 0) {
      # there exists a previous tick for the year
      return(paste0(
        prev_ticks$ticktext[which_prev_tick],
        current_ticks$ticktext[which_tick]
      ))
    } else {
      # no previous tick for the year
      return(paste0(year, "<br>", current_ticks$ticktext[which_tick]))
    }
  })

  ticks <- list(
    ticktext = c(paste0(years[1] - 1), ticktext),
    tickvals = as.integer(c(years[1] - 1, years))
  )

  return(ticks)
}

#' Calculate Y-Axis Tick Marks for Multiple Axes in Plotly
#'
#' @description
#' This function computes the tick marks for y-axes on a Plotly graph, ensuring
#' that gridlines align across multiple y-axes by matching dtick ratios. It
#' adjusts the maximum range of the axes based on these ratios to align
#' gridlines correctly.
#'
#' @param extremas A list with 'min' and 'max' values for each y-axis.
#' @param num_ticks The desired number of ticks on each y-axis.
#' @param center_around A numeric value to center the axis ranges.
#'
#' @details
#' The function calculates tick mark spacing (`dtick`) and ratios, then adjusts
#' axis ranges to match the largest dtick ratio, ensuring gridlines align across
#'  axes. It avoids reducing the axis range maximum to prevent cutting off data
#' points.
#'
#' @return A list with dtick, min, and max for each y-axis, adjusted for
#' alignment.
#'
#' @keywords internal
#' @references
#' Adapted from the methodology outlined by Victor Bezak in the GitHub
#' repository:
#' https://github.com/VictorBezak/Plotly_Multi-Axes_Gridlines
get_y_ticks <- function(extremas, num_ticks, center_around) {
  axes <- list(y = NULL, y2 = NULL)

  # Rebase extremas by shifiting them so that they are centered around 0.
  rebase_extremas <- function(y_extremas, y_center_around) {
    centered_min <- y_extremas$min - y_center_around
    centered_max <- y_extremas$max - y_center_around

    return(list(min = centered_min, max = centered_max))
  }

  y_centered_extremas <- rebase_extremas(extremas$y, center_around$y)
  y2_centered_extremas <- rebase_extremas(extremas$y2, center_around$y2)

  get_dticks <- function(y_min, y_max, num_ticks) {
    if (y_min < 0) {
      y_range <- y_max - y_min
    } else {
      y_range <- y_max
    }

    y_range <- y_range * 1000 # mult by 1000 to account for ranges < 1
    y_len <- nchar(as.character(floor(y_range)))

    y_pow10_divisor <- 10^(y_len - 1)
    y_firstdigit <- floor(y_range / y_pow10_divisor)
    # div by 1000 to account for ranges < 1
    y_max_base <- y_pow10_divisor * y_firstdigit / 1000

    dtick <- y_max_base / num_ticks
    y_range <- y_range / 1000

    # Finding the intitial ratios for comparison
    dtick_ratio <- y_range / dtick

    return(list(dtick = dtick, y_range = y_range, dtick_ratio = dtick_ratio))
  }

  axes$y <- get_dticks(
    y_centered_extremas$min, y_centered_extremas$max, num_ticks
  )
  axes$y2 <- get_dticks(
    y2_centered_extremas$min, y2_centered_extremas$max, num_ticks
  )

  # Capture the highest dtick ratio as your global dtick ratio.
  # All other axes will have their positive and negative ranges scaled to
  # make their dtick_ratios match the global ratio. When the ratios match,
  # the gridlines match!
  global_dtick_ratio <- max(axes$y$dtick_ratio, axes$y2$dtick_ratio)

  ## Calculate Range Maximums / Minimums
  # 1. This is done by first finding the positive (negative) ratio for all
  # axes:
  #     1) What percentage of the range is coming from positive (negative)
  #        values?
  #     2) Multiply percentage by global ratio to get the percentage of the
  #        global ratio (percentage of total gridlines) that should be shown
  #        above (below) the zero baseline.
  #
  # 2. Capturing the positive (negative) ratio as the global positive (negative)
  #    ratio.
  #
  # 3. Then applying the positive (negative) ratio to all of the axis maximums
  #    (minimums) to get their new proportionally scaled range maximums
  #    (minimums).
  get_ratio <- function(y_value, y_range) {
    ratio <- abs(y_value / y_range) * global_dtick_ratio

    return(ratio)
  }
  axes$y$positive_ratio <-
    get_ratio(y_centered_extremas$max, axes$y$y_range)
  axes$y2$positive_ratio <-
    get_ratio(y2_centered_extremas$max, axes$y2$y_range)

  # Increase the ratio by 0.1 so that your range maximums are extended just
  # far enough to not cut off any part of your highest value.
  global_positive_ratio <-
    max(axes$y$positive_ratio, axes$y2$positive_ratio) + 0.1

  y_range_max <- global_positive_ratio * axes$y$dtick
  y2_range_max <- global_positive_ratio * axes$y2$dtick

  axes$y$negative_ratio <-
    get_ratio(y_centered_extremas$min, axes$y$y_range)
  axes$y2$negative_ratio <-
    get_ratio(y2_centered_extremas$min, axes$y2$y_range)

  # Increase the ratio by 0.1 so that your range minimums are extended just
  # far enough to not cut off any part of your lowest value.
  global_negative_ratio <-
    max(axes$y$negative_ratio, axes$y2$negative_ratio) + 0.1

  # If any negative value is present, you must proportionally extend the
  # range minimum of all axes.
  if (any(y_centered_extremas$min < 0, y2_centered_extremas$min < 0)) {
    y_range_min <- (global_negative_ratio) * axes$y$dtick * -1
    y2_range_min <- (global_negative_ratio) * axes$y2$dtick * -1
  } else { # If no negatives, the baseline is set to zero.
    y_range_min <- center_around$y
    y2_range_min <- center_around$y2
  }

  return(list(y = list(
    dtick = axes$y$dtick,
    # Restore the original scale
    min = y_range_min + center_around$y,
    max = y_range_max + center_around$y
  ), y2 = list(
    dtick = axes$y2$dtick,
    min = y2_range_min + center_around$y2,
    max = y2_range_max + center_around$y2
  )))
}

#' Find Extrema Values for Growth Rates and Levels
#'
#' Computes the minimum and maximum values for growth rates and levels from
#' a given data frame, adjusting these values by a certain percentage to provide
#' a margin. This function is useful for determining the range of y-axis values
#' in plotting functions.
#'
#' @param df_long A data frame in long format that includes 'data_type' as a
#' column, which should contain the types 'growth' and 'level' for
#' categorizing the data.
#'
#' @return A list with two elements 'growth' and 'level', each containing
#' a 'min' and 'max' sub-element. These represent the adjusted minimum
#' and maximum values for the growth rates and levels, respectively.
#' @keywords internal
find_extremas <- function(df_long) {
  df_growth <- subset(df_long, df_long$data_type == "growth")
  df_level <- subset(df_long, df_long$data_type == "level")

  return(list(
    growth = list(
      min = min(df_growth$value) - 0.04 * abs(min(df_growth$value)),
      max = max(df_growth$value) + 0.04 * abs(min(df_growth$value))
    ),
    level = list(
      min = min(df_level$value) - 0.04 * abs(min(df_level$value)),
      max = max(df_level$value) + 0.04 * abs(max(df_level$value))
    )
  ))
}

#' Find Optimal Ticks for y Axis
#'
#' @param df_long A data frame in long format that includes 'data_type' as a
#' column, which should contain the types 'growth' and 'level' for
#' categorizing the data.
#' @param x_range A list with x-axis range with start and end values of format
#' c(YEAR, QUARTER).
#'
#' @return A list with y1 (growth) values and text, y2 (level)
#' text and extremas.
#' @keywords internal
get_optimal_ticks <- function(df_long, x_range) {
  optimal_num_ticks_for_both_axes <- function(min_val1, max_val1,
                                              min_val2, max_val2) {
    # Calculate the span of the data for both axes
    span1 <- max_val1 - min_val1
    span2 <- max_val2 - min_val2

    # Initialize potential ticks
    potential_ticks <- 5:10

    # Calculate interval for each potential tick number for both axes
    intervals1 <- span1 / potential_ticks
    intervals2 <- span2 / potential_ticks

    # Function to determine "niceness" of a value
    is_nice <- function(val) {
      return(abs(round(val) - val) < 0.1 || abs(round(val) - val - 0.5) < 0.1)
    }

    # Find ticks where both intervals are "nice"
    nice_for_both <-
      potential_ticks[sapply(1:length(potential_ticks), function(i) {
        is_nice(intervals1[i]) && is_nice(intervals2[i])
      })]

    # If any tick numbers were found that work for both axes, return the
    # smallest (for clarity)
    if (length(nice_for_both) > 0) {
      return(min(nice_for_both))
    } else {
      return(5) # Default to 5 if no suitable tick number is found
    }
  }

  df_long <- stats::na.omit(df_long)

  # Set x_range for df_long
  df_long <- subset(
    df_long,
    df_long$dates >= x_range$start &
      df_long$dates <= x_range$end
  )
  # Find min and max values
  extremas <- find_extremas(df_long)

  # Find optimal number of ticks
  num_ticks <- optimal_num_ticks_for_both_axes(
    extremas$growth$min, extremas$growth$max,
    extremas$level$min, extremas$level$max
  )


  # Generate the ticks for the left y-axis
  y1_tickvals <- seq(
    from = extremas$growth$min, to = extremas$growth$max,
    length.out = num_ticks
  )

  # Generate the ticks for the right y-axis
  y2_tickvals <- seq(
    from = extremas$level$min, to = extremas$level$max,
    length.out = num_ticks
  )

  # Generate tick labels in "k" format
  y2_ticktext <- sapply(y2_tickvals, function(val) {
    if (val >= 1000) {
      return(paste0(round(val / 1000, 0), "k"))
    } else {
      return(as.character(round(val, 0)))
    }
  })

  return(
    list(
      y1_tickvals = y1_tickvals,
      y2_tickvals = y2_tickvals,
      y2_ticktext = y2_ticktext,
      extremas = extremas
    )
  )
}

#' Get Y Position for Annotations in a Figure
#'
#' Calculates the y-axis position for the next annotation to be added to a
#' figure based on the number of existing annotations. This function automates
#' the adjustment of the y-axis position for annotations, ensuring they are
#' placed without overlap by starting with a base y position and modifying it
#' according to the current count of annotations within the figure.
#'
#' @param fig A figure object that potentially contains annotations within its
#' layout attributes. The figure object should adhere to the expected structure,
#' including a list of layout attributes accessible via `$x$layoutAttrs`, where
#' each element may or may not contain an `annotations` list.
#'
#' @return Numeric value indicating the y-axis position for placing the next
#' annotation. This position is calculated based on a predefined base position,
#' with adjustments made for each existing annotation to prevent overlap.
#' @keywords internal
get_y_position_for_annotations <- function(fig) {
  # Base position for the first annotation (adjust as needed)
  base_y_pos <- -0.1
  offset_y <- -0.04 # Offset between annotations

  layout_attrs <- fig$x$layoutAttrs

  if (is.null(layout_attrs)) {
    annotation_count <- 0
  } else {
    annotation_count <- sum(
      sapply(layout_attrs, function(x) !is.null(x$annotations))
    )
  }

  if (annotation_count == 0) {
    return(base_y_pos)
  } else {
    return(base_y_pos + offset_y * annotation_count)
  }
}

#' Create Plotly Annotations at Ticks
#'
#' Generates a list of annotations for Plotly plots based on x tick position.
#' This function is designed to create text annotations for specific points on a
#' plot, such as in-sample and out-of-sample ticks, with customizable y
#' positions and formatting.
#'
#' @param x_ticks A data frame containing the tick information. Must include
#' columns `year` for the x-axis positions, `value` for the text of the
#' annotations, and `color_code` for the color of the text.
#' @param y_position Numeric value specifying the y-axis position of all
#' annotations, typically in paper coordinates (0 to 1).
#' @param font A list specifying font attributes for the annotation text, with
#' optional `family` and `size` components to customize the font family and
#' size, respectively. If not specified, default Plotly text properties are
#' used.
#'
#' @return A list of lists, where each inner list represents an annotation in
#' the format required by Plotly. Each annotation includes properties such as
#' position, text, and formatting.
#' @keywords internal
create_annotations <- function(x_ticks, y_position,
                               font = list(family = NULL, size = NULL)) {
  annotations_list <- list()

  for (i in seq_len(nrow(x_ticks))) {
    annotations_list[[length(annotations_list) + 1]] <- list(
      x = x_ticks$year[i],
      y = y_position,
      text = paste0("<b>", round(x_ticks$value[i], 1), "</b>%"),
      showarrow = FALSE,
      xref = "x",
      yref = "paper",
      xanchor = "center",
      yanchor = "top",
      font = list(
        color = x_ticks$color_code[i],
        family = font$family,
        size = font$size
      )
    )
  }

  return(annotations_list)
}
