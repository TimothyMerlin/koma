#' Attach Color Codes to Data Frame Based on Status
#'
#' This function maps the `status_column` values in the provided data frame to
#' color codes, based on a specified color mapping, and attaches these color
#' codes as a new column `color_code` in the data frame.
#'
#' @param df_long A data frame containing a column specified by `status_column`
#' which indicates the status of each sample.
#' @param marker_color A named list or other key-value mapping structure where
#' the names or keys correspond to statuses and the values correspond to color
#' codes.
#' @param status_column The name of the column in `df_long` that contains the
#' status information.
#'
#' @return A data frame identical to `df_long`, but with an additional column
#' `color_code` which contains the color codes mapped from the `status_column`
#' based on the `marker_color` mapping.
#' @keywords internal
attach_color_code <- function(df_long, marker_color, status_column) {
  # Check if the specified status_column exists in df_long
  if (!status_column %in% names(df_long)) {
    stop("The specified status_column does not exist in df_long")
  }

  # Map the status to color code using the marker_color mapping
  df_long$color_code <- sapply(
    as.character(df_long[[status_column]]),
    function(status) {
      if (status %in% names(marker_color)) {
        return(marker_color[[status]])
      } else {
        return(NA) # Return NA for unknown statuses
      }
    }
  )

  df_long
}

#' Update Alpha Channel for RGB/RGBA Colors
#'
#' Converts rgb/rgba strings to rgba with the provided alpha value.
#'
#' @param color A color string in "rgb(...)" or "rgba(...)" format.
#' @param alpha Numeric alpha value in \code{[0, 1]}.
#' @return A color string with the updated alpha.
#' @keywords internal
set_alpha <- function(color, alpha) {
  if (grepl("^rgba\\(", color)) {
    return(sub(
      "rgba\\(([^,]+),([^,]+),([^,]+),[^)]+\\)",
      sprintf("rgba(\\1,\\2,\\3,%s)", alpha),
      color
    ))
  }
  if (grepl("^rgb\\(", color)) {
    return(sub(
      "rgb\\(([^,]+),([^,]+),([^,]+)\\)",
      sprintf("rgba(\\1,\\2,\\3,%s)", alpha),
      color
    ))
  }
  color
}

#' Build Fan Chart Data from Forecast Quantiles
#'
#' Constructs a long data frame with lower/upper band values for fan charts.
#' If requested quantiles are missing, they are computed from forecast draws.
#'
#' @param x A `koma_forecast` object.
#' @param tsl In-sample time series list used to anchor the forecast.
#' @param forecast_start Forecast start date for windowing.
#' @param variables Character vector of variables to include.
#' @param fan_quantiles Numeric probabilities for the fan chart.
#'
#' @return A data frame with band values for plotting or `NULL` when no bands
#' can be constructed.
#' @keywords internal
build_fan_data <- function(x, tsl, forecast_start, variables, fan_quantiles) {
  quantiles_list <- x$quantiles
  if (is.null(quantiles_list)) {
    quantiles_list <- list()
  }

  if (!is.null(fan_quantiles)) {
    probs <- normalize_quantile_probs(fan_quantiles)
    if (length(probs)) {
      desired_names <- quantile_names_from_probs(probs)
      missing <- setdiff(desired_names, names(quantiles_list))
      if (length(missing)) {
        if (is.null(x$forecasts)) {
          cli::cli_abort(c(
            "x" = "Fan chart requires forecast draws for the requested quantiles.",
            "i" = "Run forecast with point_forecast = list(active = FALSE)."
          ))
        } else {
          freq <- stats::frequency(x$forecasts[[1]])
          missing_probs <- probs[match(missing, desired_names)]
          computed <- quantiles_from_forecasts(
            x$forecasts,
            freq,
            probs = missing_probs
          )
          computed_list <- as_ets_list(computed, tsl)
          if (is.list(computed_list) &&
            length(computed_list) > 0 &&
            all(vapply(computed_list, is_ets, logical(1)))) {
            computed_list <- list(computed_list)
            names(computed_list) <- names(computed)
          }
          quantiles_list <- c(quantiles_list, computed_list)
        }
      }
      quantiles_list <- quantiles_list[intersect(desired_names, names(quantiles_list))]
    }
  }

  if (!length(quantiles_list)) {
    if (is.null(x$forecasts)) {
      cli::cli_abort(c(
        "x" = "Fan chart requires forecast draws, but none are available.",
        "i" = "Run forecast with point_forecast = list(active = FALSE)."
      ))
    }
    cli::cli_abort(c(
      "x" = "Fan chart requested, but no quantiles are available.",
      "i" = "Check that forecast draws are valid and quantiles can be computed."
    ))
  }

  pairs <- get_fan_pairs(names(quantiles_list), fan_quantiles)
  if (!length(pairs)) {
    cli::cli_warn(c(
      "!" = "Fan chart requested, but no symmetric quantile pairs found.",
      "i" = "Provide pairs like 0.1/0.9 or 5/95 (or q_10/q_90)."
    ))
    return(NULL)
  }

  quantile_vars <- names(quantiles_list[[pairs[[1]]$lower]])
  if (is.null(quantile_vars)) {
    quantile_vars <- names(tsl)
  }
  available_vars <- intersect(variables, intersect(names(tsl), quantile_vars))
  if (!length(available_vars)) {
    cli::cli_warn("Fan chart requested, but no matching variables found.")
    return(NULL)
  }
  tsl <- tsl[available_vars]
  out <- list()

  for (ix in seq_along(pairs)) {
    lower_name <- pairs[[ix]]$lower
    upper_name <- pairs[[ix]]$upper

    lower_list <- quantiles_list[[lower_name]]
    upper_list <- quantiles_list[[upper_name]]

    if (is.null(names(lower_list)) && length(lower_list) == length(tsl)) {
      names(lower_list) <- names(tsl)
    }
    if (is.null(names(upper_list)) && length(upper_list) == length(tsl)) {
      names(upper_list) <- names(tsl)
    }

    lower_list <- lower_list[available_vars]
    upper_list <- upper_list[available_vars]

    lower_level <- level(as_mets(concat(tsl, lower_list)))
    upper_level <- level(as_mets(concat(tsl, upper_list)))

    lower_level <- stats::window(lower_level, start = forecast_start)
    upper_level <- stats::window(upper_level, start = forecast_start)

    dates <- as.numeric(stats::time(lower_level))

    lower_level <- as.matrix(lower_level)
    upper_level <- as.matrix(upper_level)
    colnames(lower_level) <- available_vars
    colnames(upper_level) <- available_vars

    for (i in seq_along(available_vars)) {
      var <- available_vars[i]
      lower_vec <- lower_level[, i, drop = TRUE]
      upper_vec <- upper_level[, i, drop = TRUE]
      out[[length(out) + 1L]] <- data.frame(
        dates = dates,
        lower = as.numeric(lower_vec),
        upper = as.numeric(upper_vec),
        band = paste0(lower_name, "-", upper_name),
        band_order = ix,
        variable = var,
        data_type = "level"
      )
    }
  }

  do.call(rbind, out)
}

#' Match Quantile Names into Symmetric Fan Pairs
#'
#' @param quantile_names Character vector of quantile names (e.g., "q_5").
#' @param fan_quantiles Numeric probabilities to constrain pairing.
#'
#' @return A list of lower/upper quantile name pairs.
#' @keywords internal
get_fan_pairs <- function(quantile_names, fan_quantiles = NULL) {
  probs <- vapply(quantile_names, parse_quantile_name, numeric(1))
  valid <- !is.na(probs)
  probs <- probs[valid]
  names(probs) <- quantile_names[valid]

  if (!length(probs)) {
    return(list())
  }

  fan_probs <- if (is.null(fan_quantiles)) {
    probs
  } else {
    normalize_quantile_probs(fan_quantiles)
  }

  if (is.null(fan_probs) || !length(fan_probs)) {
    return(list())
  }
  fan_probs <- fan_probs[!is.na(fan_probs)]
  if (!length(fan_probs)) {
    return(list())
  }

  if (any(fan_probs > 1)) {
    fan_probs <- fan_probs / 100
  }

  fan_probs <- c(fan_probs[fan_probs < 0.5], 1 - fan_probs[fan_probs > 0.5])
  lower_probs <- sort(unique(fan_probs[fan_probs < 0.5]))

  get_name <- function(target, tol = 1e-8) {
    diffs <- abs(probs - target)
    idx <- which.min(diffs)
    if (length(idx) && diffs[idx] <= tol) names(probs)[idx] else NA_character_
  }

  pairs <- lapply(lower_probs, function(p) {
    lower_name <- get_name(p)
    upper_name <- get_name(1 - p)
    if (is.na(lower_name) || is.na(upper_name)) {
      return(NULL)
    }
    list(lower = lower_name, upper = upper_name)
  })

  Filter(Negate(is.null), pairs)
}
