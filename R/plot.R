#' Plot koma Forecasts
#'
#' Plot koma forecasts
#'
#' @param x A `koma_forecast` object ([forecast]).
#' @param y Ignored. Included for compatibility with the generic function.
#' @param ... Additional parameters:
#'   \describe{
#'     \item{variables}{A vector of variable names to plot.}
#'     \item{fig}{Optional. A Plotly figure object. Default is NULL.}
#'     \item{theme}{Optional. A theme for the plot. Default is NULL.}
#'     \item{central_tendency}{Optional. A string specifying the type of
#'     forecast to print. Can be "mean", "median", or a quantile name like
#'     "q_5", "q_50", "q_95". Default is "mean" if available, otherwise
#'     "median", or a specified quantile.}
#'   }
#'
#' @return A Plotly figure object displaying the data.
#'
#' @export
plot.koma_forecast <- function(x, y = NULL, ...) {
  stopifnot(inherits(x, "koma_forecast"))

  new_plot(x, ...)
}

new_plot <- function(x, ...) {
  # Extract additional arguments
  args <- list(...)
  variables <- args$variables
  fig <- args$fig
  theme <- args$theme
  central_tendency <- args$central_tendency

  if (is.null(theme)) {
    theme <- init_koma_theme()
  }

  # sanity checks
  stopifnot(
    "variables must be a non-NULL character vector." =
      !is.null(variables) || is.character(variables)
  )

  stopifnot(
    "fig must be a plotly object or NULL." =
      is.null(fig) || inherits(fig, "plotly")
  )

  if (is.null(central_tendency)) {
    out <- x[["mean"]]
  } else if (central_tendency %in% c("mean", "median")) {
    out <- x[[central_tendency]]
  } else if (central_tendency %in% names(x$quantiles)) {
    out <- x$quantiles[[central_tendency]]
  } else {
    stop("Please provide a valid `central_tendency`.")
  }

  missing_vars <- setdiff(variables, names(out))
  if (length(missing_vars) > 0) {
    stop(
      "The following variables are not contained in forecast: ",
      paste(missing_vars, collapse = ", ")
    )
  }

  tsl <- x$ts_data[names(out)]
  forecast_start <- stats::start(x$mean[[1]])
  current_date <- iterate_n_periods(forecast_start, -1, frequency = 4)
  suppressWarnings(
    tsl <- lapply(tsl, function(x) {
      stats::window(x, end = current_date)
    })
  )
  out <- as_mets(concat(tsl, out))

  # Index level data at dates if start and end dates provided
  if (!any(is.null(theme$index$start), is.null(theme$index$end))) {
    out <- rebase(out, theme$index$start, theme$index$end)
  }

  out_annual <- tempdisagg::ta(level(out),
    conversion = "sum", to = "annual"
  )

  df_long <- prepare_data_to_plot(
    list(
      growth = rate(out),
      level = level(out),
      growth_annual = rate(out_annual)
    ),
    start = dates_to_num(forecast_start, frequency = 4)
  )

  # Only keep the variable(s) that we want to plot
  df_long <- subset(df_long, df_long$variable %in% variables)

  plotli(df_long, fig = fig, theme = theme, args)
}
