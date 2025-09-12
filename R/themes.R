#' Initiate Default Theme
#'
#' Initiate default theme used to plot forecasts.
#'
#' @param index Optional list with two elements: `start` and `end`.
#' These dates are used to anchor the time series data. Dates should
#' be in the format: c(YEAR, QUARTER).
#' @param title A list to specify title options. Default is NULL.
#'   - `text`: Sets the plot's title. If not provided, then the variable name
#'             will be displayed.
#'   - `font`: Sets the plot title font.
#' @param font Sets the global font. Note that fonts used in traces and other
#' layout components inherit from the global font.
#' @param trace_name Optional list to specify the naming of traces.
#' If not provided, the names will be generated based on the
#' start date of the forecasts. For instance, if the forecast
#' starts at 2023-Q1, the default name for `in_sample_growth`
#' would be "2023-Q1 Data (Growth Rate)", and similarly for
#' other traces.
#' The list can have four elements:
#'   - `in_sample_growth`: Naming for in-sample growth data traces.
#'   - `in_sample_level`: Naming for in-sample level data traces.
#'   - `forecast_growth`: Naming for forecast growth data traces.
#'   - `forecast_level`: Naming for forecast level data traces.
#' @param xaxis A list with custom labels for the x-axis.
#'   - `range`: Sets the range of this axis. List with `start` and `end`.
#'   - `tickfont`: Sets this axis' tick font, including tickfont for annual
#'                 growth rates.
#' @param yaxis A list with custom labels for the y-axes `y` (left) and
#'            `y2` (right).
#'   - `y`: A list with custom labels for the y-axis.
#'     - `title`: Label for the left y-axis, defaults to "QoQ, in %" (quarter
#'                on quarter).
#'     - `tick_center`: Value to center the ticks around, defaults to 0.
#'   - `y2`: A list with custom labels for the y-axis.
#'     - `title`: Label for the left y-axis, defaults to "Level".
#'     - `tick_center`: Value to center the ticks around, defaults to 100.
#'   - `tickfont`: Sets this axis' tick font.
#'   - `number_ticks`: Integer for the desired number of ticks in the y-axis.
#' @param legend A list to specify legend options.
#'   - `font`: Sets the legend font.
#' @param color A list containing color options for different elements of the
#' plot.
#' When working with RGBA colors, the alpha value (the 'A' in RGBA)
#' determines the transparency of the color, ranging from 0 (fully
#' transparent) to 1 (fully opaque).
#'   - `marker`: A list specifying the colors for different traces.
#'     - `in_sample`: Color for in-sample data traces.
#'     - `forecast`: Color for forecast traces.
#'   - `bar_textfont`: A list specifying the text color for different bars.
#'     - `in_sample`: Text color for in-sample data bars.
#'     - `forecast`: Text color for forecast data bars.
#'
#' @export
init_koma_theme <- function(index = list(start = NULL, end = NULL),
                            title = list(
                              font = NULL,
                              text = NULL
                            ),
                            font = NULL,
                            trace_name = NULL,
                            xaxis = list(
                              tickfont = NULL,
                              range = list(start = NULL, end = NULL)
                            ),
                            yaxis = list(
                              y = list(
                                title = list(
                                  text = "QoQ, in %"
                                ),
                                tick_center = 0
                              ),
                              y2 = list(
                                title = list(
                                  text = "Level"
                                ),
                                tick_center = 100
                              ),
                              tickfont = NULL,
                              number_ticks = 5
                            ),
                            legend = list(
                              font = NULL
                            ),
                            color = list(
                              marker = list(
                                in_sample = "rgba(111,111,111,0.6)",
                                forecast = "rgba(33,92,175,1)"
                              ),
                              bar_textfont = list(
                                in_sample = "black",
                                forecast = "white"
                              )
                            )) {
  as.list(environment())[names(formals())]
}
