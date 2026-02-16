#' Autocorrelation Function Plots for koma_estimate Objects
#'
#' Visualize autocorrelation functions (ACF) for coefficient draws from a
#' `koma_estimate` object. By default, beta, gamma, and sigma draws are shown
#' when available.
#'
#' @param x A `koma_estimate` object.
#' @param ... Additional arguments controlling the plot. See Details.
#'
#' @details
#' Additional arguments supported in `...`:
#' \describe{
#'   \item{variables}{Optional character vector of endogenous variables to plot.}
#'   \item{params}{Optional character vector of parameter groups to plot
#'     (e.g., "beta", "gamma", "sigma"). Defaults to all available.}
#'   \item{thin}{Optional integer thinning interval for the stored draws.
#'     Default is 1 (no thinning).}
#'   \item{max_draws}{Optional integer cap on the number of draws used per ACF.
#'     When set, the most recent draws are kept.}
#'   \item{max_lag}{Optional integer maximum lag for ACF computation. When
#'     NULL, defaults to `min(30, n_draws - 1)` per series.}
#'   \item{conf_level}{Optional numeric confidence level in `(0, 1)` used for
#'     ACF significance bands. Default is `0.95`.}
#'   \item{scales}{Facet scale option passed to \code{ggplot2::facet_wrap}.
#'     Default is "fixed".}
#'   \item{facet_ncol}{Optional integer number of columns for facets.}
#'   \item{interactive}{Logical. If TRUE and plotly is available, return an
#'     interactive plot via \code{plotly::ggplotly}. Default is FALSE.}
#' }
#'
#' ACF values are computed with \code{stats::acf(..., plot = FALSE)} for each
#' retained coefficient draw series.
#'
#' Note: \code{sigma} plots use \code{omega_tilde_jw} and show only variances
#' (no covariances) from each covariance draw.
#'
#' The red dashed horizontal lines show approximate significance bounds
#' \eqn{\pm z_{1-\alpha/2}/\sqrt{n}} for zero autocorrelation, where
#' \eqn{\alpha = 1 - \code{conf_level}} and \eqn{n} is the number of retained
#' draws for the corresponding coefficient series.
#'
#' @return A ggplot object, or a plotly object when `interactive = TRUE` and
#'   plotly is available.
#'
#' @export
acf_plot <- function(x, ...) {
  UseMethod("acf_plot")
}

#' @export
acf_plot.koma_estimate <- function(x, ...) {
  stopifnot(inherits(x, "koma_estimate"))

  args <- list(...)
  variables <- args$variables
  params <- args$params
  thin <- args$thin
  max_draws <- args$max_draws
  max_lag <- args$max_lag
  conf_level <- args$conf_level
  interactive <- args$interactive
  facet_ncol <- args$facet_ncol
  scales <- args$scales

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort(c(
      "!" = "The {.pkg ggplot2} package is required for plotting.",
      "i" = "Install it with: {.code install.packages('ggplot2')}"
    ))
  }

  if (!is.null(scales)) {
    if (!is.character(scales) || length(scales) != 1L || is.na(scales)) {
      cli::cli_abort("`scales` must be a single character value.")
    }
    scales <- tolower(scales)
    if (!scales %in% c("fixed", "free", "free_x", "free_y")) {
      cli::cli_abort(
        "`scales` must be one of \"fixed\", \"free\", \"free_x\", or \"free_y\"."
      )
    }
  } else {
    scales <- "fixed"
  }

  if (is.null(thin)) {
    thin <- 1L
  } else if (!is.numeric(thin) || length(thin) != 1L || is.na(thin)) {
    cli::cli_abort("`thin` must be a single numeric value.")
  }
  thin <- as.integer(thin)
  if (!is.finite(thin) || thin < 1L) {
    cli::cli_abort("`thin` must be a positive integer.")
  }

  if (!is.null(max_draws)) {
    if (!is.numeric(max_draws) || length(max_draws) != 1L || is.na(max_draws)) {
      cli::cli_abort("`max_draws` must be a single numeric value.")
    }
    max_draws <- as.integer(max_draws)
    if (!is.finite(max_draws) || max_draws < 1L) {
      cli::cli_abort("`max_draws` must be a positive integer.")
    }
  }

  if (!is.null(max_lag)) {
    if (!is.numeric(max_lag) || length(max_lag) != 1L || is.na(max_lag)) {
      cli::cli_abort("`max_lag` must be a single numeric value.")
    }
    max_lag <- as.integer(max_lag)
    if (!is.finite(max_lag) || max_lag < 1L) {
      cli::cli_abort("`max_lag` must be a positive integer.")
    }
  }

  if (is.null(conf_level)) {
    conf_level <- 0.95
  } else if (!is.numeric(conf_level) || length(conf_level) != 1L || is.na(conf_level)) {
    cli::cli_abort("`conf_level` must be a single numeric value.")
  }
  if (!is.finite(conf_level) || conf_level <= 0 || conf_level >= 1) {
    cli::cli_abort("`conf_level` must be strictly between 0 and 1.")
  }

  if (!is.null(facet_ncol)) {
    if (!is.numeric(facet_ncol) || length(facet_ncol) != 1L || is.na(facet_ncol)) {
      cli::cli_abort("`facet_ncol` must be a single numeric value.")
    }
    facet_ncol <- as.integer(facet_ncol)
    if (!is.finite(facet_ncol) || facet_ncol < 1L) {
      cli::cli_abort("`facet_ncol` must be a positive integer.")
    }
  }

  if (!is.null(interactive)) {
    if (!is.logical(interactive) || length(interactive) != 1L || is.na(interactive)) {
      cli::cli_abort("`interactive` must be TRUE or FALSE.")
    }
  }
  interactive <- isTRUE(interactive)

  estimates <- x$estimates
  if (is.null(variables)) {
    variables <- names(estimates)
  } else if (!is.character(variables) || !length(variables) || anyNA(variables)) {
    cli::cli_abort("`variables` must be a non-empty character vector.")
  }
  if (is.null(variables) || !length(variables)) {
    cli::cli_abort("`variables` must contain at least one name.")
  }

  if (!is.null(params)) {
    if (!is.character(params) || !length(params) || anyNA(params)) {
      cli::cli_abort("`params` must be a non-empty character vector.")
    }
  }

  missing_vars <- setdiff(variables, names(estimates))
  if (length(missing_vars)) {
    cli::cli_abort(c(
      "The following variables are not contained in the estimates:",
      ">" = missing_vars
    ))
  }

  allowed_params <- c("beta", "gamma", "sigma")
  if (is.null(params)) {
    params <- allowed_params
  } else {
    params <- tolower(params)
    unknown <- setdiff(params, allowed_params)
    if (length(unknown)) {
      cli::cli_abort(c(
        "Unknown parameter group(s):",
        ">" = unknown
      ))
    }
    params <- intersect(allowed_params, params)
  }
  if (!length(params)) {
    cli::cli_abort("No parameter groups selected for plotting.")
  }

  sys_eq <- x$sys_eq

  build_draw_df <- function(draws, variable, param_name, coef_names,
                            mat_transform = NULL) {
    prepared <- prepare_draw_matrix(
      draws = draws,
      thin = thin,
      mat_transform = mat_transform
    )
    if (is.null(prepared)) {
      return(NULL)
    }

    mat <- prepared$mat
    draw_idx <- prepared$draw_idx
    if (!is.null(max_draws) && length(draw_idx) > max_draws) {
      keep_idx <- utils::tail(seq_along(draw_idx), max_draws)
      draw_idx <- draw_idx[keep_idx]
      mat <- mat[, keep_idx, drop = FALSE]
    }

    coef_names <- normalize_coef_names(coef_names, nrow(mat))

    df_list <- list()
    for (coef_jx in seq_len(nrow(mat))) {
      series <- as.numeric(mat[coef_jx, ])
      series <- series[is.finite(series)]
      n_series <- length(series)
      if (n_series < 2L) {
        next
      }

      lag_max <- if (is.null(max_lag)) {
        min(30L, n_series - 1L)
      } else {
        min(max_lag, n_series - 1L)
      }
      if (lag_max < 1L) {
        next
      }

      acf_j <- stats::acf(series, lag.max = lag_max, plot = FALSE)
      acf_vals <- as.numeric(acf_j$acf)
      lag_vals <- seq_along(acf_vals) - 1L
      alpha <- 1 - conf_level
      critical <- stats::qnorm(1 - alpha / 2)
      conf <- critical / sqrt(n_series)

      df_j <- data.frame(
        lag = lag_vals,
        acf = acf_vals,
        confint = conf,
        variable = variable,
        param = param_name,
        coef = coef_names[coef_jx],
        label = paste(variable, param_name, coef_names[coef_jx], sep = ":"),
        stringsAsFactors = FALSE
      )
      df_list[[length(df_list) + 1L]] <- df_j
    }

    if (!length(df_list)) {
      return(NULL)
    }

    do.call(rbind, df_list)
  }

  df_list <- list()

  for (variable in variables) {
    jx <- match(variable, sys_eq$endogenous_variables)
    if (is.na(jx)) {
      cli::cli_abort(c(
        "Variable not found in endogenous variables:",
        ">" = variable
      ))
    }

    estimate_jx <- estimates[[variable]]

    idx_beta <- which(sys_eq$character_beta_matrix[, jx] != 0)
    if (length(idx_beta)) {
      beta_names <- sys_eq$total_exogenous_variables[idx_beta]
      df_beta <- add_draws(
        param_name = "beta",
        draws = estimate_jx$beta_jw,
        coef_names = beta_names,
        params = params,
        variable = variable,
        build_draw_df = build_draw_df
      )
      if (!is.null(df_beta)) {
        df_list[[length(df_list) + 1L]] <- df_beta
      }
    }

    idx_gamma <- which(
      sys_eq$character_gamma_matrix[, jx] != 0 &
        sys_eq$character_gamma_matrix[, jx] != 1
    )
    if (length(idx_gamma)) {
      gamma_names <- sys_eq$endogenous_variables[idx_gamma]
      df_gamma <- add_draws(
        param_name = "gamma",
        draws = estimate_jx$gamma_jw,
        coef_names = gamma_names,
        params = params,
        variable = variable,
        build_draw_df = build_draw_df
      )
      if (!is.null(df_gamma)) {
        df_list[[length(df_list) + 1L]] <- df_gamma
      }
    }

    df_sigma <- add_draws(
      param_name = "sigma",
      draws = estimate_jx$omega_tilde_jw,
      coef_names = "omega",
      params = params,
      variable = variable,
      build_draw_df = build_draw_df,
      # Plot only the variance element from each covariance draw.
      mat_transform = function(mat) mat[1, , drop = FALSE]
    )
    if (!is.null(df_sigma)) {
      df_list[[length(df_list) + 1L]] <- df_sigma
    }
  }

  if (!length(df_list)) {
    cli::cli_abort("No draws available to plot after filtering.")
  }

  df_long <- do.call(rbind, df_list)
  conf_df <- unique(df_long[c("label", "confint")])

  .data <- ggplot2::.data
  fig <- ggplot2::ggplot(df_long, ggplot2::aes(x = .data$lag, y = .data$acf)) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.3, color = "#666666") +
    ggplot2::geom_hline(
      data = conf_df,
      ggplot2::aes(yintercept = .data$confint),
      linetype = "dashed",
      linewidth = 0.3,
      color = "#b2182b"
    ) +
    ggplot2::geom_hline(
      data = conf_df,
      ggplot2::aes(yintercept = -.data$confint),
      linetype = "dashed",
      linewidth = 0.3,
      color = "#b2182b"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(xend = .data$lag, y = 0, yend = .data$acf),
      linewidth = 0.45,
      color = "#2b2b2b",
      alpha = 0.9
    ) +
    ggplot2::facet_wrap(~label, scales = scales, ncol = facet_ncol) +
    ggplot2::labs(x = "Lag", y = "ACF") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(size = 9)
    )

  if (interactive) {
    if (!requireNamespace("plotly", quietly = TRUE)) {
      cli::cli_abort(c(
        "!" = "The {.pkg plotly} package is required for interactive plots.",
        "i" = "Install it with: {.code install.packages('plotly')}"
      ))
    }
    fig <- plotly::ggplotly(fig)
  }

  fig
}
