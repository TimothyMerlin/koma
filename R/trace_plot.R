#' Trace Plots for koma_estimate Objects
#'
#' Visualize MCMC trace plots for coefficient draws from a `koma_estimate`
#' object. By default, beta, gamma, and sigma draws are shown when available.
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
#'   \item{max_draws}{Optional integer cap on the number of draws per trace.
#'     When set, the most recent draws are kept.}
#'   \item{scales}{Facet scale option passed to \code{ggplot2::facet_wrap}.
#'     Default is "free_y".}
#'   \item{facet_ncol}{Optional integer number of columns for facets.}
#'   \item{interactive}{Logical. If TRUE and plotly is available, return an
#'     interactive plot via \code{plotly::ggplotly}. Default is FALSE.}
#' }
#'
#' Note: \code{sigma} plots use \code{omega_tilde_jw} and show only variances
#' (no covariances) from each covariance draw.
#'
#' @return A ggplot object, or a plotly object when `interactive = TRUE` and
#'   plotly is available.
#'
#' @export
trace_plot <- function(x, ...) {
  UseMethod("trace_plot")
}

#' @export
trace_plot.koma_estimate <- function(x, ...) {
  stopifnot(inherits(x, "koma_estimate"))

  args <- list(...)
  variables <- args$variables
  params <- args$params
  thin <- args$thin
  max_draws <- args$max_draws
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
    scales <- "free_y"
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

    values <- as.vector(mat)
    keep <- is.finite(values)
    if (!any(keep)) {
      return(NULL)
    }

    coef_rep <- rep(coef_names, times = length(draw_idx))
    df <- data.frame(
      draw = rep(draw_idx, each = nrow(mat)),
      value = values,
      variable = variable,
      param = param_name,
      coef = coef_rep,
      label = paste(variable, param_name, coef_rep, sep = ":"),
      stringsAsFactors = FALSE
    )
    df[keep, , drop = FALSE]
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

  .data <- ggplot2::.data
  fig <- ggplot2::ggplot(df_long, ggplot2::aes(x = .data$draw, y = .data$value)) +
    ggplot2::geom_line(color = "#2b2b2b", linewidth = 0.4, alpha = 0.7) +
    ggplot2::facet_wrap(~label, scales = scales, ncol = facet_ncol) +
    ggplot2::labs(x = "Draw", y = "Value") +
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
