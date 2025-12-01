#' Estimate the Simultaneous Equations Model (SEM)
#'
#' Estimate a system of simultaneous equations model (SEM) using a Bayesian
#' approach. This function incorporates Gibbs sampling and allows for
#' both density and point forecasts.
#'
#' @param ts_data Time-series data set for the estimation.
#' @param sys_eq A `koma_seq` object ([system_of_equations]) containing details
#' about the system of equations used in the model.
#' @param dates Key-value list for date ranges in various model operations.
#' @param ... Additional parameters.
#' @param options Optional settings for modifying the Gibbs sampler
#' specifications for all equations.
#' See \link[=get_default_gibbs_spec]{Gibbs Sampler Specifications}.
#' @param estimates Optional. A `koma_estimate` object
#' (see \code{\link{estimate}}) containing the estimates of the previously
#' estimated simultaneous equations model. Use this parameter when some
#' equations of the system need to be re-estimated.
#' @inheritParams fill_ragged_edge
#'
#' @section Parallel:
#' This function provides the option for parallel computing through
#' the `future::plan()` function.
#' For a detailed example on executing `estimate` in parallel, see the vignette:
#' \code{vignette("parallel")}.
#' For more details, see the [future package
#' documentation](https://cran.r-project.org/web/packages/future/future.pdf).
#'
#' @inheritSection get_default_gibbs_spec Gibbs Sampler Specifications
#'
#' @details
#' After estimation, use \code{\link[=summary.koma_estimate]{summary}} for a
#' full table of posterior summaries (with optional credible intervals and
#' texreg output) and \code{\link[=print.koma_estimate]{print}} for a concise
#' console-friendly overview of the estimated system.
#'
#' @return An object of class `koma_estimate`.
#'
#' An object of class `koma_estimate`is a list containing the following
#' elements:
#' \describe{
#'   \item{estimates}{The estimated parameters and other relevant information
#'   obtained from the model.}
#'   \item{sys_eq}{A `koma_seq` object containing details about the system of
#'   equations used in the model.}
#'   \item{ts_data}{The time-series data used for the estimation, with any `NA`
#'   values removed and lagged variables created.}
#'   \item{y_matrix}{The Y matrix constructed from the balanced data, used in
#'   the estimation process.}
#'   \item{x_matrix}{The X matrix constructed from the balanced data, used in
#'   the estimation process.}
#'   \item{gibbs_specifications}{The specifications used for the Gibbs
#'   sampling.}
#' }
#'
#' @seealso
#' - To create a `koma_seq` object see \code{\link{system_of_equations}}.
#' - For a comprehensive example of using `estimate`, see
#'   \code{vignette("koma")}.
#' - Related functions within the package that may be of interest:
#'   \code{\link{forecast}}.
#' @export
estimate <- function(ts_data, sys_eq, dates,
                     ...,
                     options = list(),
                     point_forecast = NULL,
                     estimates = NULL) {
  check_dots_used(...)
  setup_global_progress_handler()

  equation_settings <- sys_eq$equation_settings[sys_eq$stochastic_equations]
  set_gibbs_settings(options, equation_settings)

  cli::cli_h1("Estimation")
  UseMethod("estimate")
}

#' @export
estimate.list <- function(ts_data, sys_eq, dates,
                          ...,
                          options = list(),
                          point_forecast = NULL,
                          estimates = NULL) {
  if (!inherits(ts_data, "list")) {
    cli::cli_abort("`ts_data` must be a list. You provided a {class(ts_data)}.")
  }
  if (!all(sapply(ts_data, function(x) inherits(x, "koma_ts")))) {
    # defaults
    series_type <- "level"
    method <- "percentage"

    cli::cli_text(
      "Some of the time series in `ts_data` are not `ets`.",
      "They will be automatically converted with `as_ets` using the defaults:"
    )
    cli::cli_h2("Default settings")
    cli::cli_text("series_type = {series_type}")
    cli::cli_text("method = {method}")

    user_input <- readline("Are these correct? (y/n): ")
    if (tolower(user_input) != "y") {
      series_type <- readline("Enter series_type: ")
      method <- readline("Enter method: ")
    }

    # Convert ts -> koma_ts if needed
    ts_data <- lapply(ts_data, function(x) {
      if (inherits(x, "ts") && !inherits(x, "koma_ts")) {
        x <- as_ets(x, series_type = series_type, method = method)
      }
      if (!inherits(x, "koma_ts")) {
        cli::cli_abort("All elements must be koma_ts after conversion")
      }
      x
    })
  }
  if (!all(sapply(ts_data, function(x) inherits(x, "koma_ts")))) {
    cli::cli_abort("Each element in `ts_data` must be of class 'koma_ts'.")
  }
  if (!inherits(sys_eq, "koma_seq")) {
    cli::cli_abort(c(
      "`sys_eq` must be of class 'koma_seq'.",
      "You provided a {class(sys_eq)}."
    ))
  }
  vars <- c(sys_eq$endogenous_variables, sys_eq$exogenous_variables, sys_eq$weight_variables)
  if (any(!vars %in% names(ts_data))) {
    cli::cli_abort(c(
      "The following series are missing in {.var ts_data}: {.var {vars[!vars %in% names(ts_data)]}}"
    ))
  }

  pre <- new_prepare_estimation(ts_data, sys_eq, dates, point_forecast)
  if (is.null(estimates)) {
    estimates <- new_estimate(pre$sys_eq, pre$y_matrix, pre$x_matrix)
  } else {
    # reestimate some equations
    stopifnot(inherits(estimates, "koma_estimate"))
    eq_jx <- identify_reestimation_indices(sys_eq, estimates$sys_eq)
    estimates <- estimates$estimates

    if (!is.null(eq_jx)) {
      # estimate subset
      reestimates <- new_estimate(pre$sys_eq, pre$y_matrix, pre$x_matrix, eq_jx)
      for (name in names(reestimates)) {
        estimates[[name]] <- reestimates[[name]]
      }
      estimates <- estimates[sys_eq$stochastic_equations]
    }
  }

  tryCatch(
    {
      acceptance_probs <- build_settings(
        default = get_default_acceptance_prob(),
        settings = options,
        equation_settings = sys_eq$equation_settings
      )
      validate_estimate_sem(estimates, acceptance_probs)
    },
    error = function(e) {
      cli::cli_alert_danger("Validation failed: {e$message}.")
    }
  )

  structure(
    list(
      estimates = estimates,
      sys_eq = pre$sys_eq,
      ts_data = pre$ts_data,
      y_matrix = pre$y_matrix,
      x_matrix = pre$x_matrix,
      gibbs_specifications = get_gibbs_settings()
    ),
    class = "koma_estimate"
  )
}

new_prepare_estimation <- function(ts_data, sys_eq, dates, point_forecast) {
  # Define default options
  default_point_forecast <- list(active = TRUE, central_tendency = "mean")
  # Merge user-provided options with default options
  if (is.null(point_forecast)) {
    point_forecast <- default_point_forecast
  } else {
    point_forecast <- utils::modifyList(default_point_forecast, point_forecast)
  }

  dates <- dates_to_num(dates, frequency = 4)

  if (is.null(dates$estimation) ||
    is.null(dates$estimation$start) ||
    is.null(dates$estimation$end) ||
    !is.numeric(dates$estimation$start) ||
    !is.numeric(dates$estimation$end)
  ) {
    cli::cli_abort(c(
      "!" = "Invalid {.field dates$estimation}:",
      "x" = "{.field start} and {.field end} must be numeric and provided"
    ))
  }

  # only keep data needed for system
  ts_data <- ts_data[c(
    sys_eq$endogenous_variables,
    sys_eq$exogenous_variables,
    sys_eq$weight_variables
  )]
  ##### Create Lagged Variables
  ts_data <- create_lagged_variables(
    level(ts_data), sys_eq$endogenous_variables, sys_eq$exogenous_variables,
    sys_eq$predetermined_variables
  )

  #### Calculate dynamic weights
  sys_eq$identities <- get_seq_weights(level(ts_data), sys_eq$identities, dates)

  # Check if model can be identified
  model_identification(
    sys_eq$character_gamma_matrix,
    sys_eq$character_beta_matrix,
    sys_eq$identities,
    call = rlang::caller_env()
  )

  ##### Fill ragged edge
  ts_data <- fill_ragged_edge(
    rate(ts_data), sys_eq, sys_eq$exogenous_variables, dates, point_forecast
  )

  ##### Estimate filled balanced data
  ##### Construct Y and X matrix
  balanced_data <- construct_balanced_data(
    rate(ts_data), sys_eq$endogenous_variables,
    sys_eq$total_exogenous_variables,
    dates$estimation$start, dates$estimation$end
  )

  structure(
    list(
      sys_eq = sys_eq,
      ts_data = ts_data,
      y_matrix = balanced_data$y_matrix,
      x_matrix = balanced_data$x_matrix
    )
  )
}

new_estimate <- function(sys_eq, y_matrix, x_matrix, eq_jx = NULL) {
  # estimate model
  estimate_sem(sys_eq, y_matrix, x_matrix, eq_jx = eq_jx)
}

validate_estimate_sem <- function(x, acceptance_probs) {
  validate_acceptance_prob(x, acceptance_probs)

  x
}

get_default_acceptance_prob <- function() {
  list(acceptance_prob = c(0.2, 0.6))
}

validate_acceptance_prob <- function(x, acceptance_prob) {
  acc_rate <- vapply(x, \(z) mean(z$count_accepted, na.rm = TRUE), numeric(1))
  eqs <- names(acc_rate)

  acceptance_prob <- vapply(acceptance_prob, `[[`, numeric(2), "acceptance_prob")
  flag <- acc_rate < acceptance_prob[1, eqs] | acc_rate > acceptance_prob[2, eqs]

  out <- data.frame(
    eq   = eqs,
    rate = sprintf("%.1f%%", acc_rate * 100),
    flag = flag
  )

  if (any(out$flag, na.rm = TRUE)) {
    default <- get_default_acceptance_prob()
    cli::cli_h1(
      cli::col_red("{cli::symbol$warning} MCMC Acceptance Probability Warnings")
    )

    purrr::walk(
      which(out$flag),
      ~ cli::cli_li("{.strong {out$eq[.x]}}: {out$rate[.x]}")
    )
    cli::cli_text("\n")
    cli::cli_alert_info(
      "Some acceptance probabilities are outside the recommended range ({default$acceptance_prob[1]*100}%-{default$acceptance_prob[2]*100}%).
      Consider revising the equations, tuning each equation's tau, or adjusting your priors."
    )
    cli::cli_text("\n")
  }
}

#' @export
format.koma_estimate <- function(x,
                                 ...,
                                 variables = NULL,
                                 central_tendency = "mean",
                                 ci_low = 5,
                                 ci_up = 95,
                                 digits = 2) {
  parsed_eq <- lapply(x$sys_eq$equations, split_eq)

  if (!is.null(variables)) {
    stopifnot(is.character(variables))
    lhs_names <- vapply(parsed_eq, function(eq) eq$lhs, character(1))
    missing <- setdiff(variables, lhs_names)
    if (length(missing)) {
      cli::cli_abort(
        "The following variables are not part of this estimate: {.val {missing}}"
      )
    }
    parsed_eq <- Filter(function(eq) eq$lhs %in% variables, parsed_eq)
  }

  out <- c()
  for (equation in parsed_eq) {
    op <- equation$op
    lhs <- equation$lhs
    rhs <- equation$rhs

    # no coefficients to replace for identity equations
    if (grepl("~", op)) {
      # get estimates
      est <- summary_statistics(
        lhs, x$estimates, x$sys_eq,
        central_tendency, ci_low, ci_up
      )

      rhs <- gsub("constant", round(est[[1]]$coef["constant"], digits), rhs)

      # add coefficients in front of variables
      for (var in names(est[[1]]$coef)) {
        coef_value <- round(est[[1]]$coef[[var]], digits)
        if (var != "constant") {
          rhs <- gsub(var, paste0(coef_value, "*", var), rhs, fixed = TRUE)
        }
      }
    }
    # replace calculated weights for identity equations
    if (grepl("\\(*\\)", lhs)) {
      lhs <- sub("\\([^)]*\\)\\*", "", lhs)
      iden <- x$sys_eq$identities[[lhs]]
      # Replace the components with their weights
      for (component in names(iden$components)) {
        weight_name <- iden$components[[component]]
        weight <- round(iden$weights[[weight_name]], digits)
        # Regular expression to find the pattern (value)*component
        pattern <- paste0("\\(([^\\)]+)\\)\\*", component)
        replacement <- paste0(weight, "*", component)

        rhs <- sub(pattern, replacement, rhs)
      }
    }
    # replace +- with -
    rhs <- gsub("+-", "-", rhs, fixed = TRUE)
    rhs <- gsub("--", "-", rhs, fixed = TRUE)
    # Add spaces around operators in the right-hand side
    rhs <- gsub("([+-])", " \\1 ", rhs)
    rhs <- gsub("L([0-9]+)", "L\\1", rhs)

    out <- c(out, paste0(lhs, op, rhs))
  }

  format.koma_seq(list(equations = out))
}

#' Print method for koma_estimate objects
#'
#' Provides a concise, console-friendly overview of the estimated system.
#'
#' @param x A `koma_estimate` object.
#' @param ... Additional arguments forwarded to formatting internals.
#' @param variables Optional character vector of endogenous variables to print.
#'   Defaults to all variables.
#' @param central_tendency Central tendency used when summarizing estimates
#'   (e.g., "mean", "median"). Defaults to "mean".
#' @param ci_low Lower bound (percent) for credible intervals. Defaults to 5.
#' @param ci_up Upper bound (percent) for credible intervals. Defaults to 95.
#' @param digits Number of digits to print for numeric values. Defaults to 2.
#'
#' @return Invisibly returns `x` after printing.
#' @seealso \code{\link[=summary.koma_estimate]{summary.koma_estimate}} for
#'   detailed posterior summaries.
#' @export
print.koma_estimate <- function(x,
                                ...,
                                variables = NULL,
                                central_tendency = "mean",
                                ci_low = 5,
                                ci_up = 95,
                                digits = 2) {
  cli::cli_h1("Estimates")
  formatted_equations <- format(
    x,
    ...,
    variables = variables,
    central_tendency = central_tendency,
    ci_low = ci_low,
    ci_up = ci_up,
    digits = digits
  )
  cat(paste(formatted_equations, collapse = "\n"), "\n")
}

#' Summary method for koma_estimate objects
#'
#' This function provides a summary for koma_estimate objects.
#' It can return either a texreg object or an ASCII table.
#'
#' @param object A koma_estimate object.
#' @param ... Additional parameters:
#'   \describe{
#'     \item{variables}{Optional. A character vector of variables to summarize.
#'    Default is NULL, which means all variables will be summarized.}
#'     \item{central_tendency}{Optional. A string specifying the measure of
#'      central tendency ("mean", "median"). Default is "mean".}
#'     \item{ci_low}{Optional. Lower bound of the confidence interval. Default
#'          is 5.}
#'     \item{ci_up}{Optional. Upper bound of the confidence interval. Default is
#'          95.}
#'     \item{texreg_object}{Optional. If TRUE, returns a texreg object. Default
#'          is FALSE, which returns an ASCII table.}
#'     \item{digits}{Optional. Number of digits to round numeric values.
#'     Default is 2.}
#'   }
#' @return Depending on the value of texreg_object, returns either a list of
#' texreg objects or prints an ASCII table.
#' @export
summary.koma_estimate <- function(object, ...) {
  is_texreg_installed <- check_texreg_installed()

  args <- list(...)
  variables <- args$variables
  central_tendency <- args$central_tendency
  ci_low <- args$ci_low
  ci_up <- args$ci_up
  texreg_object <- args$texreg_object
  digits <- args$digits

  # Set default values if not provided
  if (is.null(variables)) variables <- names(object$estimates)
  if (is.null(central_tendency)) central_tendency <- "mean"
  if (is.null(ci_low)) ci_low <- 5
  if (is.null(ci_up)) ci_up <- 95
  if (is.null(texreg_object) || !is_texreg_installed) {
    texreg_object <- FALSE
  }
  if (is.null(digits)) digits <- 2

  if (is.null(variables)) variables <- names(object$estimates)
  all_tr <- list()
  for (endogenous_variable in variables) {
    est <- summary_statistics(
      endogenous_variable, object$estimates, object$sys_eq,
      central_tendency, ci_low, ci_up
    )
    if (is_texreg_installed) {
      tr <- texreg::createTexreg(
        coef.names = est[[1]]$coef.names,
        coef = est[[1]]$coef,
        ci.low = est[[1]]$ci.low,
        ci.up = est[[1]]$ci.up,
        pvalues = est[[1]]$pvalues
      )
    } else {
      n <- length(est[[1]]$coef)
      tr <- vector("character", 2 * n)
      ind_coef <- seq(1, 2 * n, 2)
      ind_ci <- seq(2, 2 * n, 2)

      # Fill coefficient elements
      tr[ind_coef] <- round(est[[1]]$coef, digits)
      names(tr)[ind_coef] <- est[[1]]$coef.names

      # Fill CI elements and assign empty names
      tr[ind_ci] <- paste0(
        "[", round(est[[1]]$ci.low, digits), "; ",
        round(est[[1]]$ci.up, digits), "]"
      )
      names(tr)[ind_ci] <- ""
    }
    all_tr[[endogenous_variable]] <- tr
  }

  if (texreg_object) {
    return(all_tr)
  } else {
    if (is_texreg_installed) {
      ci_level <- ci_up - ci_low
      custom_note <- sprintf(
        "Posterior %s (%.0f%% credible interval: [%.1f%%,  %.1f%%])",
        central_tendency, ci_level, ci_low, ci_up
      )

      texreg::screenreg(
        all_tr,
        ci.test = NA,
        digits = digits,
        custom.note = custom_note
      )
    } else {
      for (var in names(all_tr)) {
        tr <- all_tr[[var]]

        lines <- glue::glue("{fr(cli::style_bold(names(tr)))} {fl(tr)}")
        # minus 8 for the invisible styling
        max_width <- max(nchar(lines)) - 8

        cli::cat_line(strrep("=", max_width))
        cli::cli_text("{cli::style_bold(var)}")
        cli::cat_line(strrep("-", max_width))
        cat(lines, sep = "\n")
        cli::cat_line(strrep("=", max_width))
        cli::cat_line("")
      }
    }
  }
}

check_texreg_installed <- function() rlang::is_installed("texreg")

#' @keywords internal
identify_reestimation_indices <- function(current_sys_eq, prev_sys_eq) {
  prev_char_beta <- prev_sys_eq$character_beta_matrix
  char_beta <- current_sys_eq$character_beta_matrix

  # Remove columns of deterministic equations
  char_beta <- char_beta[, current_sys_eq$stochastic_equations]

  eq_jx <- NULL

  for (ix in seq_len(ncol(char_beta))) {
    endog <- colnames(char_beta)[ix]

    # Get exogenous variables in the equation that are not zero
    exog <- rownames(char_beta)[char_beta[, endog] != 0]

    # Determine if the previous exogenous variables exist and
    # get them if they do
    if (endog %in% colnames(prev_char_beta)) {
      prev_exog <- rownames(prev_char_beta)[prev_char_beta[, endog] != 0]
    } else {
      prev_exog <- character()
    }

    if (!identical(exog, prev_exog)) {
      eq_jx <- c(eq_jx, ix)
    }
  }

  eq_jx
}
