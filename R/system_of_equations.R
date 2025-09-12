#' System of Equations Class
#'
#' Create and manipulate a system of equations.
#'
#' This function constructs an object of class `koma_seq` representing a system
#' of equations, extracting and organizing key components like endogenous
#' variables, gamma matrix, beta matrix, and more. Equations should be separated
#' by commas if provided as a single string.
#'
#' @section Equations:
#' - "epsilon" flags stochastic equations; equations without "epsilon" are
#'    treated as identity equations.
#' - Lagged variables are denoted by `X.L(x)` for variable `X` and lag `L(x)`
#'  (e.g. `.L(1)`, `.L(2)`).
#' - To add an intercept, include "constant" in the equation.
#'
#' @param equations A character string or vector containing the system of
#' equations. If a single string, equations should be separated by commas.
#' @param exogenous_variables A character vector of exogenous variables.
#' @param ... Additional arguments for future extensions.
#'
#' @return An object of class `koma_seq` with the following components:
#' - `equations`: A character vector of the equations.
#' - `endogenous_variables`: A character vector of endogenous variables.
#' - `stochastic_equations`: A character vector of stochastic equations.
#' - `identities`: A character vector of identity equations.
#' - `character_gamma_matrix`: A gamma matrix in character form.
#' - `character_beta_matrix`: A beta matrix in character form.
#' - `predetermined_variables`: A character vector of lagged variables.
#' - `total_exogenous_variables`: A character vector of combined constant,
#'   predetermined, and exogenous variables.
#' - `priors`: A list of priors per equation.
#'
#' @examples
#' equations <-
#'   "consumption ~ gdp + consumption.L(1) + consumption.L(2),
#' investment ~ gdp + investment.L(1) + real_interest_rate,
#' current_account ~ current_account.L(1) + world_gdp,
#' manufacturing ~ manufacturing.L(1) + world_gdp,
#' service ~ service.L(1) + population + gdp,
#' gdp == 0.4*manufacturing + 0.6*service"
#'
#' exogenous_variables <- c("real_interest_rate", "world_gdp", "population")
#'
#' system <- system_of_equations(equations, exogenous_variables)
#' print(system)
#'
#' @seealso [get_endogenous_variables()], [get_identities()],
#'   [construct_gamma_matrix()], [extract_lagged_vars()],
#'   [construct_beta_matrix()], [get_max_lag()]
#' @export
system_of_equations <- function(equations = vector(),
                                exogenous_variables = vector(),
                                ...) {
  if (length(equations) == 1) {
    split_equations <- function(text) {
      # split on comma or newline, but not inside {}, [], or ()
      split_regex <- "(,|\\n)(?![^\\{]*\\})(?![^\\[]*\\])(?![^\\(]*\\))"
      unlist(strsplit(text, split_regex, perl = TRUE))
    }
    equations <- split_equations(equations)
  }

  # Trim whitespace from each component of the vector
  equations <- trimws(equations)
  equations <- gsub(" ", "", equations)
  # Remove empty strings
  equations <- equations[equations != ""]

  priors <- lapply(equations, extract_priors)
  equation_settings <- lapply(equations, extract_settings)
  equations <- sapply(equations, parse_equation, USE.NAMES = FALSE)

  # Validate variable names per single equation
  for (equation in equations) {
    validate_equation(equation)
    validate_priors(equation)
  }

  validate_completeness(equations, exogenous_variables)

  object <- new_system_of_equations(equations, exogenous_variables, priors, equation_settings)

  validate_system_of_equations(object)

  object
}

# low level constructor
new_system_of_equations <- function(equations, exogenous_variables, priors, equation_settings, ...) {
  stopifnot(is.character(equations))

  endogenous_variables <- get_endogenous_variables(equations)

  out <- parse_lags(equations)
  predetermined_variables <- out$variables
  equations <- out$equations

  total_exogenous_variables <- c("constant", predetermined_variables, exogenous_variables)

  character_gamma_matrix <- construct_gamma_matrix(equations, endogenous_variables)
  character_beta_matrix <- construct_beta_matrix(equations, total_exogenous_variables)

  identities <- get_identities(equations, character_gamma_matrix, character_beta_matrix)

  stochastic_variables <- endogenous_variables[!endogenous_variables %in% names(identities)]
  weight_variables <- get_weight_variables(equations)
  equation_settings <- stats::setNames(equation_settings, endogenous_variables)


  structure(
    list(
      equations = equations,
      endogenous_variables = endogenous_variables,
      stochastic_equations = stochastic_variables,
      identities = identities,
      character_gamma_matrix = character_gamma_matrix,
      character_beta_matrix = character_beta_matrix,
      exogenous_variables = exogenous_variables,
      predetermined_variables = predetermined_variables,
      total_exogenous_variables = total_exogenous_variables,
      weight_variables = weight_variables,
      priors = priors,
      equation_settings = equation_settings
    ),
    class = "koma_seq"
  )
}

#' Check if Object is a System of Equations
#'
#' This function checks if the given object inherits from the class `koma_seq`,
#' indicating that it represents a system of equations.
#'
#' @param x An object to be checked.
#'
#' @return Logical. Returns `TRUE` if the object inherits from the class
#' `koma_seq`, and `FALSE` otherwise.
#' @export
is_system_of_equations <- function(x) {
  inherits(x, "koma_seq")
}

#' Format System of Equations
#'
#' This function formats an object of class `koma_seq` for better readability.
#' It formats the equations to ensure proper spacing around operators and aligns
#' the equations for a cleaner display.
#'
#' @param x An object of class `koma_seq`.
#' @param ... Additional arguments passed to or from other methods.
#'
#' @return A character vector of the formatted equations.
#' @export
format.koma_seq <- function(x, ...) {
  parsed <- lapply(x$equations, split_eq)

  # Build a named list with LHS as names and the operator plus RHS as values
  eq_list <- stats::setNames(
    lapply(parsed, function(eq) {
      rhs <- gsub("([+*])", " \\1 ", eq$rhs)
      op <- ifelse(eq$op == "~", "~  ", "== ")
      glue::glue("{op}{fl(rhs)}")
    }),
    sapply(parsed, function(eq) eq$lhs)
  )

  # This call uses fr() on the names so that they align automatically
  glue::glue("{fr(cli::style_bold(names(eq_list)))} {fl(eq_list)}")
}

# Identify equation type and split accordingly
split_eq <- function(eq) {
  if (grepl("~", eq)) {
    parts <- strsplit(eq, "~")[[1]]
    op <- "~"
  } else {
    parts <- strsplit(eq, "==")[[1]]
    op <- "=="
  }
  list(lhs = trimws(parts[[1]]), rhs = trimws(parts[[2]]), op = op)
}

#' Print System of Equations
#'
#' This function prints an object of class `koma_seq`
#' to ensure the equations are displayed in a readable format.
#'
#' @param x An object of class `koma_seq`.
#' @param ... Additional arguments passed to or from other methods.
#' @export
print.koma_seq <- function(x, ...) {
  cli::cli_h1("System of Equations")
  formatted_equations <- format(x, ...)
  cat(paste(formatted_equations, collapse = "\n"), "\n")
}

parse_equation <- function(equation) {
  stopifnot(is.character(equation))

  operator <- if (grepl("~", equation)) {
    "~"
  } else if (grepl("==", equation)) {
    "=="
  } else {
    cli::cli_abort("Equation must either contain '~' or '=='")
  }

  parts <- strsplit(equation, operator)[[1]]
  lhs <- trimws(parts[1])
  rhs <- trimws(parts[2])

  clean_rhs <- function(rhs, pat) {
    # Remove occurrences of pattern (e.g., '+1', '+0', or '-1')
    new_rhs <- gsub(pat, "", rhs, perl = TRUE)
    # Replace multiple '+' with a single '+'
    new_rhs <- gsub("\\+\\s*\\+", "+", new_rhs)
    # Trim leading and trailing '+' signs
    new_rhs <- gsub("^\\s*\\+|\\+\\s*$", "", new_rhs)
    new_rhs
  }

  if (operator == "~") {
    # Strip all priors to prevent false 0 matches inside them
    rhs <- gsub("\\{[^}]*\\}", "", rhs)
    # Strip all equation specific settings
    rhs <- gsub("\\[.*?\\]$", "", rhs)

    # Remove explicit zero (0) to drop the constant
    if (grepl("(?<!\\.L\\()\\b0\\b(?!\\))", rhs, perl = TRUE)) {
      rhs <- clean_rhs(rhs, "(?<!\\.L\\()\\b0\\b(?!\\))")
      # Remove explicit -1 to drop the constant
    } else if (grepl("(?<!\\.L\\()-1(?!\\))", rhs, perl = TRUE)) {
      rhs <- clean_rhs(rhs, "(?<!\\.L\\()-1(?!\\))")
      # Remove explicit 1, then add "constant+" for an intercept
    } else if (grepl("(?<!\\.L\\()\\b1\\b(?!\\))", rhs, perl = TRUE)) {
      rhs <- clean_rhs(rhs, "(?<!\\.L\\()\\b1\\b(?!\\))")
      rhs <- paste0("constant+", rhs)
      # Remove explicit constant (somewhere), then add "constant+" for an intercept
    } else if (grepl("(?<!\\.L\\()\\bconstant\\b(?!\\))", rhs, perl = TRUE)) {
      rhs <- clean_rhs(rhs, "(?<!\\.L\\()\\bconstant\\b(?!\\))")
      rhs <- paste0("constant+", rhs)
      # Implicit constant: add "constant+" at the start
    } else {
      rhs <- paste0("constant+", rhs)
    }
  } else if (operator == "==") {
  } else {
    cli::cli_abort("Equation must be either a stochastic equation '~' or an identity equation '=='")
  }

  paste0(lhs, operator, rhs)
}

#' Extract Variables from a System of Equations
#'
#' This function identifies and extracts variable names from a set of
#' equations. It removes whitespace and splits the equations by common
#' operators such as `~`, `==`, `+`, `*`, and `-`. It filters out components
#' that represent numeric values, only returning variable names.
#'
#' @param equations A character vector containing the equations to extract
#' variables from.
#'
#' @return A list of character vectors, where each vector contains the variables
#' from a single equation.
get_variables <- function(equations) {
  variables <- lapply(equations, function(equation) {
    # remove all whitespace
    equation <- gsub("\\s", "", equation)
    # split equation into components
    components <- strsplit(equation, split = "[~==+*\\-]+")
    # This pattern matches numbers that can have a decimal point
    # with digits before and after it.
    numeric_pattern <- "^\\d*\\.?\\d+$"
    components[[1]][!grepl(numeric_pattern, components[[1]])]
  })
  names(variables) <- equations

  variables
}

#' Get endogenous variables from a system of equations
#'
#' This function takes a system of equations and returns it's
#' endogenous variables.
#'
#' @param equations A character string containing the system of equations,
#' where equations are separated by commas.
#'
#' @return A character vector of endogenous variables.
#' @keywords internal
get_endogenous_variables <- function(equations) {
  equations <- gsub("\\s", "", equations)
  endogenous_variables <- list()

  endogenous_variables <- sapply(strsplit(equations, "==|~"), "[", 1)

  # remove weight names of identiy equations like "(nom_x)*"
  endogenous_variables <- gsub("\\([^)]*\\)\\*", "", endogenous_variables)
  endogenous_variables <- endogenous_variables[!is.na(endogenous_variables)]

  endogenous_variables
}

extract_endogenous_variables <- function(equations) {
  equations <- gsub("\\s", "", equations)

  endogenous_variables <- sapply(
    strsplit(equations, "(~|==)"),
    `[`,
    1
  )

  endogenous_variables <- gsub("\\([^)]*\\)\\*", "", endogenous_variables)

  endogenous_variables <- unique(endogenous_variables)
  endogenous_variables <- endogenous_variables[!is.na(endogenous_variables)]

  endogenous_variables
}

#' Extract identities from equations
#'
#' This function filters out equations containing 'epsilon', then extracts
#' weights and identities based on `character_gamma_matrix` and
#' `character_beta_matrix`.
#'
#' @param equations A character string containing the system of equations,
#' where equations are separated by commas.
#' @param character_gamma_matrix A matrix \eqn{\Gamma} that holds the
#' coefficients in character form for all equations, with potential entries
#' like "theta", to be used in the weights extraction process.
#' The dimensions of the matrix are \eqn{(T \times n)}, where \eqn{T} is the
#' number of observations and \eqn{n} the number of equations.
#' @param character_beta_matrix A matrix \eqn{\beta} that holds the
#' coefficients in character form for all equations, with potential entries
#' like "theta", to be used in the weights extraction process.
#' The dimensions of the matrix are \eqn{(k \times n)}, where \eqn{k} is the
#' number of exogenous variables and \eqn{n} the number of equations.
#'
#' @return A list of identities extracted from the input equations.
#' Each entry contains:
#'   - equation: The raw equation
#'   - components: The components extracted from the equation
#'   - weights: The corresponding weights for each component
#' @keywords internal
get_identities <- function(equations, character_gamma_matrix,
                           character_beta_matrix) {
  # Filter equations that are not stochastic
  filtered_equations <- equations[!grepl("~", equations)]

  get_weight_struct <- function(mat, name) {
    index <- grep("theta", mat)
    indices <- arrayInd(index, .dim = dim(mat))
    # row name is the component's name
    row_names <- rownames(mat)[indices[, 1]]
    # col name is the aggregate's name
    col_names <- colnames(mat)[indices[, 2]]
    col_names <- gsub("\\([^)]*\\)\\*", "", col_names)

    weight_names <- mat[indices]
    # remove - for cases where -theta
    weight_names <- gsub("-", "", weight_names)

    list(
      lhs = col_names,
      component = row_names,
      character_weight = weight_names,
      matrix = rep(name, length(weight_names))
    )
  }

  weights <- list(
    get_weight_struct(character_beta_matrix, "beta"),
    get_weight_struct(character_gamma_matrix, "gamma")
  )
  weights <- list(
    lhs = unlist(lapply(weights, `[[`, "lhs")),
    component = unlist(lapply(weights, `[[`, "component")),
    character_weight = unlist(lapply(weights, `[[`, "character_weight")),
    matrix = unlist(lapply(weights, `[[`, "matrix"))
  )
  weights$character_weight <-
    stats::setNames(weights$character_weight, weights$component)


  # Check if the thetas exist in character gamma or beta matrices
  check_existence <- function(character_weights, character_gamma_matrix,
                              character_beta_matrix) {
    all(sapply(character_weights, function(x) {
      any(grepl(x, character_gamma_matrix)) ||
        any(grepl(x, character_beta_matrix))
    }))
  }

  thetas_exist <- check_existence(
    weights$character_weight, character_gamma_matrix, character_beta_matrix
  )

  if (!thetas_exist) {
    # TODO: add more specific error message
    cli::cli_abort(
      "Not all thetas in weights list exist also in character matrices."
    )
  }

  out <- list()

  # for each identity equation assign the character weight name,
  # e.g. "theta1_7", and the weight value
  for (eq in filtered_equations) {
    result <- get_weights(eq)
    ind_eq <- grep(paste0("^", names(result), "$"), weights$lhs)
    # character weights of equation
    eq_character_weights <- weights$character_weight[ind_eq]
    eq_matrix <- weights$matrix[ind_eq]
    for (idx in seq_along(eq_character_weights)) {
      component <- names(eq_character_weights)[idx]
      theta <- eq_character_weights[[component]]
      result[[names(result)]][["components"]][[component]] <- theta

      ind_component <- which(names(result[[names(result)]][["components"]]) == component)
      names(result[[names(result)]][["weights"]])[ind_component] <- theta

      result[[names(result)]][["matrix"]][ind_component] <- eq_matrix[idx]
    }

    out <- c(out, result)
  }

  out
}

get_weights <- function(eq) {
  x <- unlist(strsplit(eq, "=="))
  lhs <- x[1] # Left-hand side of the equation
  rhs <- x[2] # Right-hand side of the equation

  # Remove parentheses and components from RHS
  components <- gsub("\\([^()]*\\)\\*", "", rhs)

  # Add parentheses around all numbers if they are not already between parentheses
  rhs <- gsub("(?<!\\()\\b(\\d+\\.?\\d*)\\b(?!\\))", "(\\1)", rhs, perl = TRUE)
  # Replace '-(' with '+(-'
  rhs <- gsub("-\\(", "+(-", rhs)

  # Remove parentheses and components from RHS
  # (different in case there where numbers)
  components <- gsub("\\([^()]*\\)\\*", "", rhs)

  # Split RHS at '+', '-', '*', or '/'
  components <- unlist(strsplit(components, "\\+|\\-|\\*|\\/"))

  weights <- list()
  for (component in components) {
    pattern <- paste0("\\(([^)]*?)\\)\\*\\Q", component, "\\E(?!\\w)")
    weight_str <- regmatches(rhs, gregexpr(pattern, rhs, perl = TRUE))

    # Capture only what is between the parentheses
    pattern <- paste0("\\(([^)]*)\\)\\*\\Q", component, "\\E")
    weight_str <- gsub(pattern, "\\1", weight_str)

    weight_num <- suppressWarnings(as.numeric(weight_str))

    if (is.na(weight_num)) {
      weights <- c(weights, weight_str) # Save as string if conversion fails
    } else {
      weights <- c(weights, weight_num) # Save as numeric if conversion succeeds
    }
  }

  stats::setNames(
    list(list(
      equation = eq,
      components = stats::setNames(rep(list(NULL), length(components)), components),
      weights = weights
    )),
    lhs
  )
}

#' Extract Weight Names from Equations
#'
#' This function takes a vector of equations and extracts the names of the
#' variables that appear within parentheses.
#'
#' @inheritParams system_of_equations
#'
#' @return A character vector of unique weight names that appeared within
#' parentheses in the input equations.
#'
#' @keywords internal
get_weight_variables <- function(equations) {
  # Extract words with parentheses
  x <- unlist(regmatches(
    equations,
    gregexpr("\\(([^)]+)\\)", equations)
  ))
  # Remove parentheses
  x <- gsub("[()]", "", x)

  # Split by mathematical operators
  x <- unique(unlist(strsplit(x, "[+*/-]")))

  # Trim whitespace
  x <- trimws(x)

  # Keep only non-numeric strings
  pattern <- "^([0-9]+(\\.[0-9]*)?|\\.[0-9]+)([eE][+-]?[0-9]+)?$"
  x <- x[!grepl(pattern, x)]

  # remove empty elements
  x <- x[nzchar(x)]

  sort(x)
}

#' Extract Component Weights from an Equation
#'
#' Given a mathematical equation, this function identifies its individual
#' components and their associated weights. For components paired with a numeric
#' value via a `*`, that numeric value is considered its weight. Otherwise, the
#' weight is set to NULL.
#'
#' @param raw_eq A character string of a mathematical equation. The equation is
#'   expected to have a format like "lhs == rhs", where the right-hand side
#'   (rhs) might contain components combined using '+' or '-'.
#' @param weights_list A pre-constructed list containing character
#'   representations of weights derived from both `character_gamma_matrix` and
#'   `character_beta_matrix`.
#'
#' @return A list with the following elements:
#'   * `equation`: The original equation provided as input.
#'   * `components`: A list of components extracted from the rhs of the
#'     equation.
#'   * `weights`: A list of weights, corresponding to each component. If a
#'     component doesn't have an explicit weight in the equation, its weight is
#'     NULL.
#'
#' @seealso
#' \code{\link{get_identities}} for a function that uses this function's
#'   outputs.
#' @keywords internal
get_weights_and_comps <- function(raw_eq, weights_list) {
  # Splitting at '==' to get the right-hand side
  eq <- unlist(strsplit(raw_eq, "=="))
  lhs <- eq[1]
  lhs <- gsub("\\([^)]*\\)\\*", "", lhs)
  rhs <- eq[2]

  # Splitting the RHS at '+' or '-'
  rhs_split <- unlist(strsplit(rhs, "\\+|-"))

  convert_variable <- function(x) {
    # if a variable defined in parentheses then use this as weight
    if (grepl("\\(*\\)", x)) {
      return(gsub("[()]", "", x))
    }
    if (all(x == as.integer(x))) {
      return(as.integer(x))
    } else {
      return(as.numeric(x))
    }
  }

  if (grepl("\\*", rhs)) {
    # Replace "-" with unique pattern
    rhs_modified <- gsub("-", "DELIMITER-", rhs)
    # Replace "+" with unique pattern (if needed)
    rhs_modified <- gsub("\\+", "DELIMITER", rhs_modified)
    # Split
    rhs_modified <- unlist(strsplit(rhs_modified, "DELIMITER"))
    rhs_modified <- strsplit(rhs_modified, "\\*")
    # Reverse the order of elements at the lowest level, so that the
    # component name is in the first position and the weigth value in the
    # second position if it exists
    rhs_modified <- lapply(rhs_modified, function(x) x[length(x):1])

    value_weights <- list()
    rhs_split <- c()

    for (var in rhs_modified) {
      # if var has two elements then we have a component for which a weight
      # value is defined in the equation
      if (length(var) == 2) {
        value <- list(convert_variable(var[2]))
      } else if (length(var) == 1) {
        value <- list(NULL)
      }
      names(value) <- var[1]
      value_weights <- append(value_weights, value)
      rhs_split <- append(rhs_split, var[1])
    }
  } else {
    value_weights <- vector("list", length(rhs_split))
  }
  # Trim whitespace
  rhs <- trimws(rhs_split)

  components <- vector("list", length(rhs))
  components <- stats::setNames(components, rhs)

  ind_eq <- grep(lhs, weights_list$lhs)
  # character weights of equation
  eq_character_weights <- weights_list$character_weight[ind_eq]
  for (component in rhs) {
    components[[component]] <- eq_character_weights[[component]]
  }

  value_weights <- stats::setNames(value_weights, components)
  null_elements <- sapply(value_weights, is.null)

  if (any(null_elements)) {
    # Get the names of the elements that are NULL
    null_names <- names(components)[null_elements]
    stop(
      "The following weights of equation ", raw_eq,
      " are NULL: ", paste(null_names, collapse = ", ")
    )
  }

  identity_equation <- list(
    list(
      equation = raw_eq,
      components = components,
      weights = value_weights
    )
  )

  if (grepl("\\(*\\)", eq[1])) {
    # Extract weight name inside the parentheses
    lhs_weight <- gsub(".*\\(([^)]*)\\).*", "\\1", eq[1])
    identity_equation[[1]]$lhs_weight <- lhs_weight
  }

  # Check if any element of value_weights is not numeric and if lhs_weight
  # does not exist
  if (any(sapply(value_weights, function(x) !is.numeric(x))) && !
  exists("lhs_weight")) {
    stop(
      "A non-numeric value weight detected and the left hand side variable ",
      "'lhs_weight' is missing.\n",
      "To define weight calculation, please provide 'lhs_weight'.\n",
      "Example:\n",
      "  (nom_agg_value)*agg_value == (nom_component1)*component1 + ",
      "(nom_component2)*component2\n",
      "The equation provided was: ", raw_eq, "\n"
    )
  }

  names(identity_equation) <- lhs

  identity_equation
}

#' Parse Lagged Variables in Equation Strings
#'
#' @description
#' Identifies and standardizes lagged variable references in a system of
#' equations. Supports both `var.L(lag)` and `lag(var, lag)` notation.
#'
#' The function processes each equation, detects lag patterns, and rewrites
#' them in a unified form. It returns both the modified equations and the list
#' of unique lagged variables.
#'
#' @param equations A character vector of equation strings.
#'
#' @return A list with components:
#' \describe{
#'   \item{variables}{A sorted character vector of unique lagged variables.}
#'   \item{equations}{A character vector of equations with standardized lags.}
#' }
#'
#' @keywords internal
parse_lags <- function(equations) {
  # Define patterns for L() and lag()
  # var_pos captures the group that contains the variable name
  # lag_spec_pos captures the group that contains the lag specification
  patterns <- list(
    list(
      regex = "([a-zA-Z0-9_]+)\\.L\\(([^\\)]+)\\)",
      var_pos = 1, lag_spec_pos = 2
    ),
    list(
      regex = "lag\\(([a-zA-Z0-9_\\.]+),\\s*([^)]+)\\)",
      var_pos = 1, lag_spec_pos = 2
    )
  )

  out <- extract_lagged_vars(equations, patterns[[1]])
  lagged_vars <- out$variables
  out <- extract_lagged_vars(out$equations, patterns[[2]])
  lagged_vars <- unique(c(lagged_vars, out$variables))

  list(variables = lagged_vars, equations = out$equations)
}

#' Extract Lagged Variables from Equations
#'
#' @description
#' This function extracts lagged variables from a given system of equations
#' using a provided pattern that defines how lagged variables are structured.
#'
#' It iterates over each equation, finds matching lag expressions, and
#' standardizes them in the form `var.L(lag)`. It also rewrites each equation
#' to expand lag expressions and returns both the modified equations and the
#' set of unique lagged variables found.
#'
#' @param equations A character vector of equations as strings.
#' @param pattern A list defining the regex pattern and capture positions for
#' variable name and lag specification. Must contain elements `regex`,
#' `var_pos`, and `lag_spec_pos`.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{variables}{Character vector of unique lagged variables extracted.}
#'   \item{equations}{Character vector of transformed equations.}
#' }
#' @keywords internal
extract_lagged_vars <- function(equations, pattern) {
  all_vars <- c()
  all_equations <- c()

  for (ix in seq_along(equations)) {
    out <- extract_from_matches(equations[ix], pattern)
    all_vars <- c(all_vars, out$variables)
    all_equations <- c(all_equations, out$equation)
  }
  list(variables = unique(all_vars), equations = all_equations)
}

parse_lag_spec <- function(lag_spec) {
  parts <- strsplit(lag_spec, ",")[[1]]
  lags <- c()
  for (part in parts) {
    part <- trimws(part)
    if (grepl(":", part)) {
      bounds <- as.integer(strsplit(part, ":")[[1]])
      lags <- c(lags, seq(bounds[1], bounds[2]))
    } else {
      lags <- c(lags, as.integer(part))
    }
  }
  unique(lags)
}

extract_from_matches <- function(equation, pattern) {
  matches <- gregexpr(pattern$regex, equation, perl = TRUE)
  raw_matches <- regmatches(equation, matches)[[1]]

  all_vars <- c()

  for (expr in raw_matches) {
    m <- regexec(pattern$regex, expr, perl = TRUE)
    parts <- regmatches(expr, m)[[1]]
    if (length(parts) < 3) next # skip if no proper match

    var_name <- parts[pattern$var_pos + 1]
    lag_spec <- parts[pattern$lag_spec_pos + 1]
    lags <- parse_lag_spec(lag_spec)

    vars <- paste0(var_name, ".L(", lags, ")")
    new_str <- paste(vars, collapse = "+")
    equation <- sub(expr, new_str, equation, fixed = TRUE)

    all_vars <- c(all_vars, vars)
  }
  # ensure type consistency
  if (length(all_vars) == 0) {
    return(list(character(0), equation = equation))
  }

  list(variables = all_vars, equation = equation)
}

extract_priors <- function(equation) {
  # only extract priors if the equation is stochastic
  if (grepl("~", equation)) {
    pattern <- "\\{([^}]+)\\}((?:[A-Za-z_][A-Za-z_0-9]*(?:\\.L\\([0-9:]+\\))?)|1)?"
    matches <- regmatches(equation, gregexpr(pattern, equation, perl = TRUE))[[1]]

    prior_vals <- sub(pattern, "\\1", matches, perl = TRUE)
    var_names <- sub(pattern, "\\2", matches, perl = TRUE)

    # Rename captured groups: "1" becomes "constant" and empty becomes "sigma"
    var_names[var_names == "1"] <- "constant"
    var_names[var_names == ""] <- "epsilon"

    priors <- lapply(prior_vals, function(p) {
      comps <- strsplit(p, ",")[[1]]
      list(as.numeric(comps[1]), as.numeric(comps[2]))
    })
    if (length(priors) == 0) {
      return(list())
    }
    names(priors) <- var_names

    # Ensure the epsilon (error term) prior ("epsilon") is the last element.
    # If a error term prior exists but is not last, remove it and warn.
    if ("epsilon" %in% names(priors) && (
      names(priors)[length(priors)] != "epsilon" ||
        sum(names(priors) == "epsilon") > 1)
    ) {
      removed <- matches[names(priors) == "epsilon"]
      cli::cli_warn(
        c(
          "x" = "Error term prior must be the last element.",
          "i" = "In equation:",
          "{equation}",
          "!" = "Removing {removed}."
        )
      )

      priors <- priors[names(priors) != "epsilon"]
    }

    priors
  } else {
    list()
  }
}

extract_settings <- function(equation) {
  equation <- trimws(equation)
  # pull out “[key=val,…]” if present
  match <- regexec("\\[(.*?)\\]", equation)
  content <- regmatches(equation, match)[[1]][2]

  if (!is.na(content) && nzchar(content)) {
    expr <- parse(text = paste0("list(", content, ")"))[[1]]
    out <- eval(expr, envir = baseenv())
  } else {
    out <- list()
  }

  out
}
