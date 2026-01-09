#' Validate Equations
#'
#' This function validates a character vector of equations, ensuring that they
#' adhere to a specific format.
#' The equations should be in the format "left_variable==right_variables", where
#' right variables can be separated by '+', '-', or '*'. The function checks for
#' valid equation structure, valid variable names, no duplicate regressors in a
#' single equation, and no duplicate dependent variables across all equations.
#'
#' @return Returns object.
#' @keywords internal
validate_system_of_equations <- function(x) {
  stopifnot(is_system_of_equations(x))

  equations <- x$equations
  endogenous_variables <- x$endogenous_variables
  exogenous_variables <- x$exogenous_variables

  # Check if all exogenous variables have been declared
  variables <- get_variables(equations)
  variables <- unlist(variables)

  if (any(exogenous_variables %in% endogenous_variables)) {
    stop(
      "An exogenous variable cannot also be endogenous: ",
      paste(
        exogenous_variables[exogenous_variables %in% endogenous_variables],
        collapse = ", "
      )
    )
  }

  if (length(grep("~", equations)) == 0) {
    cli::cli_abort(c(
      "No stochastic equation detected in the system of equations.",
      "i" = "Ensure that at least one equation uses the '~' syntax to ",
      "indicate a stochastic relationship.",
      "For example: 'Y ~ X'."
    ))
  }

  # Check for duplicate endogenous or exogenous
  duplicated_endogenous <- endogenous_variables[duplicated(endogenous_variables)]
  if (length(duplicated_endogenous) > 0) {
    cli::cli_abort(c(
      "Declared endogenous variables are not unique.",
      "x" = paste(duplicated_endogenous, collapse = ", "),
      "i" = "Ensure that each endogenous variable is declared only once."
    ))
  }

  duplicated_exogenous <- exogenous_variables[duplicated(exogenous_variables)]
  if (length(duplicated_exogenous) > 0) {
    stop(
      "Declared exogenous variables are not unique. Duplicates: ",
      paste(unique(duplicated_exogenous), collapse = ", ")
    )
  }

  # Check for duplicate dependent variables
  left_vars_all <- unlist(lapply(
    equations,
    function(equation) unlist(strsplit(equation, "=="))[1]
  ))
  if (length(left_vars_all) != length(unique(left_vars_all))) {
    stop("Duplicate dependent variables found.")
  }

  return(TRUE)
}

#' Validate Individual Equation
#'
#' This function validates an individual equation to ensure that it follows a
#' specific structure. The expected format for the equation is "left_variable ==
#' right_variables", where `right_variables` can be a combination of variables
#' separated by '+', '-', or '*'. The function checks the following:
#' - Validity of the variable names.
#' - Correct structure (exactly one '==' separator).
#' - Stochastic error term `epsilon` must be the last element.
#' - Duplicate regressors within a single equation are not allowed.
#'
#' @param equation A character string representing an equation in the format
#' "left_variable == right_variables".
#'
#' @return Logical. Returns `TRUE` if the equation is valid.
#' Throws an error with a specific message if any checks fail.
#' @keywords internal
validate_equation <- function(equation) {
  parts <- unlist(strsplit(equation, "==|~"))
  if (length(parts) != 2) {
    cli::cli_abort("Invalid equation structure: {equation}")
  }

  error_msg <- function(var, msg) {
    cli::cli_abort(c(
      "!" = "In Equation: {equation}. Invalid variable: {var}.",
      "i" = "Reason: {msg}"
    ))
  }

  left_var <- trimws(parts[1])
  if (!is_valid_var(left_var)) {
    error_msg(left_var, "Left side variable is invalid")
  }

  right_vars <- strsplit(parts[2], "[\\+\\-]")[[1]]
  right_vars <- lapply(right_vars, function(v) {
    list(
      original = trimws(v),
      split = unlist(strsplit(v, "[\\*]"))
    )
  })

  lapply(right_vars, function(rv) {
    validate_var_term(rv, error_msg)
  })

  canon <- sapply(right_vars, function(rv) {
    gsub("\\s+", "", rv$original)
  })
  if (length(unique(canon)) < length(canon)) {
    cli::cli_abort("Duplicate regressors found in: {equation}")
  }

  TRUE
}

validate_var_term <- function(rv, error_msg) {
  split <- rv$split
  n_split <- length(split)

  if (n_split == 0) {
    return(TRUE)
  } else if (n_split == 1) {
    if (rv$original %in% c("1", "0")) {
      return(invisible(TRUE))
    }
    if (!is_valid_var(split)) {
      error_msg(rv$original, "Invalid single component variable")
    }
  } else if (n_split == 2) {
    valid_expr <- grepl("^\\(.*\\)$", split[1])
    valid_num <- grepl("^[0-9]+(?:\\.[0-9]+)?$", split[1])
    if (!(valid_expr || valid_num)) {
      error_msg(
        rv$original,
        "Weight must be a number or expression in parentheses"
      )
    }
    if (!is_valid_var(split[2])) {
      error_msg(
        rv$original,
        "Invalid variable name in weighted term"
      )
    }
  } else {
    error_msg(
      rv$original,
      "Invalid structure: too many components separated by '*'"
    )
  }
}

is_valid_var <- function(name) {
  # Remove weight and prior syntax
  name <- gsub("\\([^)]*\\)\\*", "", name)
  name <- gsub("\\{[^}]*\\}", "", name)

  # Pattern for standard syntax: var, var.L(1), var[1:4]
  pattern1 <- "^[a-zA-Z][a-zA-Z0-9_]*((\\.L\\([0-9:,]+\\))|"
  pattern1 <- paste0(pattern1, "(\\[[0-9:,]+\\]))?$")

  # Pattern for lag() syntax, e.g. lag(investment,2:3)
  pattern2 <- "^lag\\([[:space:]]*[a-zA-Z][a-zA-Z0-9_]*[[:space:]]*,[[:space:]]*"
  pattern2 <- paste0(pattern2, "[0-9]+(:[0-9]+)?[[:space:]]*\\)$")

  valid1 <- grepl(pattern1, name)
  valid2 <- grepl(pattern2, name)
  not_number <- !grepl("^[0-9]+$", name)

  (valid1 | valid2) & not_number
}

#' Validate Completeness
#'
#' The function checks if all exogenous variables are declared.
#'
#' @inheritParams system_of_equations
#'
#' @return Logical. Returns `TRUE` if the equation is valid.
#' Throws an error with a specific message if any checks fail.
validate_completeness <- function(equations, exogenous_variables) {
  # Check if all exogenous variables have been declared
  variables <- get_variables(equations)
  variables <- unlist(variables)

  get_base_variable <- function(var) {
    var <- trimws(var)
    if (grepl("^lag\\(", var)) {
      return(sub(
        "^lag\\s*\\(\\s*([a-zA-Z][a-zA-Z0-9_]*)\\s*,.*",
        "\\1", var
      ))
    } else if (grepl("\\.L\\(", var)) {
      return(sub("^(.*?)\\.L\\(.*", "\\1", var))
    } else if (grepl("\\[[0-9:,]+\\]$", var)) {
      return(sub("^(.*?)\\[[0-9:,]+\\]$", "\\1", var))
    } else {
      return(var)
    }
  }

  variables <- sapply(variables, get_base_variable)

  missing_variables <- setdiff(
    variables,
    c(
      "constant",
      get_endogenous_variables(equations),
      exogenous_variables
    )
  )
  # Variables in parentheses are used in weight calculation
  # Remove all variables in parentheses from missing
  missing_variables <-
    missing_variables[!grepl("^\\(.*\\)$", missing_variables)]
  # Remove prior syntax
  missing_variables <-
    missing_variables[!grepl("^\\{[0-9.,]*\\}", missing_variables)]
  # Remove lag
  missing_variables <- missing_variables[
    !grepl("\\.L\\(", missing_variables) &
      !grepl("^lag\\(", missing_variables) &
      !grepl("\\[[0-9:,]+\\]$", missing_variables)
  ]

  if (length(missing_variables) != 0) {
    cli::cli_abort(c(
      "Undeclared exogenous variables detected:",
      stats::setNames(missing_variables, rep("x", length(missing_variables)))
    ))
  }

  # Check if there are redundant exogenous variables
  redundant <- setdiff(
    exogenous_variables,
    variables
  )

  if (length(redundant) != 0) {
    cli::cli_abort(c(
      "Redundant exogenous variables detected:",
      stats::setNames(redundant, rep("x", length(redundant)))
    ))
  }
}

validate_priors <- function(equation) {
  variables <- get_variables(equation)[[1]]
  dependent_variable <- variables[1]
  variables <- variables[2:length(variables)]

  # extract priors and variable names
  priors <- ifelse(
    grepl("\\{[0-9]+(\\.[0-9]*)?,[0-9]+(\\.[0-9]*)?\\}", variables),
    sub(".*(\\{[0-9]+(\\.[0-9]*)?,[0-9]+(\\.[0-9]*)?\\}).*", "\\1", variables),
    # Keep "" if no prior exists, else FALSE
    ifelse(grepl("\\{", variables), FALSE, "")
  )

  # Check for invalid priors and print warnings/errors using cli
  if (any(priors == FALSE)) {
    invalid_vars <- variables[priors == FALSE]
    invalid_vars <- gsub("\\{", "{{", invalid_vars)
    invalid_vars <- gsub("\\}", "}}", invalid_vars)

    cli::cli_abort(c(
      "!" = "Invalid priors detected for these variables:",
      "x" = invalid_vars
    ))
  }

  # Check if the dependent variable contains invalid characters
  if (grepl("[{}()]", dependent_variable)) {
    dependent_variable <- gsub("\\{", "{{", dependent_variable)
    dependent_variable <- gsub("\\}", "}}", dependent_variable)
    cli::cli_abort(c(
      "Invalid dependent variable.",
      "!" = paste0(
        "Dependent variable '", dependent_variable,
        "' must not include {{}} or ()."
      ),
      "i" = "Ensure the name is plain without special characters."
    ))
  }
}
