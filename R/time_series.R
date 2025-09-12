#' Extended Time-Series Object
#'
#' The function \code{\link{ets}} is used to create an extended time-series
#' (ets) object. Any additional attribute passed is saved as such in the ets.
#'
#'
#' @param ... Additional attributes.
#' @inheritParams stats::ts
#' @return A koma_ts object.
#'
#' @seealso \code{\link[stats]{ts}}
#' @name koma_ts
#' @export
ets <- function(data = NA, start = NULL, end = NULL, frequency = NULL,
                deltat = NULL, ts.eps = getOption("ts.eps"), ...) {
  if (stats::is.ts(data)) {
    x <- data
  } else {
    args <- list(data = data)
    if (!is.null(start)) {
      args$start <- start
    }
    if (!is.null(end)) {
      args$end <- end
    }
    if (!is.null(frequency)) {
      args$frequency <- frequency
    }
    if (!is.null(deltat)) {
      args$deltat <- deltat
    }
    if (!missing(ts.eps)) {
      args$ts.eps <- ts.eps
    }
    x <- do.call(stats::ts, args)
  }
  new_ets(x, ...)
}

new_ets <- function(x = stats::ts(), ...) {
  UseMethod("new_ets")
}

#' @importFrom stats is.ts
new_ets.ts <- function(x = stats::ts(), ...) {
  stopifnot(stats::is.ts(x))
  args <- list(...)

  ts_obj <- x
  class(ts_obj) <- unique(c("koma_ts", class(x)))

  x_attr <- attributes(x)
  x_attr <- x_attr[names(x_attr) != "class"]
  attributes(ts_obj) <- c(attributes(ts_obj), x_attr, args)
  attr(ts_obj, "ets_attributes") <- names(args)

  ts_obj
}

#' @importFrom stats is.mts
new_ets.mts <- function(x = stats::ts(), ...) {
  stopifnot(stats::is.mts(x))

  num_series <- ncol(x)

  # Ensure type and method are vectors of the correct length
  args <- list(...)
  args <- lapply(args, function(x) {
    if (length(x) == 1) {
      rep(list(x), num_series)
    } else if (length(x) == num_series) {
      x
    } else {
      cli::cli_abort(
        c(
          "The number of elements per attribute have to match either the number
          of series or one.",
          "x" = "Error occurred for elements: {.strong {x}}",
          "i" = "Expected length was 1 or {num_series}",
          " " = "Number of elements provided: {length(x)}"
        )
      )
    }
  })

  ts_obj <- x
  class(ts_obj) <- unique(c("koma_ts", class(x)))

  x_attr <- attributes(x)
  x_attr <- x_attr[names(x_attr) != "class"]
  attributes(ts_obj) <- c(attributes(ts_obj), x_attr, args)
  attr(ts_obj, "ets_attributes") <- names(args)

  ts_obj
}

#' Convert a stats::ts object to a koma_ts object
#'
#' @param x A time series object (default is an empty time series).
#' @param ... Additional attributes.
#' @return A koma_ts object.
#' @rdname koma_ts
#' @export
as_ets <- function(x = stats::ts(), ...) {
  new_ets(x, ...)
}


#' Check if the object is a koma_ts object
#'
#' \code{\link{as_ets}} and \code{\link{is_ets}} coerce an object to a time-
#' series and test whether an object is a time series of class `koma_ts`.
#'
#' @param x An object to be coerced / checked.
#' @return TRUE if the object is of class `koma_ts`, otherwise FALSE.
#' @rdname koma_ts
#' @export
is_ets <- function(x) {
  inherits(x, "koma_ts")
}

#' Convert an mets koma_ts object to a list
#'
#' @param x An object to be converted.
#' @param ... Additional arguments.
#' @return A list with ets koma_ts objects.
#' @export
as_list <- function(x, ...) {
  UseMethod("as_list")
}

#' @export
as_list.mts <- function(x, ...) {
  # Ensure `x` is of the expected type `koma_ts`
  if (!is_ets(x)) {
    cli::cli_abort(c(
      "x" = "`x` must be of type {.val koma_ts}, but is of type
      {.val {class(x)}}.",
      "i" = "Please provide a valid `koma_ts` object for `x`."
    ))
  }

  x_attr <- get_custom_attributes(x)
  ets_attributes <- x_attr["ets_attributes"]
  x_attr["ets_attributes"] <- NULL

  out <- lapply(seq_len(ncol(x)), function(i) {
    tryCatch(
      {
        out <- x[, i]

        out <- stats::na.omit(out)
        attributes(out) <- c(
          attributes(out),
          lapply(x_attr, function(k) {
            if (length(k) >= i) {
              elem <- k[i]
              if (is.expression(elem[[1]])) {
                elem[[1]]
              } else {
                unlist(elem)
              }
            } else {
              NA
            }
          }),
          ets_attributes
        )
        class(out) <- unique(c("koma_ts", class(out)))

        # removing unnecessary attribute
        attributes(out)$na.action <- NULL
      },
      error = function(e) {
        stop(paste("Failed to process column ",
          colnames(x)[i], ": ", e,
          sep = ""
        ))
      }
    )

    out
  })

  names(out) <- colnames(x)

  out
}

#' @export
as_list.list <- function(x, ...) {
  x
}

#' Convert a list object with ets koma_ts objects to a koma_ts multivariate
#' time series (mets)
#'
#' @param x An object to be converted.
#' @param ... Additional arguments.
#' @return A koma_ts multivariate time series object.
#' @export
as_mets <- function(x, ...) {
  UseMethod("as_mets")
}

#' @export
as_mets.list <- function(x, ...) {
  rlang::check_dots_used()

  if (length(x) == 1) {
    return(as_ets(x[[1]], ...))
  }

  mts_obj <- do.call(cbind, x)
  x_attr <- lapply(x, function(k) get_custom_attributes(k))

  # Extract attribute names and check for set equality
  attr_names <- lapply(x_attr, names)
  stopifnot(
    "Provide the same attributes for each series in your list." =
      all(sapply(attr_names, function(x) setequal(x, attr_names[[1]])))
  )

  # Use lapply to extract all elements and then concatenate them
  result <- lapply(unique(unlist(lapply(x_attr, names))), function(nm) {
    unname(lapply(x_attr, `[[`, nm))
  })
  names(result) <- unique(unlist(lapply(x_attr, names)))

  ets_attributes <- unlist(unique(result[["ets_attributes"]]))
  result$ets_attributes <- ets_attributes

  attributes(mts_obj) <- c(attributes(mts_obj), result)

  class(mts_obj) <- unique(c("koma_ts", class(mts_obj)))

  mts_obj
}

#' @importFrom stats as.ts
#' @export
as.ts.koma_ts <- function(x, ...) {
  rlang::check_dots_used()

  stopifnot(is_ets(x))

  # Remove 'koma_ts' from the class attribute
  class(x) <- setdiff(class(x), "koma_ts")

  # Get custom attributes to remove them
  custom_attrs <- get_custom_attributes(x)

  # Remove custom attributes
  attributes(x)[names(custom_attrs)] <- NULL
  attr(x, "ets_attributes") <- NULL

  x
}

#' @export
as.ts.list <- function(x, ...) {
  rlang::check_dots_used()

  all_elements_are_koma_ts <- all(sapply(x, is_ets))
  if (!all_elements_are_koma_ts) {
    cli::cli_abort(c(
      "x" = "Not all elements in the list are of type {.val koma_ts}.",
      "i" = "Please ensure all list elements are of the correct type."
    ))
  }

  lapply(x, function(k) as.ts.koma_ts(k))
}

#' @importFrom stats is.ts
#' @importFrom stats is.mts
#' @export
print.koma_ts <- function(x, ...) {
  custom_attrs <- get_custom_attributes(x)
  if (!is.null(custom_attrs[[1]])) {
    attributes_x <- setdiff(attributes(x), custom_attrs)
    names(attributes_x) <- names(attributes(x))[!names(attributes(x)) %in% names(custom_attrs)]
    attributes(x) <- attributes(x)

    if (stats::is.ts(x) && !stats::is.mts(x)) {
      custom_attrs$ets_attributes <- NULL

      cat(paste(custom_attrs, collapse = ", "), "\n", sep = "")
    }
  }
  NextMethod("print")
}

#' @export
format.koma_ts <- function(x, ...) {
  NextMethod("print")
}

#' @export
`dimnames<-.koma_ts` <- function(x, value) {
  attr(x, "dimnames") <- value
  x
}

get_custom_attributes <- function(x) {
  attributes(x)[c(attr(x, "ets_attributes"), "ets_attributes")]
}

#' @export
Ops.koma_ts <- function(e1, e2 = NULL) {
  result <- NextMethod()

  # Preserve custom attributes
  custom_attrs_e1 <- c(class = list(class(e1)), get_custom_attributes(e1))

  if (!is.null(e2)) {
    custom_attrs_e2 <- c(class = list(class(e2)), get_custom_attributes(e2))
    if (inherits(e1, "koma_ts") && inherits(e2, "koma_ts")) {
      stopifnot(all.equal(custom_attrs_e1, custom_attrs_e2))
    } else if (inherits(e2, "koma_ts")) {
      custom_attrs_e1 <- custom_attrs_e2
    }
  }

  attributes(result) <- c(attributes(result), custom_attrs_e1)

  result
}

#' @export
diff.koma_ts <- function(x, ...) {
  result <- NextMethod("diff")

  custom_attrs <- c(class = list(class(x)), get_custom_attributes(x))

  # Preserve custom attributes in the result
  attributes(result) <- c(attributes(result), custom_attrs)

  class(result) <- class(x)

  result
}

#' @importFrom stats lag
#' @export
lag.koma_ts <- function(x, k = 1, ...) {
  result <- NextMethod("lag")

  custom_attrs <- c(class = list(class(x)), get_custom_attributes(x))

  # Preserve custom attributes in the result
  attributes(result) <- c(attributes(result), custom_attrs)

  class(result) <- class(x)

  # if rate update the anker
  if (!is.null(custom_attrs$anker)) {
    p <- stats::tsp(result)
    anker_date <- custom_attrs$anker[2] - (k / p[3L])
    attr(result, "anker") <- c(custom_attrs$anker[1], anker_date)
  }

  result
}

#' @importFrom stats window
#' @export
window.koma_ts <- function(x, ...) {
  result <- NextMethod("window")

  custom_attrs <- c(class = list(class(x)), get_custom_attributes(x))

  # Preserve custom attributes in the result
  attributes(result) <- c(attributes(result), custom_attrs)

  class(result) <- class(x)

  # if rate update the anker
  if (!is.null(custom_attrs$anker)) {
    x <- level(x)
    xtime <- stats::time(x)
    i <- which(xtime == stats::tsp(stats::na.omit(result))[1]) - 1

    if (length(i) == 0) {
      cli::cli_abort(c(
        "Anker update failed.",
        "x" = "No matching timestamp found for anker update."
      ))
    }

    attr(result, "anker") <- c(x[i], xtime[i])
  }

  result
}

#' @importFrom stats na.omit
#' @export
na.omit.koma_ts <- function(object, ...) {
  result <- NextMethod("na.omit")

  custom_attrs <- c(class = list(class(object)), get_custom_attributes(object))

  # Preserve custom attributes in the result
  attributes(result) <- c(attributes(result), custom_attrs)

  class(result) <- class(object)

  result
}

#' @export
cumsum.koma_ts <- function(x, ...) {
  result <- NextMethod("cumsum", x)
  attributes(result) <- attributes(x)
  result
}

#' @export
cumprod.koma_ts <- function(x, ...) {
  result <- NextMethod("cumprod", x)
  attributes(result) <- attributes(x)
  result
}

#' @importFrom tempdisagg ta
#' @export
ta.koma_ts <- function(x, conversion, to, ...) {
  result <- NextMethod("ta", x)

  custom_attrs <- c(class = list(class(x)), get_custom_attributes(x))
  # Preserve custom attributes in the result
  attributes(result) <- c(attributes(result), custom_attrs)
  class(result) <- class(x)
  result
}

#' Compute the rate of change for a time series
#'
#' Required attributes of the input time series are:
#' \itemize{
#'  \item \code{series_type} - "rate" or "level"
#'  \item \code{method} - "percentage", "diff_log", "none", or an expression
#' }
#'
#'
#' @param x A time series object. Supported classes are \code{ts} and
#' \code{ets} (a subclass of \code{ts}).
#' @param ...	arguments passed to methods (unused for the default method).
#' @return An \code{ets} object.
#'
#' @examples
#' x <- ets(1:10, series_type = "level", method = "diff_log")
#' rate(x)
#'
#' @export
rate <- function(x, ...) {
  rlang::check_dots_used()

  UseMethod("rate")
}

#' @export
rate.ts <- function(x, ...) {
  stopifnot(is_ets(x))
  type <- attr(x, "series_type")
  if (is.null(type)) {
    stop("Type cannot be NULL")
  }
  type <- match.arg(type, c("rate", "level"))

  method <- attr(x, "method")
  if (is.null(method)) {
    stop("Method cannot be NULL")
  }
  if (!is.expression(method)) {
    method <- match.arg(method, c("percentage", "diff_log", "none"))
  }

  if (type == "rate") {
    out <- x
    if (is.null(attr(out, "anker"))) {
      attr(out, "anker") <- NA
      if (!("anker" %in% attr(out, "ets_attributes"))) {
        attr(out, "ets_attributes") <- c(attr(out, "ets_attributes"), "anker")
      }
    }
  } else if (is.expression(method)) {
    out <- eval(method)
  } else {
    epsilon <- 1e-9 # Small number to replace zeros
    .fn <- switch(method,
      "diff_log" = function(x) {
        x <- ifelse(x == 0, epsilon, x)
        diff(log(x)) * 100
      },
      "percentage" = function(x) {
        x <- ifelse(x == 0, epsilon, x)
        ((x / stats::lag(x, -1)) - 1) * 100
      },
      "none" = function(x) x,
      stop("Invalid method provided")
    )
    out <- .fn(x)
    out <- stats::na.omit(out)

    if (method != "none") {
      attr(out, "anker") <- c(
        utils::head(stats::na.omit(x), 1),
        stats::tsp(stats::na.omit(x))[1]
      )
    } else {
      attr(out, "anker") <- NA
    }

    # removing unnecessary attribute
    attributes(out)$na.action <- NULL

    if (!("anker" %in% attr(out, "ets_attributes"))) {
      attr(out, "ets_attributes") <- c(attr(out, "ets_attributes"), "anker")
    }
  }
  attr(out, "series_type") <- "rate"

  out
}

#' @export
rate.list <- function(x, ...) {
  all_elements_are_koma_ts <- all(sapply(x, is_ets))
  if (!all_elements_are_koma_ts) {
    cli::cli_abort(c(
      "x" = "Not all elements in the list are of type {.val koma_ts}.",
      "i" = "Please ensure all list elements are of the correct type."
    ))
  }

  out <- lapply(seq_along(x), function(k) {
    tryCatch(
      {
        rate(x[[k]])
      },
      error = function(e) {
        name <- names(x)[k]
        stop(paste("Rate calculation failed for element:", name, "\n", e))
      }
    )
  })

  names(out) <- names(x)
  out
}

#' @export
rate.mts <- function(x, ...) {
  # Ensure `x` is of the expected type `koma_ts`
  if (!is_ets(x)) {
    cli::cli_abort(c(
      "x" = "`x` must be of type {.val koma_ts}, but is of type
      {.val {class(x)}}.",
      "i" = "Please provide a valid `koma_ts` object for `x`."
    ))
  }

  y <- as_list(x)
  z <- rate(y)
  as_mets(z)
}

#' Compute the level for a time series
#'
#' Required attributes of the input time series are:
#' \itemize{
#'  \item \code{series_type} - "rate" or "level"
#'  \item \code{method} - "percentage", "diff_log", "none", or an expression
#' }
#'
#' @param x A time series object. Supported classes are \code{ts} and
#' \code{ets} (a subclass of \code{ts}).
#' @param ...	arguments passed to methods (unused for the default method).
#' @return An \code{ets} object.
#'
#' @examples
#' x <- ets(c(0.3, 0.1, 0.2, -0.1), series_type = "rate", method = "percentage")
#' level(x)
#'
#' @export
level <- function(x, ...) {
  rlang::check_dots_used()

  UseMethod("level")
}

#' @export
level.ts <- function(x, ...) {
  stopifnot(is_ets(x))
  type <- attr(x, "series_type")
  if (is.null(type)) {
    stop("Type cannot be NULL")
  }
  type <- match.arg(type, c("rate", "level"))
  if (type == "level") {
    return(x)
  }
  method <- attr(x, "method")
  if (is.null(method)) {
    stop("Method cannot be NULL")
  }
  if (!is.expression(method)) {
    method <- match.arg(method, c(NULL, "percentage", "diff_log", "none"))
  }

  x <- stats::na.omit(x)
  if (is.expression(method)) {
    evaluate_expression <- function(expr) {
      eval(expr)
    }

    out <- evaluate_expression(method)
  } else {
    .fn <- switch(method,
      "diff_log" = function(x) exp(cumsum(x / 100)) * 100,
      "percentage" = function(x) cumprod(1 + x / 100) * 100,
      "none" = function(x) x,
      method
    )
    out <- .fn(x)
    if (method != "none") {
      out <- ets(
        c(100, out),
        end = stats::end(out),
        frequency = stats::frequency(out)
      )
    }

    attributes(out) <- c(attributes(out), get_custom_attributes(x))
    attr(out, "series_type") <- "level"
    if (!is.null(attr(out, "anker"))) {
      anker <- attr(out, "anker")
      if (!is.na(anker[1]) && method != "none") {
        stopifnot(is.numeric(anker[1]))
        out <- anker[1] / 100 * out

        if (anker[2] != stats::tsp(out)[1]) {
          cli::cli_abort(c(
            "Anker date mismatch:",
            "x" = "The anker date {.strong {anker[2]}} does not match the
          time series start date {.strong {stats::tsp(out)[1]}}.",
            "i" = "Ensure the anker date aligns with the start date of the
          time series."
          ))
        }
      }
      attr(out, "anker") <- NULL
      ets_attr <- attr(out, "ets_attributes")
      # remove anker string from ets_attr
      attr(out, "ets_attributes") <- ets_attr[ets_attr != "anker"]
    }
  }
  out
}

#' @export
level.mts <- function(x, ...) {
  # Ensure `x` is of the expected type `koma_ts`
  if (!is_ets(x)) {
    cli::cli_abort(c(
      "x" = "`x` must be of type {.val koma_ts}, but is of type
      {.val {class(x)}}.",
      "i" = "Please provide a valid `koma_ts` object for `x`."
    ))
  }

  y <- as_list(x)
  z <- level(y)
  as_mets(z)
}

#' @export
level.list <- function(x, ...) {
  all_elements_are_koma_ts <- all(sapply(x, is_ets))
  if (!all_elements_are_koma_ts) {
    cli::cli_abort(c(
      "x" = "Not all elements in the list are of type {.val koma_ts}.",
      "i" = "Please ensure all list elements are of the correct type."
    ))
  }

  mapply(function(k, name) {
    tryCatch(
      {
        level(k)
      },
      error = function(e) {
        cli::cli_abort(c(
          "x" = "Level calculation failed for element: {.val {name}}.",
          "!" = "{e$message}"
        ))
      }
    )
  }, x, names(x), SIMPLIFY = FALSE)
}

#' Concatenate two time series
#'
#' @param x A `koma_ts` object.
#' @param y A `koma_ts` object to be concatenated to x.
#' @param ...	arguments passed to methods (unused for the default method).
#' @export
concat <- function(x, y, ...) {
  rlang::check_dots_used()

  UseMethod("concat")
}

#' @export
concat.ts <- function(x, y, ...) {
  # Ensure both x and y are of type 'koma_ts'
  if (!inherits(x, "koma_ts")) {
    cli::cli_abort(c(
      "x" = "`x` is not of type {.val koma_ts}, but of type {.val {class(x)}}.",
      "i" = "Please provide a time series of type {.val koma_ts} for `x`."
    ))
  }
  if (!inherits(y, "koma_ts")) {
    cli::cli_abort(c(
      "x" = "`y` is not of type {.val koma_ts}, but of type {.val {class(y)}}.",
      "i" = "Please provide a time series of type {.val koma_ts} for `y`."
    ))
  }

  x <- level(stats::na.omit(x))
  y <- level(stats::na.omit(y))

  custom_attrs_x <- get_custom_attributes(x)
  custom_attrs_y <- get_custom_attributes(y)
  stopifnot(identical(custom_attrs_x, custom_attrs_y))

  ts_diff <- ets(
    start = stats::end(x), end = stats::start(y),
    frequency = stats::frequency(x)
  )

  ts_diff[1] <- utils::tail(x, 1)
  ts_diff[length(ts_diff)] <- utils::head(y, 1)

  if (length(ts_diff) == 1) {
    stopifnot(all.equal(utils::tail(x, 1), utils::head(y, 1)))
  }

  if (any(is.na(ts_diff))) {
    cli::cli_warn(c(
      "!" = "Time series did not align correctly.",
      "i" = "A time series with `NA` values was created to fill gaps."
    ))
  }

  out <- ets(
    c(
      x[-length(x)],
      ts_diff,
      y[-1]
    ),
    start = stats::start(x), frequency = stats::frequency(x)
  )
  attributes(out) <- c(attributes(out), custom_attrs_x)
  out
}

#' @export
concat.list <- function(x, y, ...) {
  stopifnot(inherits(x, "list"), inherits(y, "list"))

  all_elements_are_koma_ts <- all(sapply(x, is_ets), sapply(y, is_ets))
  if (!all_elements_are_koma_ts) {
    cli::cli_abort(c(
      "x" = "Not all elements in the list are of type {.val koma_ts}.",
      "i" = "Ensure that each element in both `x` and `y` is a valid
      time series of type {.val koma_ts}."
    ))
  }

  if (!all(names(x) %in% names(y))) {
    missing_names <- setdiff(names(x), names(y))
    cli::cli_abort(c(
      "x" = "Not all elements in `x` are present in `y`.",
      "i" = "The following elements are missing in `y`:
      {.val {missing_names}}.",
      "i" = "Please ensure that all time series in `x` are available in `y`."
    ))
  }

  out <- lapply(names(x), function(k) concat(x[[k]], y[[k]]))
  names(out) <- names(x)
  out
}

#' @export
concat.mts <- function(x, y, ...) {
  # Ensure `x` is of the expected type `koma_ts`
  if (!is_ets(x)) {
    cli::cli_abort(c(
      "x" = "`x` must be of type {.val koma_ts}, but is of type
      {.val {class(x)}}.",
      "i" = "Please provide a valid `koma_ts` object for `x`."
    ))
  }

  x <- as_list(x)
  y <- as_list(y)
  out <- concat.list(x, y, ...)
  as_mets(out)
}


#' Rebase Time Series Data Relative to a Base Period
#'
#' Calculates a rebased time series using a given date range as the base
#' period. The base period average is set to 100, and all other values are
#' scaled accordingly.
#'
#' @param x An ets object.
#' @inheritParams stats::ts
#' @param ...	arguments passed to methods (unused for the default method).
#' @return An ets object with the level computed.
#' @export
rebase <- function(x, start, end, ...) {
  rlang::check_dots_used()

  UseMethod("rebase")
}

#' @export
rebase.ts <- function(x, start, end, ...) {
  base <- as.numeric(mean(stats::window(x, start = start, end = end)))
  x / base * 100
}

#' @export
rebase.list <- function(x, start, end, ...) {
  stopifnot(inherits(x, "list"))

  all_elements_are_koma_ts <- all(sapply(x, is_ets))
  if (!all_elements_are_koma_ts) {
    cli::cli_abort(c(
      "Elements of list {.var x} must be of class koma_ts.",
      "i" = "Ensure that each element in the list is a valid 'koma_ts' object."
    ))
  }

  lapply(x, function(k) rebase(k, start, end))
}

#' @export
rebase.mts <- function(x, start, end, ...) {
  # Ensure `x` is of the expected type `koma_ts`
  if (!is_ets(x)) {
    cli::cli_abort(c(
      "x" = "`x` must be of type {.val koma_ts}, but is of type
      {.val {class(x)}}.",
      "i" = "Please provide a valid `koma_ts` object for `x`."
    ))
  }

  x <- as_list(x)
  out <- rebase.list(x, start, end, ...)
  as_mets(out)
}


#' Get the type of a koma_ts object
#'
#' @param x A koma_ts object.
#' @param type The type to filter by.
#' @param var An optional variable to filter by.
#' @param ...	arguments passed to methods (unused for the default method).
#' @return A koma_ts object with the specified type.
#' @export
type <- function(x, type, var = NULL, ...) {
  rlang::check_dots_used()

  UseMethod("type")
}

#' @export
type.mts <- function(x, type, var = NULL, ...) {
  # Ensure `x` is of the expected type `koma_ts`
  if (!is_ets(x)) {
    cli::cli_abort(c(
      "x" = "`x` must be of type {.val koma_ts}, but is of type
      {.val {class(x)}}.",
      "i" = "Please provide a valid `koma_ts` object for `x`."
    ))
  }

  y <- as_list(x)

  type(y, type, var)
}

#' @export
type.list <- function(x, type, var = NULL, ...) {
  if (length(type) != 1) {
    cli::cli_abort(c(
      "x" = "'type' must be a single value, not a vector.",
      "i" = "You provided a vector of length {length(type)}.",
      "i" = "Please ensure that 'type' is a single value."
    ))
  }

  y <- x[
    sapply(x, function(series) attr(series, "series_type") == type)
  ]
  z <- x[
    sapply(x, function(series) attr(series, "value_type") == type)
  ]
  result <- c(y, z)

  # If var is provided, filter nominal series for that specific variable
  if (!is.null(var)) {
    result <- result[sapply(names(result), function(series) series %in% var)]
  }

  result
}
