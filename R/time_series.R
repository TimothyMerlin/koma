#' Extended Time-Series Object
#'
#' The function \code{\link{ets}} is used to create an extended time-series
#' (ets) object. Any additional attribute passed is saved as such in the ets.
#'
#' Mixed arithmetic between \code{koma_ts} and plain \code{ts} objects currently
#' follows base R's group generic dispatch and may emit an
#' "Incompatible methods" warning even when the resulting values are valid.
#' Coercing both operands to \code{koma_ts} avoids that warning.
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
        if (length(x_attr)) {
          restored_attrs <- lapply(names(x_attr), function(attr_name) {
            values <- x_attr[[attr_name]]
            if (length(values) >= i) {
              elem <- values[i]
              if (is.expression(elem[[1]])) {
                elem[[1]]
              } else {
                unlist(elem)
              }
            } else {
              NA
            }
          })
          names(restored_attrs) <- names(x_attr)
          restored_attrs$ets_attributes <- names(restored_attrs)
          out <- restore_koma_attrs(out, restored_attrs)
        } else {
          class(out) <- unique(c("koma_ts", class(out)))
        }

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

# Internal registry for attribute-specific metadata policies
if (is.null(the$koma_attr_policies)) {
  the$koma_attr_policies <- list()
}

get_koma_attr_policy <- function(attr) {
  the$koma_attr_policies[[attr]]
}

default_koma_attr_merge <- function(left, right, attr, op = NULL, template = NULL) {
  if (is.null(left)) {
    return(right)
  }
  if (is.null(right)) {
    return(left)
  }
  if (isTRUE(all.equal(left, right))) {
    return(left)
  }

  cli::cli_abort(c(
    "Cannot merge koma_ts attribute {.field {attr}}.",
    "x" = "The attribute values differ for operation {.val {if (is.null(op)) 'unknown' else op}}.",
    "i" = "Register a policy with {.fn set_koma_attr_policy} for this attribute."
  ))
}

align_ts_metadata <- function(x, template) {
  if (is.null(x) || !stats::is.ts(x)) {
    return(x)
  }

  aligned <- suppressWarnings(stats::window(
    x,
    start = stats::start(template),
    end = stats::end(template),
    extend = TRUE
  ))

  if (!isTRUE(all.equal(stats::tsp(aligned), stats::tsp(template)))) {
    aligned <- stats::ts(
      as.vector(aligned),
      start = stats::start(template),
      frequency = stats::frequency(template)
    )
  }

  aligned
}

collect_koma_attrs <- function(x) {
  if (!is_ets(x)) {
    return(list())
  }

  get_custom_attributes(x)
}

transform_koma_attrs <- function(attrs, op, template = NULL, ...) {
  attr_names <- setdiff(names(attrs), "ets_attributes")
  transformed <- lapply(attr_names, function(attr_name) {
    value <- attrs[[attr_name]]

    policy <- get_koma_attr_policy(attr_name)
    handler <- if (is.null(policy)) NULL else policy[[op]]
    if (is.null(handler)) {
      value
    } else {
      handler(
        value = value,
        attr = attr_name,
        template = template,
        ...
      )
    }
  })
  names(transformed) <- attr_names

  transformed$ets_attributes <- names(transformed)
  transformed
}

apply_koma_attrs <- function(x, attrs) {
  if (!length(attrs)) {
    return(x)
  }

  attrs$class <- NULL
  attr_names <- setdiff(names(attrs), "ets_attributes")
  attrs$ets_attributes <- attr_names

  class(x) <- unique(c("koma_ts", class(x)))
  attributes(x) <- c(attributes(x), attrs)
  x
}

rebuild_ts_like <- function(result, template) {
  if (stats::is.ts(result)) {
    return(result)
  }

  if (stats::is.mts(template)) {
    dims <- dim(template)
    dim(result) <- dims
    result <- stats::ts(
      result,
      start = stats::start(template),
      frequency = stats::frequency(template)
    )
    colnames(result) <- colnames(template)
    return(result)
  }

  stats::ts(
    as.vector(result),
    start = stats::start(template),
    frequency = stats::frequency(template)
  )
}

#' Register metadata behavior for a koma_ts attribute
#'
#' @param attr Name of the attribute.
#' @param merge Optional binary merge handler with signature
#'   `function(left, right, attr, op = NULL, template = NULL)`.
#' @param lag Optional lag handler with signature
#'   `function(value, attr, template = NULL, ...)`.
#' @param window Optional window handler with signature
#'   `function(value, attr, template = NULL, ...)`.
#' @param na_omit Optional `na.omit` handler with signature
#'   `function(value, attr, template = NULL, ...)`.
#' @return The registered policy, invisibly.
#' @export
set_koma_attr_policy <- function(attr, merge = NULL, lag = NULL, window = NULL, na_omit = NULL) {
  stopifnot(is.character(attr), length(attr) == 1L, nzchar(attr))
  handlers <- list(merge = merge, lag = lag, window = window, na_omit = na_omit)
  non_null_handlers <- Filter(Negate(is.null), handlers)
  stopifnot(all(vapply(non_null_handlers, is.function, logical(1))))

  the$koma_attr_policies[[attr]] <- handlers

  invisible(handlers)
}

#' Align a koma_ts attribute or metadata series to another time series
#'
#' @param x A `koma_ts` object or metadata series.
#' @param attr Name of the attribute to align. Only used when `x` is `koma_ts`.
#' @param template A time series whose `tsp` should be used.
#' @return The aligned attribute value.
#' @export
align_koma_attr <- function(x, attr = NULL, template) {
  stopifnot(stats::is.ts(template))

  value <- if (is_ets(x)) {
    stopifnot(is.character(attr), length(attr) == 1L)
    attr(x, attr)
  } else {
    x
  }

  align_ts_metadata(value, template)
}

#' Merge custom metadata from two koma_ts objects
#'
#' @param e1 First operand.
#' @param e2 Second operand.
#' @param op Name of the operation.
#' @param template Result template used for alignment.
#' @return A named list of merged custom attributes.
#' @export
merge_koma_attrs <- function(e1, e2 = NULL, op = NULL, template = NULL) {
  attrs_e1 <- collect_koma_attrs(e1)
  attrs_e2 <- collect_koma_attrs(e2)

  attr_names <- union(
    setdiff(names(attrs_e1), "ets_attributes"),
    setdiff(names(attrs_e2), "ets_attributes")
  )

  merged <- lapply(attr_names, function(attr_name) {
    policy <- get_koma_attr_policy(attr_name)
    merge_handler <- if (is.null(policy)) NULL else policy$merge
    if (is.null(merge_handler)) {
      merge_handler <- default_koma_attr_merge
    }

    merge_handler(
      left = attrs_e1[[attr_name]],
      right = attrs_e2[[attr_name]],
      attr = attr_name,
      op = op,
      template = template
    )
  })
  names(merged) <- attr_names

  merged$ets_attributes <- attr_names
  merged
}

#' Remove custom metadata from a koma_ts object
#'
#' @param x A time series object.
#' @return A plain `ts` object when `x` is `koma_ts`, otherwise `x`.
#' @export
strip_koma_attrs <- function(x) {
  if (!is_ets(x)) {
    return(x)
  }

  as.ts.koma_ts(x)
}

#' Restore custom metadata onto a time series
#'
#' @param x A time series object.
#' @param attrs A named list of custom attributes.
#' @return A `koma_ts` object.
#' @export
restore_koma_attrs <- function(x, attrs) {
  stopifnot(stats::is.ts(x), is.list(attrs))

  apply_koma_attrs(x, attrs)
}

#' @importFrom stats is.ts
#' @importFrom stats is.mts
#' @export
print.koma_ts <- function(x, ...) {
  custom_attrs <- get_custom_attributes(x)
  attr_names <- setdiff(names(custom_attrs), "ets_attributes")

  format_attr_value <- function(value) {
    if (stats::is.ts(value)) {
      descriptor <- paste(class(as.vector(value))[1], "[", length(value), "obs]")
      if (is.logical(as.vector(value))) {
        descriptor <- paste(descriptor, paste(sum(value, na.rm = TRUE), "TRUE"))
      }
      return(paste("ts", descriptor))
    }
    if (is.list(value) && !is.expression(value)) {
      return(paste0("list[", length(value), "]"))
    }
    paste(
      utils::capture.output(utils::str(value, give.attr = FALSE, vec.len = 4)),
      collapse = " "
    )
  }

  cat("<koma_ts>\n")
  if (length(attr_names)) {
    cat("attributes:\n")
    for (attr_name in attr_names) {
      cat("  ", attr_name, ": ", format_attr_value(custom_attrs[[attr_name]]), "\n", sep = "")
    }
    cat("\n")
  }
  cat("series:\n")
  print(strip_koma_attrs(x), ...)

  invisible(x)
}

#' @export
format.koma_ts <- function(x, ...) {
  format(strip_koma_attrs(x), ...)
}

#' @export
`dimnames<-.koma_ts` <- function(x, value) {
  attr(x, "dimnames") <- value
  x
}

get_custom_attributes <- function(x) {
  attr_names <- attr(x, "ets_attributes")
  if (is.null(attr_names)) {
    return(list())
  }

  attributes(x)[unique(c(attr_names, "ets_attributes"))]
}

#' @export
Ops.koma_ts <- function(e1, e2 = NULL) {
  lhs <- strip_koma_attrs(e1)
  rhs <- strip_koma_attrs(e2)

  result <- if (is.null(e2)) {
    do.call(.Generic, list(lhs))
  } else {
    do.call(.Generic, list(lhs, rhs))
  }

  merged_attrs <- merge_koma_attrs(e1, e2, op = .Generic, template = result)
  restore_koma_attrs(result, merged_attrs)
}

#' @export
diff.koma_ts <- function(x, ...) {
  result <- NextMethod("diff")

  restore_koma_attrs(result, get_custom_attributes(x))
}

#' @importFrom stats lag
#' @export
lag.koma_ts <- function(x, k = 1, ...) {
  result <- NextMethod("lag")

  custom_attrs <- transform_koma_attrs(
    get_custom_attributes(x),
    op = "lag",
    template = result,
    k = k,
    ...
  )

  result <- restore_koma_attrs(result, custom_attrs)

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

  custom_attrs <- transform_koma_attrs(
    get_custom_attributes(x),
    op = "window",
    template = result,
    ...
  )

  result <- restore_koma_attrs(result, custom_attrs)

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

  custom_attrs <- transform_koma_attrs(
    get_custom_attributes(object),
    op = "na_omit",
    template = result,
    ...
  )

  restore_koma_attrs(result, custom_attrs)
}

#' @export
cumsum.koma_ts <- function(x, ...) {
  result <- NextMethod("cumsum", x)
  result <- rebuild_ts_like(result, x)
  restore_koma_attrs(result, get_custom_attributes(x))
}

#' @export
cumprod.koma_ts <- function(x, ...) {
  result <- NextMethod("cumprod", x)
  result <- rebuild_ts_like(result, x)
  restore_koma_attrs(result, get_custom_attributes(x))
}

#' @importFrom tempdisagg ta
#' @export
ta.koma_ts <- function(x, conversion, to, ...) {
  result <- NextMethod("ta", x)

  restore_koma_attrs(result, get_custom_attributes(x))
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
