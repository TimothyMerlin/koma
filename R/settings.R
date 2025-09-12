build_settings <- function(default, settings, equation_settings) {
  if (!is.null(settings)) {
    common <- intersect(names(settings), names(default))
    settings <- settings[common]
    default <- utils::modifyList(default, settings)
  }

  out <- lapply(equation_settings, function(x) {
    common <- intersect(names(x), names(default))
    utils::modifyList(default, x[common])
  })

  out
}
