.onLoad <- function(libname, pkgname) {
  methods::setOldClass("koma_estimate")
  if (requireNamespace("texreg", quietly = TRUE)) {
    methods::setMethod(f = texreg::extract, "koma_estimate", extract.koma_estimate)
  }
}
