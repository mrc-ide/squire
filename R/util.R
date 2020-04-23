#' @noRd
`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

#' @noRd
squire_file <- function(path) {
  system.file(path, package = "squire", mustWork = TRUE)
}
