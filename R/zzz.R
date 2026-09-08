## Package options that can be set via environment variables when the
## package is loaded, e.g. R_LMODWEB_DATAPATH sets option 'lmodweb.datapath'.
.onLoad <- function(libname, pkgname) {
  update_package_option("lmodweb.datapath", envvar = "R_LMODWEB_DATAPATH")
}

update_package_option <- function(name, envvar, default = NULL) {
  ## An R option already set takes precedence
  value <- getOption(name, NULL)
  if (is.null(value)) {
    value <- Sys.getenv(envvar, NA_character_)
    if (is.na(value)) value <- default
  }
  if (is.null(value)) return(invisible(NULL))
  args <- list(value)
  names(args) <- name
  do.call(options, args = args)
  invisible(value)
}
