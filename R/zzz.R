# R/zzz.R

# This object holds the authentication state for the package.
# It is initialized in .onLoad using gargle::init_AuthState().
.auth <- NULL

# Prevents R CMD check NOTE: Undefined global functions or variables
# when .auth is used across multiple files.
utils::globalVariables(c(".auth", ".endpoints"))

.endpoints <- NULL

.onLoad <- function(libname, pkgname) {
  utils::assignInMyNamespace(
    ".auth",
    gargle::init_AuthState(package = pkgname, auth_active = TRUE)
  )

  if (file.exists(system.file("R/sysdata.rda", package = pkgname))) {
    load(system.file("R/sysdata.rda", package = pkgname), envir = as.environment(topenv()))
  }
}
