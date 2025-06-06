# R/zzz.R

.sm_pkg_env <- new.env(parent = emptyenv())

.sm_pkg_env$current_project <- NULL

# This object holds the authentication state for the package.
# It is initialized in .onLoad using gargle::init_AuthState().
.sm_auth <- NULL

# Endpoints
.sm_endpoints <- NULL

# Prevents R CMD check NOTE: Undefined global functions or variables
# when .auth is used across multiple files.
utils::globalVariables(c(".sm_auth", ".sm_endpoints", ".sm_pkg_env"))

.onLoad <- function(libname, pkgname) {
  utils::assignInMyNamespace(
    ".sm_auth",
    gargle::init_AuthState(package = pkgname, auth_active = TRUE)
  )

  if (file.exists(system.file("R/sysdata.rda", package = pkgname))) {
    load(system.file("R/sysdata.rda", package = pkgname), envir = as.environment(topenv()))
  }
}
