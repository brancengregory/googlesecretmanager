# R/zzz.R

# This object holds the authentication state for the package.
# It is initialized in .onLoad using gargle::init_AuthState().
.auth <- NULL

# Prevents R CMD check NOTE: Undefined global functions or variables
# when .auth is used across multiple files.
utils::globalVariables(".auth")

.onLoad <- function(libname, pkgname) {
  utils::assignInMyNamespace(
    ".auth",
    gargle::init_AuthState(package = pkgname, auth_active = TRUE)
  )

  # You can add other .onLoad configurations here if needed
  # For example, setting up options or checking dependencies
}
