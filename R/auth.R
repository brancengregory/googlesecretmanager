# Environment to store the token
.auth_env <- new.env(parent = emptyenv())

# Default scopes for Secret Manager
SECRETMANAGER_SCOPES <- "https://www.googleapis.com/auth/cloud-platform"

#' Authenticate with Google Secret Manager
#'
#' This function handles authentication with Google Cloud Platform, specifically
#' for accessing Secret Manager. It uses `gargle` for token management.
#' Authentication will try to find a token using `gargle::token_fetch()`,
#' which supports various methods like service account keys (via `path` argument
#' or the `GARGLER_SERVICE_JSON` environment variable), Application Default
#' Credentials (ADC), or interactive OAuth flows if run in an interactive session.
#'
#' @param path Optional. Path to a service account JSON key file. If `NULL`,
#'   `gargle` will attempt to find credentials via other methods (e.g., ADC,
#'   environment variables, or OAuth flow).
#' @param scopes The OAuth2 scopes to request. Defaults to the necessary scope
#'   for Secret Manager and Cloud Platform read-only access.
#' @param ... Additional arguments passed to `gargle::token_fetch()`.
#' @return A `gargle::Token2.0` object, invisibly. The token is also cached
#'   within the package environment for subsequent calls.
#' @export
#' @examples
#' \dontrun{
#'   # Attempt authentication using Application Default Credentials or other
#'   # gargle-supported methods (e.g., GCE metadata server, Cloud SDK config)
#'   secretmanager_auth()
#'
#'   # Using a service account key file
#'   # secretmanager_auth(path = "/path/to/your/service-account-key.json")
#'
#'   # Using a specific email for OAuth flow (if multiple accounts are available)
#'   # secretmanager_auth(email = "your-email@example.com")
#' }
secretmanager_auth <- function(path = NULL, scopes = SECRETMANAGER_SCOPES, ...) {
  cred <- gargle::token_fetch(
    scopes = scopes,
    path = path,
    ...
  )
  .auth_env$token <- cred
  invisible(.auth_env$token)
}

#' Get the current authentication token
#'
#' Retrieves the token stored by `secretmanager_auth()`. If no token is found
#' or the token is invalid, it attempts to authenticate using default settings.
#'
#' @return A `gargle::Token2.0` object.
#' @keywords internal
get_sm_token <- function() {
  if (is.null(.auth_env$token) || !gargle::token_has_cred(.auth_env$token)) {
    message("No valid token found or token expired. Attempting to authenticate with default settings...")
    secretmanager_auth()
  }
  if (is.null(.auth_env$token) || !gargle::token_has_cred(.auth_env$token)) {
    stop(
      "Authentication failed or no token available. ",
      "Please run `secretmanager_auth()` explicitly with appropriate parameters (e.g., path to service account key).",
      call. = FALSE
    )
  }
  .auth_env$token
}

#' Clear Cached Authentication Token
#'
#' Removes the cached Google Cloud authentication token from the package
#' environment. Subsequent API calls will require re-authentication.
#'
#' @export
#' @examples
#' \dontrun{
#'   secretmanager_auth_clear()
#'   # Next call to get_secret_payload() will trigger re-authentication
#' }
secretmanager_auth_clear <- function() {
  if (exists("token", envir = .auth_env)) {
    rm("token", envir = .auth_env)
  }
  invisible(NULL)
}
