# R/auth.R

#' @importFrom gargle gargle_oauth_email gargle_oauth_cache gargle_oob_default
#' @importFrom gargle check_is_service_account token_fetch
#' @importFrom gargle gargle_oauth_client_from_json
#' @importFrom gargle token_email
#' @importFrom httr Token2.0 config
#' @importFrom lifecycle deprecate_warn
#' @importFrom utils assignInMyNamespace globalVariables
NULL


#' Authenticate with Google Secret Manager
#'
#' @description
#' This function handles authentication with Google Cloud Secret Manager. It's
#' called automatically when the first token is needed, or it can be called
#' directly by the user to pre-authenticate or to switch identities, scopes,
#' or authentication methods (e.g., user OAuth, service account token).
#'
#' Following `gargle` best practices for sensitive APIs, `secretmanager` does
#' **not** come with a built-in OAuth client or API key. You must configure
#' your own via [sm_auth_configure()] or provide a service account
#' token via the `path` argument.
#'
#' @param email Optional. The email address of the Google identity you want to
#'   authenticate with. Useful for selecting a specific account if you have
#'   multiple, or for non-interactive authentication. If `NULL`, gargle will
#'   try to obtain it from the `"gargle_oauth_email"` option or allow you to
#'   choose from a list in interactive sessions. See [gargle::gargle_oauth_email()]
#'   for more details.
#' @param path Optional. Path to a service account token (JSON file) or a
#'   pre-existing token. If provided, this will be used for authentication
#'   instead of the OAuth flow. See [gargle::token_fetch()] for details on
#'   accepted formats.
#' @param scopes The OAuth scopes to request. For Secret Manager, a common scope is
#'   `"https://www.googleapis.com/auth/secretmanager"` or the broader
#'   `"https://www.googleapis.com/auth/cloud-platform"`. Defaults to
#'   `"https://www.googleapis.com/auth/secretmanager"`.
#' @param cache The location of the OAuth token cache. Defaults to
#'   [gargle::gargle_oauth_cache()].
#' @param use_oob Whether to prefer "out-of-band" (OOB) authentication. Defaults to
#'   [gargle::gargle_oob_default()]. Useful for non-interactive sessions where
#'   a browser cannot be easily launched.
#' @param token A pre-existing token object (e.g., from `httr::Token2.0` or
#'   another `gargle`-using package). If provided, this token will be used directly.
#'
#' @return Invisibly returns `NULL`. The main effect is to configure authentication
#'   state for the package.
#' @export
#' @examples
#' \dontrun{
#' # To configure your own OAuth client (do this once per project/user):
#' sm_auth_configure(
#'  path = "/path/to/your/oauth-client-secret.json"
#' )
#'
#' # Authenticate (often not needed explicitly, called by API functions):
#' sm_auth()
#'
#' # Authenticate with a specific user:
#' sm_auth(email = "my_user@example.com")
#'
#' # Authenticate using a service account:
#' sm_auth(path = "/path/to/your/service-account-key.json")
#'
#' # Authenticate using a pre-fetched token (like googleCloudStorageR example):
#' token <- gargle::token_fetch(
#'   scopes = "https://www.googleapis.com/auth/cloud-platform"
#' )
#' sm_auth(token = token)
#' }
sm_auth <- function(email = gargle::gargle_oauth_email(),
                               path = NULL,
                               scopes = "https://www.googleapis.com/auth/secretmanager", # Adjust if needed
                               cache = gargle::gargle_oauth_cache(),
                               use_oob = gargle::gargle_oob_default(),
                               token = NULL) {

  # If a token is explicitly passed, use it directly
  if (!is.null(token)) {
    if (!inherits(token, "Token2.0")) {
      stop("The 'token' argument must be an object of class 'httr::Token2.0'.", call. = FALSE)
    }
    .sm_auth$set_cred(token)
    .sm_auth$set_auth_active(TRUE)
    invisible()
  }

  # If no explicit token, proceed with token_fetch
  gargle::check_is_service_account(path, hint = "sm_auth_configure")

  client <- sm_oauth_client()
  if (is.null(client) && is.null(path) && is.null(token)) {
    stop(
      "No OAuth client configured and no service account path or token provided.\n",
      "Please configure an OAuth client using `sm_auth_configure()`,\n",
      "or provide a service account token via the `path` argument or the `token` argument.",
      call. = FALSE
    )
  }

  cred <- gargle::token_fetch(
    scopes = scopes,
    client = client,
    email = email,
    path = path,
    package = "secretmanager", # Important for caching and user messages
    cache = cache,
    use_oob = use_oob,
    token = token # This token argument is for gargle::token_fetch's internal logic
  )

  if (!inherits(cred, "Token2.0")) {
    stop(
      "Failed to obtain a valid token. Please check your authentication setup.",
      call. = FALSE
    )
  }

  .sm_auth$set_cred(cred)
  .sm_auth$set_auth_active(TRUE)

  invisible()
}

#' Check if a token is available
#'
#' @description
#' Checks if a token is present in the internal authentication state.
#' This is an internal helper function.
#'
#' @return Logical. `TRUE` if a token is available, `FALSE` otherwise.
#' @keywords internal
sm_has_token <- function() {
  inherits(.sm_auth$cred, "Token2.0")
}

#' Provide a token for Secret Manager API requests
#'
#' @description
#' Retrieves the current token for Secret Manager. If authentication is active
#' (`.sm_auth$auth_active` is `TRUE`) and no token is cached, it will trigger
#' [sm_auth()] to obtain one.
#'
#' This function is typically used by other package functions that make API
#' requests.
#'
#' @return An `httr::config` object containing the `httr::Token2.0` object, or `NULL` if auth is inactive.
#' @export
#' @examples
#' \dontrun{
#' # Configure auth first if needed (e.g., with your client ID)
#' # sm_auth_configure(path = "path/to/client.json")
#' # sm_auth() # or let it be called automatically
#'
#' token <- sm_token()
#' if (!is.null(token)) {
#'   # Use token in httr::GET() or other API calls
#' }
#' }
sm_token <- function() {
  if (isFALSE(.sm_auth$auth_active)) {
    # For Secret Manager, API key access is not typical for core operations.
    # If auth is inactive, it implies no authenticated requests should be made.
    return(NULL)
  }
  if (!sm_has_token()) {
    sm_auth() # Attempt to authenticate
  }
  # The token should be an httr::config object containing the Token2.0 object
  # gargle::token_fetch returns a Token2.0 object, .sm_auth$set_cred stores it.
  # httr requests expect a Token2.0 object directly or an httr::config()
  if (sm_has_token()) {
    return(httr::config(token = .sm_auth$cred))
  } else {
    # Should not happen if sm_auth() was successful
    # but as a fallback:
    return(NULL)
  }
}

#' De-authenticate from Secret Manager
#'
#' @description
#' Clears the current Secret Manager token. This means the next API request
#' that requires authentication will trigger the authentication process anew
#' (e.g., by calling [sm_auth()]).
#'
#' Since Secret Manager generally requires authentication for all its significant
#' operations (and doesn't typically use API keys for accessing secrets),
#' de-authentication primarily serves to clear the current user's session or
#' force a re-authentication.
#'
#' @return Invisibly returns `NULL`.
#' @export
#' @examples
#' \dontrun{
#' sm_deauth()
#' # Next API call will re-trigger auth
#' # list_secrets() # (Assuming this is a function in your package)
#' }
sm_deauth <- function() {
  # For APIs that always require a token (like Secret Manager for core ops),
  # deauth just clears the token. Auth remains active so next call triggers auth.
  # This follows the bigrquery model described in gargle documentation.
  .sm_auth$clear_cred()
  .sm_auth$set_auth_active(TRUE) # Keep auth active to re-trigger on next need
  invisible()
}

#' Configure OAuth client and API key for Secret Manager
#'
#' @description
#' This function allows advanced users to provide their own OAuth client ID and
#' secret (from a JSON file downloaded from Google Cloud Console) or an API key.
#'
#' **OAuth Client**: For `secretmanager`, providing your own OAuth client is
#' **highly recommended** as the package does not ship with a default client
#' due to the sensitive nature of the API.
#'
#' **API Key**: While the `gargle` framework supports API keys, most Secret
#' Manager operations require OAuth2.0 authentication. An API key might be
#' useful for very limited, typically read-only, public data scenarios, which are
#' rare for Secret Manager. It's included for structural consistency with `gargle`
#' but may have limited direct use for this package.
#'
#' @param path Path to a JSON file containing the OAuth client ID and secret.
#'   This is the recommended way to configure an OAuth client.
#' @param client An `httr::oauth_app` object or `gargle::gargle_oauth_client`
#'   object. Alternatively, provide `path`.
#' @param api_key A string representing your Google Cloud API key.
#' @param app Deprecated. Use `client` instead.
#' @return Invisibly returns the updated auth configuration (an `AuthState` object).
#' @export
#' @examples
#' \dontrun{
#' # Configure with an OAuth client downloaded from GCP
#' sm_auth_configure(
#'   path = "/path/to/your/oauth-client-secret.json"
#' )
#'
#' # To configure with an API key (less common for Secret Manager):
#' # sm_auth_configure(api_key = "YOUR_API_KEY")
#'
#' # Check configured client:
#' sm_oauth_client()
#' }
sm_auth_configure <- function(path = NULL, client = NULL, api_key = NULL, app = NULL) {
  if (!is.null(app)) {
    lifecycle::deprecate_warn("1.0.0", "sm_auth_configure(app = )", "sm_auth_configure(client = )")
    client <- app
  }

  if (!is.null(path) && !is.null(client)) {
    stop("Must supply `path` or `client`, but not both.", call. = FALSE)
  }

  if (!is.null(path)) {
    client <- gargle::gargle_oauth_client_from_json(path)
  }

  if (!is.null(client)) {
    if (!inherits(client, "gargle_oauth_client")) {
      stop("`client` must be a `gargle_oauth_client` or `NULL`.", call. = FALSE)
    }
    .sm_auth$set_client(client)
  }

  if (!is.null(api_key)) {
    stopifnot(is.character(api_key), length(api_key) == 1) # Changed is.string to is.character & length check
    .sm_auth$set_api_key(api_key)
  }

  invisible(.sm_auth)
}

#' Retrieve the configured OAuth client
#'
#' @description
#' Returns the currently configured OAuth client for the `secretmanager` package.
#' This client is used in the OAuth flow to obtain tokens.
#' By default, this will be `NULL` until configured by the user via
#' [sm_auth_configure()].
#'
#' @return A `gargle_oauth_client` object, or `NULL` if no client is configured.
#' @export
#' @examples
#' \dontrun{
#' # Configure client first
#' # sm_auth_configure(path = "/path/to/your/client.json")
#' client <- sm_oauth_client()
#' if (!is.null(client)) {
#'   print(client)
#' }
#' }
sm_oauth_client <- function() {
  .sm_auth$client
}

#' Retrieve the configured API key
#'
#' @description
#' Returns the currently configured API key for the `secretmanager` package.
#' Note: API keys have limited use with Secret Manager, which primarily relies
#' on OAuth2.0.
#'
#' @return A string containing the API key, or `NULL` if no key is configured.
#' @export
#' @examples
#' \dontrun{
#' # Configure API key first (if applicable)
#' # sm_auth_configure(api_key = "YOUR_API_KEY")
#' key <- sm_api_key()
#' if (!is.null(key)) {
#'   print(key)
#' }
#' }
sm_api_key <- function() {
  .sm_auth$api_key
}

#' Get information about the authenticated user
#'
#' @description
#' Provides information about the Google identity associated with the current
#' token. This usually includes the email address.
#' It attempts to retrieve this information from the token itself.
#'
#' @param token An optional token object. If `NULL`, uses the cached token
#'   obtained by [sm_token()].
#' @return A list containing user information (e.g., email) or `NULL` if
#'   no token is available or user information cannot be parsed.
#' @export
#' @examples
#' \dontrun{
#' # Authenticate first
#' # sm_auth()
#'
#' user_info <- sm_user()
#' if (!is.null(user_info)) {
#'   print(user_info$email)
#' }
#' }
sm_user <- function(token = NULL) {
  if (is.null(token)) {
    # Ensure we have a token by calling sm_token(),
    # which internally calls sm_auth() if needed.
    # We need the actual Token2.0 object, not the httr::config() wrapper.
    if (!sm_has_token()) {
      sm_token() # This triggers auth and populates .sm_auth$cred
    }
    token_obj <- .sm_auth$cred
  } else {
    token_obj <- token
  }

  if (!inherits(token_obj, "Token2.0")) {
    return(NULL)
  }

  # Try to get email from token claims
  # gargle::token_email attempts to parse the ID token if available
  email <- gargle::token_email(token_obj)
  if (!is.null(email)) {
    return(list(email = email))
  }

  # If email couldn't be extracted, return NULL or a list with NA
  return(list(email = NA_character_))
}
