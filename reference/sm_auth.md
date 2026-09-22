# Authenticate with Google Secret Manager

This function handles authentication with Google Cloud Secret Manager.
It's called automatically when the first token is needed, or it can be
called directly by the user to pre-authenticate or to switch identities,
scopes, or authentication methods (e.g., user OAuth, service account
token).

Following `gargle` best practices for sensitive APIs,
`googlesecretmanager` does **not** come with a built-in OAuth client or
API key. You must configure your own via
[`sm_auth_configure()`](https://brancengregory.github.io/googlesecretmanager/reference/sm_auth_configure.md)
or provide a service account token via the `path` argument.

## Usage

``` r
sm_auth(
  email = gargle::gargle_oauth_email(),
  path = NULL,
  scopes = "https://www.googleapis.com/auth/secretmanager",
  cache = gargle::gargle_oauth_cache(),
  use_oob = gargle::gargle_oob_default(),
  token = NULL
)
```

## Arguments

- email:

  Optional. The email address of the Google identity you want to
  authenticate with. Useful for selecting a specific account if you have
  multiple, or for non-interactive authentication. If `NULL`, gargle
  will try to obtain it from the `"gargle_oauth_email"` option or allow
  you to choose from a list in interactive sessions. See
  [`gargle::gargle_oauth_email()`](https://gargle.r-lib.org/reference/gargle_options.html)
  for more details.

- path:

  Optional. Path to a service account token (JSON file) or a
  pre-existing token. If provided, this will be used for authentication
  instead of the OAuth flow. See
  [`gargle::token_fetch()`](https://gargle.r-lib.org/reference/token_fetch.html)
  for details on accepted formats.

- scopes:

  The OAuth scopes to request. For Secret Manager, a common scope is
  `"https://www.googleapis.com/auth/secretmanager"` or the broader
  `"https://www.googleapis.com/auth/cloud-platform"`. Defaults to
  `"https://www.googleapis.com/auth/secretmanager"`.

- cache:

  The location of the OAuth token cache. Defaults to
  [`gargle::gargle_oauth_cache()`](https://gargle.r-lib.org/reference/gargle_options.html).

- use_oob:

  Whether to prefer "out-of-band" (OOB) authentication. Defaults to
  [`gargle::gargle_oob_default()`](https://gargle.r-lib.org/reference/gargle_options.html).
  Useful for non-interactive sessions where a browser cannot be easily
  launched.

- token:

  A pre-existing token object (e.g., from
  [`httr::Token2.0`](https://httr.r-lib.org/reference/Token-class.html)
  or another `gargle`-using package). If provided, this token will be
  used directly.

## Value

Invisibly returns `NULL`. The main effect is to configure authentication
state for the package.

## Examples

``` r
if (FALSE) { # \dontrun{
# To configure your own OAuth client (do this once per project/user):
sm_auth_configure(
 path = "/path/to/your/oauth-client-secret.json"
)

# Authenticate (often not needed explicitly, called by API functions):
sm_auth()

# Authenticate with a specific user:
sm_auth(email = "my_user@example.com")

# Authenticate using a service account:
sm_auth(path = "/path/to/your/service-account-key.json")

# Authenticate using a pre-fetched token (like googleCloudStorageR example):
token <- gargle::token_fetch(
  scopes = "https://www.googleapis.com/auth/cloud-platform"
)
sm_auth(token = token)
} # }
```
