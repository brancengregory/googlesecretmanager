# Provide a token for Secret Manager API requests

Retrieves the current token for Secret Manager. If authentication is
active (`.sm_auth$auth_active` is `TRUE`) and no token is cached, it
will trigger
[`sm_auth()`](https://brancengregory.github.io/googlesecretmanager/reference/sm_auth.md)
to obtain one.

This function is typically used by other package functions that make API
requests.

## Usage

``` r
sm_token()
```

## Value

An [`httr::config`](https://httr.r-lib.org/reference/config.html) object
containing the
[`httr::Token2.0`](https://httr.r-lib.org/reference/Token-class.html)
object, or `NULL` if auth is inactive.

## Examples

``` r
if (FALSE) { # \dontrun{
# Configure auth first if needed (e.g., with your client ID)
# sm_auth_configure(path = "path/to/client.json")
# sm_auth() # or let it be called automatically

token <- sm_token()
if (!is.null(token)) {
  # Use token in httr::GET() or other API calls
}
} # }
```
