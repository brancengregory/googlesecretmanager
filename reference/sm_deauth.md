# De-authenticate from Secret Manager

Clears the current Secret Manager token. This means the next API request
that requires authentication will trigger the authentication process
anew (e.g., by calling
[`sm_auth()`](https://brancengregory.github.io/googlesecretmanager/reference/sm_auth.md)).

Since Secret Manager generally requires authentication for all its
significant operations (and doesn't typically use API keys for accessing
secrets), de-authentication primarily serves to clear the current user's
session or force a re-authentication.

## Usage

``` r
sm_deauth()
```

## Value

Invisibly returns `NULL`.

## Examples

``` r
if (FALSE) { # \dontrun{
sm_deauth()
# Next API call will re-trigger auth
# list_secrets() # (Assuming this is a function in your package)
} # }
```
