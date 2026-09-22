# Get information about the authenticated user

Provides information about the Google identity associated with the
current token. This usually includes the email address. It attempts to
retrieve this information from the token itself.

## Usage

``` r
sm_user(token = NULL)
```

## Arguments

- token:

  An optional token object. If `NULL`, uses the cached token obtained by
  [`sm_token()`](https://brancengregory.github.io/googlesecretmanager/reference/sm_token.md).

## Value

A list containing user information (e.g., email) or `NULL` if no token
is available or user information cannot be parsed.

## Examples

``` r
if (FALSE) { # \dontrun{
# Authenticate first
# sm_auth()

user_info <- sm_user()
if (!is.null(user_info)) {
  print(user_info$email)
}
} # }
```
