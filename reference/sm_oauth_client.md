# Retrieve the configured OAuth client

Returns the currently configured OAuth client for the
`googlesecretmanager` package. This client is used in the OAuth flow to
obtain tokens. By default, this will be `NULL` until configured by the
user via
[`sm_auth_configure()`](https://brancengregory.github.io/googlesecretmanager/reference/sm_auth_configure.md).

## Usage

``` r
sm_oauth_client()
```

## Value

A `gargle_oauth_client` object, or `NULL` if no client is configured.

## Examples

``` r
if (FALSE) { # \dontrun{
# Configure client first
# sm_auth_configure(path = "/path/to/your/client.json")
client <- sm_oauth_client()
if (!is.null(client)) {
  print(client)
}
} # }
```
