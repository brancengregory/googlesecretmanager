# Retrieve the configured API key

Returns the currently configured API key for the `googlesecretmanager`
package. Note: API keys have limited use with Secret Manager, which
primarily relies on OAuth2.0.

## Usage

``` r
sm_api_key()
```

## Value

A string containing the API key, or `NULL` if no key is configured.

## Examples

``` r
if (FALSE) { # \dontrun{
# Configure API key first (if applicable)
# sm_auth_configure(api_key = "YOUR_API_KEY")
key <- sm_api_key()
if (!is.null(key)) {
  print(key)
}
} # }
```
