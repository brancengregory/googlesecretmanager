# Configure OAuth client and API key for Secret Manager

This function allows advanced users to provide their own OAuth client ID
and secret (from a JSON file downloaded from Google Cloud Console) or an
API key.

**OAuth Client**: For `googlesecretmanager`, providing your own OAuth
client is **highly recommended** as the package does not ship with a
default client due to the sensitive nature of the API.

**API Key**: While the `gargle` framework supports API keys, most Secret
Manager operations require OAuth2.0 authentication. An API key might be
useful for very limited, typically read-only, public data scenarios,
which are rare for Secret Manager. It's included for structural
consistency with `gargle` but may have limited direct use for this
package.

## Usage

``` r
sm_auth_configure(path = NULL, client = NULL, api_key = NULL, app = NULL)
```

## Arguments

- path:

  Path to a JSON file containing the OAuth client ID and secret. This is
  the recommended way to configure an OAuth client.

- client:

  An
  [`httr::oauth_app`](https://httr.r-lib.org/reference/oauth_app.html)
  object or
  [`gargle::gargle_oauth_client`](https://gargle.r-lib.org/reference/gargle_oauth_client_from_json.html)
  object. Alternatively, provide `path`.

- api_key:

  A string representing your Google Cloud API key.

- app:

  Deprecated. Use `client` instead.

## Value

Invisibly returns the updated auth configuration (an `AuthState`
object).

## Examples

``` r
if (FALSE) { # \dontrun{
# Configure with an OAuth client downloaded from GCP
sm_auth_configure(
  path = "/path/to/your/oauth-client-secret.json"
)

# To configure with an API key (less common for Secret Manager):
# sm_auth_configure(api_key = "YOUR_API_KEY")

# Check configured client:
sm_oauth_client()
} # }
```
