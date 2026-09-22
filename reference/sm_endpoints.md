# List Secret Manager endpoints

The googlesecretmanager package stores a named list of Secret Manager
API v1 endpoints (or "methods", using Google's vocabulary) internally
and these functions expose this data.

- [`sm_endpoint()`](https://brancengregory.github.io/googlesecretmanager/reference/sm_endpoint.md)
  returns one endpoint, i.e. it uses `[[`.

- `sm_endpoints()` returns a list of endpoints, i.e. it uses `[`.

The names of this list (or the `id` sub-elements) are the nicknames that
can be used to specify an endpoint in request_generate(). For each
endpoint, we store its nickname or `id`, the associated HTTP verb, the
`path`, and details about the parameters. This list is derived
programmatically from the Secret Manager API v1 Discovery Document
(`https://www.googleapis.com/discovery/v1/apis/secretmanager/v1/rest`)
using the approach described in the [Discovery Documents
section](https://gargle.r-lib.org/articles/request-helper-functions.html#discovery-documents)
of the gargle vignette [Request helper
functions](https://gargle.r-lib.org/articles/request-helper-functions.html).

## Usage

``` r
sm_endpoints(i = NULL)
```

## Arguments

- i:

  The name(s) or integer index(ices) of the endpoints to return. `i` is
  optional for `sm_endpoints()` and, if not given, the entire list is
  returned.

## Value

One or more of the Secret Manager API v1 endpoints that are used
internally by googlesecretmanager.

## Examples

``` r
str(head(sm_endpoints(), 3), max.level = 2)
#> List of 3
#>  $ secretmanager.projects.locations.list        :List of 8
#>   ..$ id            : chr "secretmanager.projects.locations.list"
#>   ..$ httpMethod    : chr "GET"
#>   ..$ path          : 'fs_path' chr "/v1/{name}/locations"
#>   ..$ parameters    :List of 16
#>   ..$ scopes        : chr "cloud-platform"
#>   ..$ description   : chr "Lists information about the supported locations for this service."
#>   ..$ response      : chr "ListLocationsResponse"
#>   ..$ parameterOrder: chr "name"
#>  $ secretmanager.projects.locations.get         :List of 8
#>   ..$ id            : chr "secretmanager.projects.locations.get"
#>   ..$ httpMethod    : chr "GET"
#>   ..$ path          : 'fs_path' chr "/v1/{name}"
#>   ..$ parameters    :List of 12
#>   ..$ scopes        : chr "cloud-platform"
#>   ..$ description   : chr "Gets information about a location."
#>   ..$ response      : chr "Location"
#>   ..$ parameterOrder: chr "name"
#>  $ secretmanager.projects.locations.secrets.list:List of 8
#>   ..$ id            : chr "secretmanager.projects.locations.secrets.list"
#>   ..$ httpMethod    : chr "GET"
#>   ..$ path          : 'fs_path' chr "/v1/{parent}/secrets"
#>   ..$ parameters    :List of 15
#>   ..$ scopes        : chr "cloud-platform"
#>   ..$ description   : chr "Lists Secrets."
#>   ..$ response      : chr "ListSecretsResponse"
#>   ..$ parameterOrder: chr "parent"
sm_endpoint("secretmanager.projects.secrets.versions.destroy")
#> $id
#> [1] "secretmanager.projects.secrets.versions.destroy"
#> 
#> $httpMethod
#> [1] "POST"
#> 
#> $path
#> /v1/{name}:destroy
#> 
#> $parameters
#> $parameters$name
#> $parameters$name$description
#> [1] "Required. The resource name of the SecretVersion to destroy in the format `projects/*/secrets/*/versions/*` or `projects/*/locations/*/secrets/*/versions/*`."
#> 
#> $parameters$name$pattern
#> [1] "^projects/[^/]+/secrets/[^/]+/versions/[^/]+$"
#> 
#> $parameters$name$location
#> [1] "path"
#> 
#> $parameters$name$required
#> [1] TRUE
#> 
#> $parameters$name$type
#> [1] "string"
#> 
#> 
#> $parameters$etag
#> $parameters$etag$description
#> [1] "Optional. Etag of the SecretVersion. The request succeeds if it matches the etag of the currently stored secret version object. If the etag is omitted, the request succeeds."
#> 
#> $parameters$etag$type
#> [1] "string"
#> 
#> $parameters$etag$location
#> [1] "body"
#> 
#> 
#> $parameters$access_token
#> $parameters$access_token$type
#> [1] "string"
#> 
#> $parameters$access_token$description
#> [1] "OAuth access token."
#> 
#> $parameters$access_token$location
#> [1] "query"
#> 
#> 
#> $parameters$alt
#> $parameters$alt$type
#> [1] "string"
#> 
#> $parameters$alt$description
#> [1] "Data format for response."
#> 
#> $parameters$alt$default
#> [1] "json"
#> 
#> $parameters$alt$enum
#> [1] "json"  "media" "proto"
#> 
#> $parameters$alt$enumDescriptions
#> [1] "Responses with Content-Type of application/json"      
#> [2] "Media download with context-dependent Content-Type"   
#> [3] "Responses with Content-Type of application/x-protobuf"
#> 
#> $parameters$alt$location
#> [1] "query"
#> 
#> 
#> $parameters$callback
#> $parameters$callback$type
#> [1] "string"
#> 
#> $parameters$callback$description
#> [1] "JSONP"
#> 
#> $parameters$callback$location
#> [1] "query"
#> 
#> 
#> $parameters$fields
#> $parameters$fields$type
#> [1] "string"
#> 
#> $parameters$fields$description
#> [1] "Selector specifying which fields to include in a partial response."
#> 
#> $parameters$fields$location
#> [1] "query"
#> 
#> 
#> $parameters$key
#> $parameters$key$type
#> [1] "string"
#> 
#> $parameters$key$description
#> [1] "API key. Your API key identifies your project and provides you with API access, quota, and reports. Required unless you provide an OAuth 2.0 token."
#> 
#> $parameters$key$location
#> [1] "query"
#> 
#> 
#> $parameters$oauth_token
#> $parameters$oauth_token$type
#> [1] "string"
#> 
#> $parameters$oauth_token$description
#> [1] "OAuth 2.0 token for the current user."
#> 
#> $parameters$oauth_token$location
#> [1] "query"
#> 
#> 
#> $parameters$prettyPrint
#> $parameters$prettyPrint$type
#> [1] "boolean"
#> 
#> $parameters$prettyPrint$description
#> [1] "Returns response with indentations and line breaks."
#> 
#> $parameters$prettyPrint$default
#> [1] "true"
#> 
#> $parameters$prettyPrint$location
#> [1] "query"
#> 
#> 
#> $parameters$quotaUser
#> $parameters$quotaUser$type
#> [1] "string"
#> 
#> $parameters$quotaUser$description
#> [1] "Available to use for quota purposes for server-side applications. Can be any arbitrary string assigned to a user, but should not exceed 40 characters."
#> 
#> $parameters$quotaUser$location
#> [1] "query"
#> 
#> 
#> $parameters$upload_protocol
#> $parameters$upload_protocol$type
#> [1] "string"
#> 
#> $parameters$upload_protocol$description
#> [1] "Upload protocol for media (e.g. \"raw\", \"multipart\")."
#> 
#> $parameters$upload_protocol$location
#> [1] "query"
#> 
#> 
#> $parameters$uploadType
#> $parameters$uploadType$type
#> [1] "string"
#> 
#> $parameters$uploadType$description
#> [1] "Legacy upload protocol for media (e.g. \"media\", \"multipart\")."
#> 
#> $parameters$uploadType$location
#> [1] "query"
#> 
#> 
#> $parameters$`$.xgafv`
#> $parameters$`$.xgafv`$type
#> [1] "string"
#> 
#> $parameters$`$.xgafv`$description
#> [1] "V1 error format."
#> 
#> $parameters$`$.xgafv`$enum
#> [1] "1" "2"
#> 
#> $parameters$`$.xgafv`$enumDescriptions
#> [1] "v1 error format" "v2 error format"
#> 
#> $parameters$`$.xgafv`$location
#> [1] "query"
#> 
#> 
#> 
#> $scopes
#> [1] "cloud-platform"
#> 
#> $description
#> [1] "Destroys a SecretVersion. Sets the state of the SecretVersion to DESTROYED and irrevocably destroys the secret data."
#> 
#> $request
#> [1] "DestroySecretVersionRequest"
#> 
#> $response
#> [1] "SecretVersion"
#> 
#> $parameterOrder
#> [1] "name"
#> 
sm_endpoint(4)
#> $id
#> [1] "secretmanager.projects.locations.secrets.create"
#> 
#> $httpMethod
#> [1] "POST"
#> 
#> $path
#> /v1/{parent}/secrets
#> 
#> $parameters
#> $parameters$parent
#> $parameters$parent$description
#> [1] "Required. The resource name of the project to associate with the Secret, in the format `projects/*` or `projects/*/locations/*`."
#> 
#> $parameters$parent$pattern
#> [1] "^projects/[^/]+/locations/[^/]+$"
#> 
#> $parameters$parent$location
#> [1] "path"
#> 
#> $parameters$parent$required
#> [1] TRUE
#> 
#> $parameters$parent$type
#> [1] "string"
#> 
#> 
#> $parameters$secretId
#> $parameters$secretId$description
#> [1] "Required. This must be unique within the project. A secret ID is a string with a maximum length of 255 characters and can contain uppercase and lowercase letters, numerals, and the hyphen (`-`) and underscore (`_`) characters."
#> 
#> $parameters$secretId$location
#> [1] "query"
#> 
#> $parameters$secretId$type
#> [1] "string"
#> 
#> 
#> $parameters$name
#> $parameters$name$description
#> [1] "Output only. The resource name of the Secret in the format `projects/*/secrets/*`."
#> 
#> $parameters$name$readOnly
#> [1] TRUE
#> 
#> $parameters$name$type
#> [1] "string"
#> 
#> $parameters$name$location
#> [1] "body"
#> 
#> 
#> $parameters$replication
#> $parameters$replication$description
#> [1] "Optional. Immutable. The replication policy of the secret data attached to the Secret. The replication policy cannot be changed after the Secret has been created."
#> 
#> $parameters$replication$`$ref`
#> [1] "Replication"
#> 
#> $parameters$replication$location
#> [1] "body"
#> 
#> 
#> $parameters$createTime
#> $parameters$createTime$description
#> [1] "Output only. The time at which the Secret was created."
#> 
#> $parameters$createTime$readOnly
#> [1] TRUE
#> 
#> $parameters$createTime$type
#> [1] "string"
#> 
#> $parameters$createTime$format
#> [1] "google-datetime"
#> 
#> $parameters$createTime$location
#> [1] "body"
#> 
#> 
#> $parameters$labels
#> $parameters$labels$description
#> [1] "The labels assigned to this Secret. Label keys must be between 1 and 63 characters long, have a UTF-8 encoding of maximum 128 bytes, and must conform to the following PCRE regular expression: `\\p{Ll}\\p{Lo}{0,62}` Label values must be between 0 and 63 characters long, have a UTF-8 encoding of maximum 128 bytes, and must conform to the following PCRE regular expression: `[\\p{Ll}\\p{Lo}\\p{N}_-]{0,63}` No more than 64 labels can be assigned to a given resource."
#> 
#> $parameters$labels$type
#> [1] "object"
#> 
#> $parameters$labels$additionalProperties
#> $parameters$labels$additionalProperties$type
#> [1] "string"
#> 
#> 
#> $parameters$labels$location
#> [1] "body"
#> 
#> 
#> $parameters$topics
#> $parameters$topics$description
#> [1] "Optional. A list of up to 10 Pub/Sub topics to which messages are published when control plane operations are called on the secret or its versions."
#> 
#> $parameters$topics$type
#> [1] "array"
#> 
#> $parameters$topics$items
#> $parameters$topics$items$`$ref`
#> [1] "Topic"
#> 
#> 
#> $parameters$topics$location
#> [1] "body"
#> 
#> 
#> $parameters$expireTime
#> $parameters$expireTime$description
#> [1] "Optional. Timestamp in UTC when the Secret is scheduled to expire. This is always provided on output, regardless of what was sent on input."
#> 
#> $parameters$expireTime$type
#> [1] "string"
#> 
#> $parameters$expireTime$format
#> [1] "google-datetime"
#> 
#> $parameters$expireTime$location
#> [1] "body"
#> 
#> 
#> $parameters$ttl
#> $parameters$ttl$description
#> [1] "Input only. The TTL for the Secret."
#> 
#> $parameters$ttl$type
#> [1] "string"
#> 
#> $parameters$ttl$format
#> [1] "google-duration"
#> 
#> $parameters$ttl$location
#> [1] "body"
#> 
#> 
#> $parameters$etag
#> $parameters$etag$description
#> [1] "Optional. Etag of the currently stored Secret."
#> 
#> $parameters$etag$type
#> [1] "string"
#> 
#> $parameters$etag$location
#> [1] "body"
#> 
#> 
#> $parameters$rotation
#> $parameters$rotation$description
#> [1] "Optional. Rotation policy attached to the Secret. May be excluded if there is no rotation policy."
#> 
#> $parameters$rotation$`$ref`
#> [1] "Rotation"
#> 
#> $parameters$rotation$location
#> [1] "body"
#> 
#> 
#> $parameters$versionAliases
#> $parameters$versionAliases$description
#> [1] "Optional. Mapping from version alias to version name. A version alias is a string with a maximum length of 63 characters and can contain uppercase and lowercase letters, numerals, and the hyphen (`-`) and underscore ('_') characters. An alias string must start with a letter and cannot be the string 'latest' or 'NEW'. No more than 50 aliases can be assigned to a given secret. Version-Alias pairs will be viewable via GetSecret and modifiable via UpdateSecret. Access by alias is only be supported on GetSecretVersion and AccessSecretVersion."
#> 
#> $parameters$versionAliases$type
#> [1] "object"
#> 
#> $parameters$versionAliases$additionalProperties
#> $parameters$versionAliases$additionalProperties$type
#> [1] "string"
#> 
#> $parameters$versionAliases$additionalProperties$format
#> [1] "int64"
#> 
#> 
#> $parameters$versionAliases$location
#> [1] "body"
#> 
#> 
#> $parameters$annotations
#> $parameters$annotations$description
#> [1] "Optional. Custom metadata about the secret. Annotations are distinct from various forms of labels. Annotations exist to allow client tools to store their own state information without requiring a database. Annotation keys must be between 1 and 63 characters long, have a UTF-8 encoding of maximum 128 bytes, begin and end with an alphanumeric character ([a-z0-9A-Z]), and may have dashes (-), underscores (_), dots (.), and alphanumerics in between these symbols. The total size of annotation keys and values must be less than 16KiB."
#> 
#> $parameters$annotations$type
#> [1] "object"
#> 
#> $parameters$annotations$additionalProperties
#> $parameters$annotations$additionalProperties$type
#> [1] "string"
#> 
#> 
#> $parameters$annotations$location
#> [1] "body"
#> 
#> 
#> $parameters$versionDestroyTtl
#> $parameters$versionDestroyTtl$description
#> [1] "Optional. Secret Version TTL after destruction request This is a part of the Delayed secret version destroy feature. For secret with TTL>0, version destruction doesn't happen immediately on calling destroy instead the version goes to a disabled state and destruction happens after the TTL expires."
#> 
#> $parameters$versionDestroyTtl$type
#> [1] "string"
#> 
#> $parameters$versionDestroyTtl$format
#> [1] "google-duration"
#> 
#> $parameters$versionDestroyTtl$location
#> [1] "body"
#> 
#> 
#> $parameters$customerManagedEncryption
#> $parameters$customerManagedEncryption$description
#> [1] "Optional. The customer-managed encryption configuration of the regionalized secrets. If no configuration is provided, Google-managed default encryption is used. Updates to the Secret encryption configuration only apply to SecretVersions added afterwards. They do not apply retroactively to existing SecretVersions."
#> 
#> $parameters$customerManagedEncryption$`$ref`
#> [1] "CustomerManagedEncryption"
#> 
#> $parameters$customerManagedEncryption$location
#> [1] "body"
#> 
#> 
#> $parameters$access_token
#> $parameters$access_token$type
#> [1] "string"
#> 
#> $parameters$access_token$description
#> [1] "OAuth access token."
#> 
#> $parameters$access_token$location
#> [1] "query"
#> 
#> 
#> $parameters$alt
#> $parameters$alt$type
#> [1] "string"
#> 
#> $parameters$alt$description
#> [1] "Data format for response."
#> 
#> $parameters$alt$default
#> [1] "json"
#> 
#> $parameters$alt$enum
#> [1] "json"  "media" "proto"
#> 
#> $parameters$alt$enumDescriptions
#> [1] "Responses with Content-Type of application/json"      
#> [2] "Media download with context-dependent Content-Type"   
#> [3] "Responses with Content-Type of application/x-protobuf"
#> 
#> $parameters$alt$location
#> [1] "query"
#> 
#> 
#> $parameters$callback
#> $parameters$callback$type
#> [1] "string"
#> 
#> $parameters$callback$description
#> [1] "JSONP"
#> 
#> $parameters$callback$location
#> [1] "query"
#> 
#> 
#> $parameters$fields
#> $parameters$fields$type
#> [1] "string"
#> 
#> $parameters$fields$description
#> [1] "Selector specifying which fields to include in a partial response."
#> 
#> $parameters$fields$location
#> [1] "query"
#> 
#> 
#> $parameters$key
#> $parameters$key$type
#> [1] "string"
#> 
#> $parameters$key$description
#> [1] "API key. Your API key identifies your project and provides you with API access, quota, and reports. Required unless you provide an OAuth 2.0 token."
#> 
#> $parameters$key$location
#> [1] "query"
#> 
#> 
#> $parameters$oauth_token
#> $parameters$oauth_token$type
#> [1] "string"
#> 
#> $parameters$oauth_token$description
#> [1] "OAuth 2.0 token for the current user."
#> 
#> $parameters$oauth_token$location
#> [1] "query"
#> 
#> 
#> $parameters$prettyPrint
#> $parameters$prettyPrint$type
#> [1] "boolean"
#> 
#> $parameters$prettyPrint$description
#> [1] "Returns response with indentations and line breaks."
#> 
#> $parameters$prettyPrint$default
#> [1] "true"
#> 
#> $parameters$prettyPrint$location
#> [1] "query"
#> 
#> 
#> $parameters$quotaUser
#> $parameters$quotaUser$type
#> [1] "string"
#> 
#> $parameters$quotaUser$description
#> [1] "Available to use for quota purposes for server-side applications. Can be any arbitrary string assigned to a user, but should not exceed 40 characters."
#> 
#> $parameters$quotaUser$location
#> [1] "query"
#> 
#> 
#> $parameters$upload_protocol
#> $parameters$upload_protocol$type
#> [1] "string"
#> 
#> $parameters$upload_protocol$description
#> [1] "Upload protocol for media (e.g. \"raw\", \"multipart\")."
#> 
#> $parameters$upload_protocol$location
#> [1] "query"
#> 
#> 
#> $parameters$uploadType
#> $parameters$uploadType$type
#> [1] "string"
#> 
#> $parameters$uploadType$description
#> [1] "Legacy upload protocol for media (e.g. \"media\", \"multipart\")."
#> 
#> $parameters$uploadType$location
#> [1] "query"
#> 
#> 
#> $parameters$`$.xgafv`
#> $parameters$`$.xgafv`$type
#> [1] "string"
#> 
#> $parameters$`$.xgafv`$description
#> [1] "V1 error format."
#> 
#> $parameters$`$.xgafv`$enum
#> [1] "1" "2"
#> 
#> $parameters$`$.xgafv`$enumDescriptions
#> [1] "v1 error format" "v2 error format"
#> 
#> $parameters$`$.xgafv`$location
#> [1] "query"
#> 
#> 
#> 
#> $scopes
#> [1] "cloud-platform"
#> 
#> $description
#> [1] "Creates a new Secret containing no SecretVersions."
#> 
#> $request
#> [1] "Secret"
#> 
#> $response
#> [1] "Secret"
#> 
#> $parameterOrder
#> [1] "parent"
#> 
```
