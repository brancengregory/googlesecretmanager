# Access a Secret Version's Value

Accesses and decrypts the secret data stored in a specific Secret
Version. Unlike
[`sm_secret_version_get()`](https://brancengregory.github.io/googlesecretmanager/reference/sm_secret_version_get.md),
which returns version metadata, this function returns the actual secret
value.

## Usage

``` r
sm_secret_version_access(
  secret,
  version_id = "latest",
  project_id = sm_project_get(),
  ...
)

# S3 method for class 'character'
sm_secret_version_access(
  secret,
  version_id = "latest",
  project_id = sm_project_get(),
  ...
)

# S3 method for class 'sm_secret'
sm_secret_version_access(
  secret,
  version_id = "latest",
  project_id = sm_project_get(),
  ...
)
```

## Arguments

- secret:

  The secret containing the version. Can be a secret ID (character
  string) or an existing `sm_secret` object.

- version_id:

  The version ID to access. Can be "latest" to access the latest
  version.

- project_id:

  The Google Cloud Project ID. Defaults to `sm_project_get()`.

- ...:

  Additional arguments for methods.

## Value

A length-one character vector containing the decoded secret value. The
`data_crc32c` checksum returned by the API is attached as an attribute.
