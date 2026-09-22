# Get Secret Version Metadata

Gets metadata for a specific Secret Version.

## Usage

``` r
sm_secret_version_get(secret, version_id, project_id = sm_project_get(), ...)

# S3 method for class 'character'
sm_secret_version_get(secret, version_id, project_id = sm_project_get(), ...)

# S3 method for class 'sm_secret'
sm_secret_version_get(secret, version_id, project_id = sm_project_get(), ...)
```

## Arguments

- secret:

  The secret containing the version. Can be a secret ID (character
  string) or an existing `sm_secret` object.

- version_id:

  The version ID to get. Can be "latest" to get the latest version.

- project_id:

  The Google Cloud Project ID. Defaults to `sm_project_get()`.

- ...:

  Additional arguments for methods.

## Value

An `sm_secret_version` object representing the version metadata.
