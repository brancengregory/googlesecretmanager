# Add a Secret Version

Adds a new version to an existing Secret.

## Usage

``` r
sm_secret_version_add(secret, payload, project_id = sm_project_get(), ...)

# S3 method for class 'character'
sm_secret_version_add(secret, payload, project_id = sm_project_get(), ...)

# S3 method for class 'sm_secret'
sm_secret_version_add(secret, payload, project_id = sm_project_get(), ...)
```

## Arguments

- secret:

  The secret to add a version to. Can be a secret ID (character string)
  or an existing `sm_secret` object.

- payload:

  The secret data to store. Will be base64 encoded.

- project_id:

  The Google Cloud Project ID. Defaults to `sm_project_get()`.

- ...:

  Additional arguments for methods.

## Value

An `sm_secret_version` object representing the new version.
