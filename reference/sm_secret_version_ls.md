# List Secret Versions

Lists metadata for all Secret Versions associated with a given Secret.

## Usage

``` r
sm_secret_version_ls(secret, project_id = sm_project_get(), filter = NULL, ...)

# S3 method for class 'sm_secret'
sm_secret_version_ls(secret, project_id = sm_project_get(), filter = NULL, ...)

# S3 method for class 'character'
sm_secret_version_ls(secret, project_id = sm_project_get(), filter = NULL, ...)
```

## Arguments

- secret:

  The secret for which to list versions. Can be an `sm_secret` object or
  a character string representing the secret ID.

- project_id:

  The Google Cloud Project ID. Defaults to `sm_project_get()`.

- filter:

  Optional. A filter string for secret versions.

- ...:

  Additional arguments for methods.
