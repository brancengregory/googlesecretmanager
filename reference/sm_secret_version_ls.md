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

## Value

An `sm_tbl` object, a data frame with class
`c("sm_tbl", "tbl_df", "tbl", "data.frame")` and one row per secret
version. Columns are `secret_id`, `version_id`, `name`, `createTime`,
`state`, and `etag`. An empty `sm_tbl` is returned when the secret has
no versions.
