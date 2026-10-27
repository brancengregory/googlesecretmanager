# Get Secret Metadata

Retrieves metadata for a specific Secret Manager secret.

## Usage

``` r
sm_secret_get(x, project_id = sm_project_get(), ...)

# S3 method for class 'character'
sm_secret_get(x, project_id = sm_project_get(), ...)

# S3 method for class 'sm_secret'
sm_secret_get(x, project_id = sm_project_get(), ...)
```

## Arguments

- x:

  The identifier for the secret. Can be a secret ID (character string)
  or an existing `sm_secret` object to refresh its metadata.

- project_id:

  The Google Cloud Project ID. Defaults to `sm_project_get()`.

- ...:

  Additional arguments for methods.

## Value

An `sm_secret` object: a list with class `c("sm_secret", "list")`
containing the secret's `name`, `secret_id`, `project_id`,
`location_id`, metadata such as `createTime`, `labels`, and `etag`, and
the raw API response in `raw_api_response`.
