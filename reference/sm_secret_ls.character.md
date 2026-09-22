# List Secrets in a Project

Lists metadata for all Secrets in a given Google Cloud Project.

## Usage

``` r
# S3 method for class 'character'
sm_secret_ls(project_id = sm_project_get(), filter = NULL, ...)
```

## Arguments

- project_id:

  The Google Cloud Project ID. Defaults to `sm_project_get()`.

- filter:

  Optional. A filter string, adhering to Secret Manager's
  [List-operation filtering
  rules](https://cloud.google.com/secret-manager/docs/filtering).

- ...:

  Additional arguments for methods.
