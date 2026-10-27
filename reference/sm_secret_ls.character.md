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
  rules](https://docs.cloud.google.com/secret-manager/docs/filtering).

- ...:

  Additional arguments for methods.

## Value

An `sm_tbl` object, a data frame with class
`c("sm_tbl", "tbl_df", "tbl", "data.frame")` and one row per secret.
Columns are `secret_id`, `project_id`, `name`, `createTime`, `labels` (a
list-column), and `etag`. An empty `sm_tbl` is returned when the project
contains no secrets.
