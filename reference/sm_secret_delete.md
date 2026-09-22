# Delete a Secret

Deletes a Secret and all of its versions.

## Usage

``` r
sm_secret_delete(secret, project_id = sm_project_get(), ...)

# S3 method for class 'character'
sm_secret_delete(secret, project_id = sm_project_get(), ...)

# S3 method for class 'sm_secret'
sm_secret_delete(secret, project_id = sm_project_get(), ...)
```

## Arguments

- secret:

  The secret to delete. Can be a secret ID (character string) or an
  existing `sm_secret` object.

- project_id:

  The Google Cloud Project ID. Defaults to `sm_project_get()`.

- ...:

  Additional arguments for methods.

## Value

Invisibly returns `NULL`.
