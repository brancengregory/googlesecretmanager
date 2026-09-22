# Update a Secret

Updates metadata of an existing Secret.

## Usage

``` r
sm_secret_update(
  secret,
  project_id = sm_project_get(),
  replication = NULL,
  labels = NULL,
  etag = NULL,
  ...
)

# S3 method for class 'character'
sm_secret_update(
  secret,
  project_id = sm_project_get(),
  replication = NULL,
  labels = NULL,
  etag = NULL,
  ...
)

# S3 method for class 'sm_secret'
sm_secret_update(
  secret,
  project_id = sm_project_get(),
  replication = NULL,
  labels = NULL,
  etag = NULL,
  ...
)
```

## Arguments

- secret:

  The secret to update. Can be a secret ID (character string) or an
  existing `sm_secret` object.

- project_id:

  The Google Cloud Project ID. Defaults to `sm_project_get()`.

- replication:

  Optional. The replication policy for the secret data. Must be a list
  with either `automatic` or `user_managed` configuration.

- labels:

  Optional. Labels to attach to the secret.

- etag:

  Optional. The etag of the secret. If provided, the update will only
  succeed if the secret's current etag matches this value.

- ...:

  Additional arguments for methods.

## Value

An `sm_secret` object representing the updated secret.
