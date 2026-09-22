# Create a Secret

Creates a new Secret containing no SecretVersions.

## Usage

``` r
sm_secret_create(
  secret_id,
  project_id = sm_project_get(),
  replication = list(automatic = list()),
  labels = NULL,
  ...
)
```

## Arguments

- secret_id:

  Required. A unique identifier for the secret within the project. Must
  be a string with a maximum length of 255 characters and can contain
  uppercase and lowercase letters, numerals, and the hyphen (`-`) and
  underscore (`_`) characters.

- project_id:

  The Google Cloud Project ID. Defaults to `sm_project_get()`.

- replication:

  Required. The replication policy for the secret data. Must be a list
  with either `automatic` or `user_managed` configuration.

- labels:

  Optional. Labels to attach to the secret.

- ...:

  Additional arguments for methods.

## Value

An `sm_secret` object representing the created secret.
