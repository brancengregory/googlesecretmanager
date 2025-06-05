# R/secrets.R
#' @importFrom httr stop_for_status content http_type
#' @importFrom jsonlite fromJSON
#' @importFrom gargle request_build request_make response_process
#' @importFrom rlang %||%
NULL


#' List Google Cloud Secrets
#'
#' @description
#' Retrieves a list of secrets within a specified Google Cloud Project.
#'
#' This function leverages the authentication configured via [secretmanager_auth()].
#'
#' @param project_id The Google Cloud Project ID.
#' @param filter Optional. A filter expression to narrow down the results.
#'   See Google Cloud Secret Manager documentation for filter syntax.
#'
#' @return A list containing information about the secrets in the project.
#' @export
#' @examples
#' \dontrun{
#' # Ensure you've authenticated, e.g., via:
#' # secretmanager_auth()
#' # or with a specific token:
#' # token <- gargle::token_fetch(scopes = "https://www.googleapis.com/auth/cloud-platform")
#' # secretmanager_auth(token = token)
#'
#' # Replace 'your-gcp-project-id' with your actual project ID
#' my_secrets <- list_secrets(project_id = "your-gcp-project-id")
#' print(my_secrets)
#' }
list_secrets <- function(
    project_id,
    filter = NULL
) {

  # Define the Secret Manager API endpoint ID for listing secrets
  # endpoint_id <- "secretmanager.projects.secrets.list"

  # Prepare parameters for the API call. These names must match
  # the parameter names used in the hardcoded endpoint definition in
  # secretmanager_request_generate (e.g., 'parent', 'filter').
  params <- list(
    parent = paste0("projects/", project_id), # API expects 'parent' for project path
    filter = filter
  )

  # Generate the request using our internal helpers
  # req <- secretmanager_request_generate(
  #   endpoint = endpoint_id,
  #   params = params
  # )

  req <- gargle::request_build(
    method = "GET",
    path = "/v1/{parent}/secrets",
    params = params,
    token = secretmanager_token(),
    base_url = "https://secretmanager.googleapis.com"
  )

  # Make the HTTP request and process the response using gargle helpers
  # response_content <- secretmanager_request_make(req)

  resp <- gargle::request_make(req) |>
    gargle::response_process()

  secrets <- resp$secrets |>
    purrr::map_chr(\(x) x$name)

  return(secrets)
}

list_secret_versions <- function(
    secret_id,
    project_id,
    filter = NULL
) {

  # Define the Secret Manager API endpoint ID for listing secrets
  # endpoint_id <- "secretmanager.projects.secrets.list"

  # Prepare parameters for the API call. These names must match
  # the parameter names used in the hardcoded endpoint definition in
  # secretmanager_request_generate (e.g., 'parent', 'filter').
  params <- list(
    parent = paste0("projects/", project_id), # API expects 'parent' for project path
    secret = secret_id,
    filter = filter
  )

  # Generate the request using our internal helpers
  # req <- secretmanager_request_generate(
  #   endpoint = endpoint_id,
  #   params = params
  # )

  req <- gargle::request_build(
    method = "GET",
    path = "/v1/{parent}/secrets/{secret}/versions",
    params = params,
    token = secretmanager_token(),
    base_url = "https://secretmanager.googleapis.com"
  )

  # Make the HTTP request and process the response using gargle helpers
  # response_content <- secretmanager_request_make(req)

  resp <- gargle::request_make(req) |>
    gargle::response_process()

  secrets <- resp$secrets |>
    purrr::map_chr(\(x) basename(x$name))

  return(resp)
}

secret_version_latest <- function(
    secret_id,
    project_id,
    filter = NULL
) {

  # Define the Secret Manager API endpoint ID for listing secrets
  # endpoint_id <- "secretmanager.projects.secrets.list"

  # Prepare parameters for the API call. These names must match
  # the parameter names used in the hardcoded endpoint definition in
  # secretmanager_request_generate (e.g., 'parent', 'filter').
  params <- list(
    # parent = paste0("projects/", project_id), # API expects 'parent' for project path
    name = secret_id,
    filter = filter
  )

  # Generate the request using our internal helpers
  # req <- secretmanager_request_generate(
  #   endpoint = endpoint_id,
  #   params = params
  # )

  req <- gargle::request_build(
    method = "GET",
    path = "/v1/{name}/versions/latest:access",
    params = params,
    token = secretmanager_token(),
    base_url = "https://secretmanager.googleapis.com"
  )

  # Make the HTTP request and process the response using gargle helpers
  # response_content <- secretmanager_request_make(req)

  resp <- gargle::request_make(req) |>
    gargle::response_process()

  secret <- jsonlite::base64_dec(resp$payload$data) |>
    rawToChar()

  return(secret)
}


#' Create a Google Cloud Secret
#'
#' @description
#' Creates a new secret in the specified Google Cloud Project.
#'
#' @param project_id The Google Cloud Project ID.
#' @param secret_id The ID of the secret to create. Must be unique within the project.
#' @param labels Optional. A list of key-value pairs to attach as labels to the secret.
#' @param replication_policy Optional. A list defining the replication policy.
#'   For example, `list(automatic = list())` for automatic replication, or
#'   `list(userManaged = list(replicas = list(list(location = "us-east1"))))`
#'   for user-managed replication in "us-east1".
#' @param etag Optional. For optimistic concurrency control.
#' @param ttl Optional. The TTL (Time-To-Live) of the secret. Must be a string
#'   representing a duration, e.g., "3600s" for 1 hour.
#'
#' @return A list containing information about the newly created secret.
#' @export
#' @examples
#' \dontrun{
#' secret_info <- create_secret(
#'   project_id = "your-gcp-project-id",
#'   secret_id = "my-new-secret",
#'   labels = list(environment = "development"),
#'   replication_policy = list(automatic = list())
#' )
#' print(secret_info)
#' }
create_secret <- function(
    project_id,
    secret_id,
    labels = NULL,
    replication_policy = NULL,
    etag = NULL,
    ttl = NULL
) {
  parent_path <- paste0("projects/", project_id)

  # Use setNames(list(), character(0)) to force an empty object {} for 'automatic'
  # when serialized to JSON by jsonlite.
  default_automatic_policy <- list(automatic = empty_object())
  resolved_replication_policy <- replication_policy %||% default_automatic_policy

  body <- list(
    replication = resolved_replication_policy,
    labels = labels,
    etag = etag,
    ttl = ttl
  )
  body <- body[!vapply(body, is.null, FUN.VALUE = logical(1))]

  req <- gargle::request_build(
    method = "POST",
    path = "/v1/{parent}/secrets",
    params = list(parent = parent_path, secretId = secret_id),
    body = body,
    token = secretmanager_token(),
    base_url = "https://secretmanager.googleapis.com"
  )

  resp <- gargle::request_make(req) |>
    gargle::response_process()

  return(resp)
}

#' Add a Secret Version
#'
#' @description
#' Adds a new version to an existing secret.
#'
#' @param project_id The Google Cloud Project ID.
#' @param secret_id The ID of the secret to which the version will be added.
#' @param payload The secret data (as a raw vector or character string).
#' @param etag Optional. For optimistic concurrency control.
#'
#' @return A list containing information about the newly created secret version.
#' @export
#' @examples
#' \dontrun{
#' # Add a new version with string data
#' add_secret_version(
#'   project_id = "your-gcp-project-id",
#'   secret_id = "my-secret",
#'   payload = "my-new-secret-value"
#' )
#'
#' # Add a new version with raw data
#' add_secret_version(
#'   project_id = "your-gcp-project-id",
#'   secret_id = "my-secret",
#'   payload = charToRaw("binary_data")
#' )
#' }
add_secret_version <- function(
    project_id,
    secret_id,
    payload,
    etag = NULL
) {
  secret_path <- paste0("projects/", project_id, "/secrets/", secret_id)

  # Base64 encode the payload
  encoded_payload <- jsonlite::base64_enc(payload)

  body <- list(
    payload = list(data = encoded_payload),
    etag = etag
  )
  body <- body[!vapply(body, is.null, FUN.VALUE = logical(1))]

  req <- gargle::request_build(
    method = "POST",
    path = "/v1/{parent}:addVersion",
    params = list(parent = secret_path),
    body = body,
    token = secretmanager_token(),
    base_url = "https://secretmanager.googleapis.com"
  )

  resp <- gargle::request_make(req) |>
    gargle::response_process()

  return(resp)
}
