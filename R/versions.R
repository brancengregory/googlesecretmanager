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
    map_chr(\(x) basename(x$name))

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

  secret <- b64::decode_as_string(resp$payload$data)

  return(secret)
}
