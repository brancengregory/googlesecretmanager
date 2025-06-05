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
#' @param page_size Optional. The maximum number of results to return per page.
#' @param page_token Optional. A token received from a previous `list_secrets`
#'   call, used to retrieve the next page.
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
    map_chr(\(x) x$name)

  return(secrets)
}
