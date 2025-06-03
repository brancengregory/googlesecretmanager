#' Get IAM Policy for a Secret
#'
#' Retrieves the IAM policy for a given secret.
#' Implements `projects.secrets.getIamPolicy`.
#'
#' @param project_id Your Google Cloud project ID.
#' @param secret_id The ID of the secret.
#' @param requested_policy_version The version of IAM policy format. Defaults to 3.
#' @param token An authentication token. If `NULL`, `get_sm_token()` is called.
#' @return A list representing the IAM policy.
#' @export
#' @examples
#' \dontrun{
#'   secretmanager_auth()
#'   policy <- get_secret_iam_policy("your-project-id", "my-secret")
#'   print(policy)
#' }
get_secret_iam_policy <- function(project_id, secret_id, requested_policy_version = 3, token = NULL) {
  stopifnot(is.character(project_id) && nzchar(project_id), is.character(secret_id) && nzchar(secret_id))
  if (is.null(token)) token <- get_sm_token()

  resource_name <- sm_secret_name(project_id, secret_id)
  url <- sprintf("%s/%s:getIamPolicy", BASE_URL_SM, resource_name)
  
  # The API docs suggest POST for getIamPolicy if options are sent,
  # but options can also be sent via GET's query parameters for simple cases.
  # For consistency with setIamPolicy (which is POST), let's use POST.
  body <- list(options = list(requestedPolicyVersion = requested_policy_version))

  req <- httr2::request(url) |>
    gargle::req_gargle_token(token = token) |>
    httr2::req_method("POST") |> # API docs show POST for getIamPolicy with options
    httr2::req_body_json(data = body) |>
    httr2::req_error(body = sm_error_body)

  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp)
}

#' Set IAM Policy for a Secret
#'
#' Sets the IAM policy for a given secret. This overwrites the existing policy.
#' To modify, it's often best to get the current policy, modify it, then set it.
#' Implements `projects.secrets.setIamPolicy`.
#'
#' @param project_id Your Google Cloud project ID.
#' @param secret_id The ID of the secret.
#' @param policy A list object representing the IAM policy (usually containing `bindings` and `etag`).
#'   Get the current policy using `get_secret_iam_policy`, modify, and pass back.
#' @param update_mask (Optional) A FieldMask specifying which fields of the policy to modify. Only "bindings" and "etag" are supported.
#' @param token An authentication token. If `NULL`, `get_sm_token()` is called.
#' @return A list representing the updated IAM policy.
#' @export
#' @examples
#' \dontrun{
#'   secretmanager_auth()
#'   # First, get the current policy
#'   current_policy <- get_secret_iam_policy("your-project-id", "my-secret")
#'
#'   # Example: Add a new member to a role (or create role if not exists)
#'   new_binding <- list(
#'     role = "roles/secretmanager.secretAccessor",
#'     members = list("serviceAccount:my-sa@your-project-id.iam.gserviceaccount.com")
#'   )
#'   # Ensure bindings is a list
#'   if (is.null(current_policy$bindings)) current_policy$bindings <- list()
#'
#'   # Check if role already exists, if so, append member, else add new binding
#'   role_exists <- FALSE
#'   if (length(current_policy$bindings) > 0) {
#'     for (i in seq_along(current_policy$bindings)) {
#'       if (current_policy$bindings[[i]]$role == new_binding$role) {
#'         current_policy$bindings[[i]]$members <- unique(c(current_policy$bindings[[i]]$members, new_binding$members))
#'         role_exists <- TRUE
#'         break
#'       }
#'     }
#'   }
#'   if (!role_exists) {
#'     current_policy$bindings[[length(current_policy$bindings) + 1]] <- new_binding
#'   }
#'   # The version field should be set according to the policy version you want to set.
#'   # If retrieved policy has version 3, use version 3.
#'   current_policy$version <- 3 # Or current_policy$version if already set
#'
#'   # Set the updated policy
#'   updated_policy <- set_secret_iam_policy("your-project-id", "my-secret", current_policy)
#'   print(updated_policy)
#' }
set_secret_iam_policy <- function(project_id, secret_id, policy, update_mask = NULL, token = NULL) {
  stopifnot(is.character(project_id) && nzchar(project_id), is.character(secret_id) && nzchar(secret_id))
  if (is.null(token)) token <- get_sm_token()

  resource_name <- sm_secret_name(project_id, secret_id)
  url <- sprintf("%s/%s:setIamPolicy", BASE_URL_SM, resource_name)
  
  body <- prepare_iam_policy_body(policy, update_mask)

  req <- httr2::request(url) |>
    gargle::req_gargle_token(token = token) |>
    httr2::req_method("POST") |>
    httr2::req_body_json(data = body) |>
    httr2::req_error(body = sm_error_body)

  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp)
}

#' Test IAM Permissions for a Secret
#'
#' Tests the caller's permissions on a specified secret.
#' Implements `projects.secrets.testIamPermissions`.
#'
#' @param project_id Your Google Cloud project ID.
#' @param secret_id The ID of the secret.
#' @param permissions A character vector of permissions to test (e.g., "secretmanager.secrets.access").
#' @param token An authentication token. If `NULL`, `get_sm_token()` is called.
#' @return A list with a `permissions` field, containing the subset of tested permissions that the caller has.
#' @export
#' @examples
#' \dontrun{
#'   secretmanager_auth()
#'   perms_to_test <- c("secretmanager.secrets.get", "secretmanager.versions.access")
#'   allowed_perms <- test_secret_iam_permissions("your-project-id", "my-secret", perms_to_test)
#'   print(allowed_perms)
#' }
test_secret_iam_permissions <- function(project_id, secret_id, permissions, token = NULL) {
  stopifnot(
    is.character(project_id) && nzchar(project_id),
    is.character(secret_id) && nzchar(secret_id),
    is.character(permissions) && length(permissions) > 0
  )
  if (is.null(token)) token <- get_sm_token()

  resource_name <- sm_secret_name(project_id, secret_id)
  url <- sprintf("%s/%s:testIamPermissions", BASE_URL_SM, resource_name)
  
  body <- list(permissions = permissions)

  req <- httr2::request(url) |>
    gargle::req_gargle_token(token = token) |>
    httr2::req_method("POST") |>
    httr2::req_body_json(data = body) |>
    httr2::req_error(body = sm_error_body)

  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp)
}