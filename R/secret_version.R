# Internal constructor function (unexported)
# This function creates a new sm_secret_version object from raw API response data.
new_sm_secret_version <- function(x) {
  # Extract and structure relevant fields from the API's SecretVersion response.
  obj_data <- list(
    name = x$name, # projects/*/secrets/*/versions/*
    secret_id = sub(".*/secrets/(.*)/versions/.*", "\\1", x$name),
    version_id = sub(".*/versions/(.*)", "\\1", x$name),
    project_id = sub("projects/(.*)/secrets/.*", "\\1", x$name),
    createTime = x$createTime,
    destroyTime = x$destroyTime,
    state = x$state,
    etag = x$etag,
    response = x # Keep raw response for debugging/completeness
  )
  # Assign the S3 classes: sm_secret_version first, then list for list-like behavior.
  structure(obj_data, class = c("sm_secret_version", "list"))
}

#' List Secret Versions
#'
#' Lists metadata for all Secret Versions associated with a given Secret.
#'
#' @param secret The secret for which to list versions. Can be an `sm_secret`
#'   object or a character string representing the secret ID.
#' @param project_id The Google Cloud Project ID. Defaults to `sm_project_get()`.
#' @param filter Optional. A filter string for secret versions.
#' @param ... Additional arguments for methods.
#'
#' @export
sm_secret_version_ls <- function(secret, project_id = sm_project_get(), filter = NULL, ...) {
  UseMethod("sm_secret_version_ls")
}

#' @rdname sm_secret_version_ls
#' @export
sm_secret_version_ls.sm_secret <- function(secret, project_id = sm_project_get(), filter = NULL, ...) {
  # 'secret' is an sm_secret object. Use its 'name' field as the parent.
  parent_resource_name <- secret$name

  if (is.null(parent_resource_name)) {
    cli::cli_abort("Invalid {.cls sm_secret} object. Missing {.field name} to list versions.")
  }

  # Build query parameters, removing NULLs
  query_params <- list(filter = filter)
  query_params <- query_params[!vapply(query_params, is.null, FUN.VALUE = logical(1))]

  cli::cli_alert_info("Listing versions for secret {.val {secret$secret_id}}...")

  # Pre-construct the full API path using glue
  # Path from Discovery Doc: "/v1/{+parent}/versions"
  api_path_full <- glue::glue("/v1/{parent_resource_name}/versions")

  # Build the gargle request
  req <- gargle::request_build(
    method = "GET",
    path = api_path_full,
    params = query_params,
    token = sm_token(),
    base_url = "https://secretmanager.googleapis.com"
  )

  # Make the request and process the response
  resp <- gargle::request_make(req) |>
    gargle::response_process()

  # Check for 'versions' field in the response (ListSecretVersionsResponse)
  if (is.null(resp$versions) || !is.list(resp$versions)) {
    cli::cli_alert_warning("No versions found or unexpected API response structure. Returning an empty {.cls sm_tbl}.")
    return(new_sm_tbl(dplyr::tibble()))
  }

  # Convert each raw version response into an sm_secret_version object, then combine into an sm_tbl
  versions_list_of_objects <- resp$versions |>
    purrr::map(new_sm_secret_version) # Map each raw version response into an sm_secret_version object

  # Combine into a tibble, extracting relevant fields for columns
  versions_tibble <- versions_list_of_objects |>
    purrr::map_dfr(\(v) {
      dplyr::tibble(
        secret_id = v$secret_id,
        version_id = v$version_id,
        name = v$name,
        createTime = v$createTime,
        state = v$state,
        etag = v$etag
      )
    })

  new_sm_tbl(versions_tibble)
}

#' @rdname sm_secret_version_ls
#' @export
sm_secret_version_ls.character <- function(secret, project_id = sm_project_get(), filter = NULL, ...) {
  secret_id <- secret # 'secret' is the secret ID string

  if (is.null(project_id)) {
    cli::cli_abort("{.arg project_id} must be specified or set via {.fn sm_project_set} to list secret versions.")
  }

  # Construct the full resource name for the secret
  secret_resource_name <- paste0("projects/", project_id, "/secrets/", secret_id)

  # Now, call the sm_secret_version_ls.sm_secret method by creating a mock sm_secret object
  # for dispatch. This avoids code duplication.
  mock_secret_obj <- list(name = secret_resource_name, secret_id = secret_id, project_id = project_id)
  class(mock_secret_obj) <- c("sm_secret", "list")

  sm_secret_version_ls.sm_secret(secret = mock_secret_obj, filter = filter, ...)
}
