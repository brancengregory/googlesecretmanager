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

#' Get Secret Version Metadata
#'
#' Gets metadata for a specific Secret Version.
#'
#' @param secret The secret containing the version. Can be a secret ID (character string)
#'   or an existing `sm_secret` object.
#' @param version_id The version ID to get. Can be "latest" to get the latest version.
#' @param project_id The Google Cloud Project ID. Defaults to `sm_project_get()`.
#' @param ... Additional arguments for methods.
#'
#' @return An `sm_secret_version` object representing the version metadata.
#' @export
sm_secret_version_get <- function(secret, version_id, project_id = sm_project_get(), ...) {
  UseMethod("sm_secret_version_get")
}

#' @rdname sm_secret_version_get
#' @export
sm_secret_version_get.character <- function(secret, version_id, project_id = sm_project_get(), ...) {
  secret_id <- secret

  if (is.null(project_id)) {
    cli::cli_abort("{.arg project_id} must be specified or set via {.fn sm_project_set}.")
  }

  # Construct the resource name
  resource_name <- paste0("projects/", project_id, "/secrets/", secret_id, "/versions/", version_id)

  cli::cli_alert_info("Getting version {.val {version_id}} of secret {.val {secret_id}}...")

  # Build the request
  req <- gargle::request_build(
    method = "GET",
    path = paste0("/v1/", resource_name),
    token = sm_token(),
    base_url = "https://secretmanager.googleapis.com"
  )

  # Make the request and process the response
  resp <- gargle::request_make(req) |>
    gargle::response_process()

  if (!is.list(resp)) {
    cli::cli_abort("API response for GetSecretVersion was not a list. Received: {.val {typeof(resp)}}")
  }

  new_sm_secret_version(resp)
}

#' @rdname sm_secret_version_get
#' @export
sm_secret_version_get.sm_secret <- function(secret, version_id, project_id = sm_project_get(), ...) {
  if (is.null(secret$name)) {
    cli::cli_abort("Invalid {.cls sm_secret} object. Missing {.field name} to get version.")
  }

  # Use the secret's own project_id for consistency
  if (!is.null(project_id) && project_id != secret$project_id) {
    cli::cli_alert_warning(
      "Provided {.arg project_id} ({.val {project_id}}) differs from object's project ({.val {secret$project_id}}). Using object's project to get version."
    )
  }

  # Construct the resource name using the secret's name
  resource_name <- paste0(secret$name, "/versions/", version_id)

  cli::cli_alert_info("Getting version {.val {version_id}} of secret {.val {secret$secret_id}}...")

  # Build the request
  req <- gargle::request_build(
    method = "GET",
    path = paste0("/v1/", resource_name),
    token = sm_token(),
    base_url = "https://secretmanager.googleapis.com"
  )

  # Make the request and process the response
  resp <- gargle::request_make(req) |>
    gargle::response_process()

  if (!is.list(resp)) {
    cli::cli_abort("API response for GetSecretVersion was not a list. Received: {.val {typeof(resp)}}")
  }

  new_sm_secret_version(resp)
}

#' Add a Secret Version
#'
#' Adds a new version to an existing Secret.
#'
#' @param secret The secret to add a version to. Can be a secret ID (character string)
#'   or an existing `sm_secret` object.
#' @param payload The secret data to store. Will be base64 encoded.
#' @param project_id The Google Cloud Project ID. Defaults to `sm_project_get()`.
#' @param ... Additional arguments for methods.
#'
#' @return An `sm_secret_version` object representing the new version.
#' @export
sm_secret_version_add <- function(secret, payload, project_id = sm_project_get(), ...) {
  UseMethod("sm_secret_version_add")
}

#' @rdname sm_secret_version_add
#' @export
sm_secret_version_add.character <- function(secret, payload, project_id = sm_project_get(), ...) {
  secret_id <- secret

  if (is.null(project_id)) {
    cli::cli_abort("{.arg project_id} must be specified or set via {.fn sm_project_set}.")
  }

  # Construct the parent resource name
  parent_resource_name <- paste0("projects/", project_id, "/secrets/", secret_id)

  # Base64 encode the payload
  encoded_payload <- jsonlite::base64_enc(payload)

  # Construct the request body
  body <- list(
    payload = list(data = encoded_payload)
  )

  cli::cli_alert_info("Adding new version to secret {.val {secret_id}}...")

  # Build the request
  req <- gargle::request_build(
    method = "POST",
    path = paste0("/v1/", parent_resource_name, ":addVersion"),
    body = body,
    token = sm_token(),
    base_url = "https://secretmanager.googleapis.com"
  )

  # Make the request and process the response
  resp <- gargle::request_make(req) |>
    gargle::response_process()

  if (!is.list(resp)) {
    cli::cli_abort("API response for AddSecretVersion was not a list. Received: {.val {typeof(resp)}}")
  }

  new_sm_secret_version(resp)
}

#' @rdname sm_secret_version_add
#' @export
sm_secret_version_add.sm_secret <- function(secret, payload, project_id = sm_project_get(), ...) {
  if (is.null(secret$name)) {
    cli::cli_abort("Invalid {.cls sm_secret} object. Missing {.field name} to add version.")
  }

  # Use the secret's own project_id for consistency
  if (!is.null(project_id) && project_id != secret$project_id) {
    cli::cli_alert_warning(
      "Provided {.arg project_id} ({.val {project_id}}) differs from object's project ({.val {secret$project_id}}). Using object's project to add version."
    )
  }

  # Base64 encode the payload
  encoded_payload <- jsonlite::base64_enc(payload)

  # Construct the request body
  body <- list(
    payload = list(data = encoded_payload)
  )

  cli::cli_alert_info("Adding new version to secret {.val {secret$secret_id}}...")

  # Build the request
  req <- gargle::request_build(
    method = "POST",
    path = paste0("/v1/", secret$name, ":addVersion"),
    body = body,
    token = sm_token(),
    base_url = "https://secretmanager.googleapis.com"
  )

  # Make the request and process the response
  resp <- gargle::request_make(req) |>
    gargle::response_process()

  if (!is.list(resp)) {
    cli::cli_abort("API response for AddSecretVersion was not a list. Received: {.val {typeof(resp)}}")
  }

  new_sm_secret_version(resp)
}

#' Delete a Secret Version
#'
#' Deletes a Secret Version.
#'
#' @param secret The secret containing the version. Can be a secret ID (character string)
#'   or an existing `sm_secret` object.
#' @param version_id The version ID to delete.
#' @param project_id The Google Cloud Project ID. Defaults to `sm_project_get()`.
#' @param ... Additional arguments for methods.
#'
#' @return Invisibly returns `NULL`.
#' @export
sm_secret_version_delete <- function(secret, version_id, project_id = sm_project_get(), ...) {
  UseMethod("sm_secret_version_delete")
}

#' @rdname sm_secret_version_delete
#' @export
sm_secret_version_delete.character <- function(secret, version_id, project_id = sm_project_get(), ...) {
  secret_id <- secret

  if (is.null(project_id)) {
    cli::cli_abort("{.arg project_id} must be specified or set via {.fn sm_project_set}.")
  }

  # Construct the resource name
  resource_name <- paste0("projects/", project_id, "/secrets/", secret_id, "/versions/", version_id)

  cli::cli_alert_info("Deleting version {.val {version_id}} of secret {.val {secret_id}}...")

  # Build the request
  req <- gargle::request_build(
    method = "DELETE",
    path = paste0("/v1/", resource_name),
    token = sm_token(),
    base_url = "https://secretmanager.googleapis.com"
  )

  # Make the request and process the response
  gargle::request_make(req) |>
    gargle::response_process()

  cli::cli_alert_success("Version {.val {version_id}} of secret {.val {secret_id}} deleted successfully.")
  invisible(NULL)
}

#' @rdname sm_secret_version_delete
#' @export
sm_secret_version_delete.sm_secret <- function(secret, version_id, project_id = sm_project_get(), ...) {
  if (is.null(secret$name)) {
    cli::cli_abort("Invalid {.cls sm_secret} object. Missing {.field name} to delete version.")
  }

  # Use the secret's own project_id for consistency
  if (!is.null(project_id) && project_id != secret$project_id) {
    cli::cli_alert_warning(
      "Provided {.arg project_id} ({.val {project_id}}) differs from object's project ({.val {secret$project_id}}). Using object's project to delete version."
    )
  }

  # Construct the resource name using the secret's name
  resource_name <- paste0(secret$name, "/versions/", version_id)

  cli::cli_alert_info("Deleting version {.val {version_id}} of secret {.val {secret$secret_id}}...")

  # Build the request
  req <- gargle::request_build(
    method = "DELETE",
    path = paste0("/v1/", resource_name),
    token = sm_token(),
    base_url = "https://secretmanager.googleapis.com"
  )

  # Make the request and process the response
  gargle::request_make(req) |>
    gargle::response_process()

  cli::cli_alert_success("Version {.val {version_id}} of secret {.val {secret$secret_id}} deleted successfully.")
  invisible(NULL)
}

#' Enable a Secret Version
#'
#' Enables a Secret Version.
#'
#' @param secret The secret containing the version. Can be a secret ID (character string)
#'   or an existing `sm_secret` object.
#' @param version_id The version ID to enable.
#' @param project_id The Google Cloud Project ID. Defaults to `sm_project_get()`.
#' @param ... Additional arguments for methods.
#'
#' @return An `sm_secret_version` object representing the enabled version.
#' @export
sm_secret_version_enable <- function(secret, version_id, project_id = sm_project_get(), ...) {
  UseMethod("sm_secret_version_enable")
}

#' @rdname sm_secret_version_enable
#' @export
sm_secret_version_enable.character <- function(secret, version_id, project_id = sm_project_get(), ...) {
  secret_id <- secret

  if (is.null(project_id)) {
    cli::cli_abort("{.arg project_id} must be specified or set via {.fn sm_project_set}.")
  }

  # Construct the resource name
  resource_name <- paste0("projects/", project_id, "/secrets/", secret_id, "/versions/", version_id)

  cli::cli_alert_info("Enabling version {.val {version_id}} of secret {.val {secret_id}}...")

  # Build the request
  req <- gargle::request_build(
    method = "POST",
    path = paste0("/v1/", resource_name, ":enable"),
    token = sm_token(),
    base_url = "https://secretmanager.googleapis.com"
  )

  # Make the request and process the response
  resp <- gargle::request_make(req) |>
    gargle::response_process()

  if (!is.list(resp)) {
    cli::cli_abort("API response for EnableSecretVersion was not a list. Received: {.val {typeof(resp)}}")
  }

  new_sm_secret_version(resp)
}

#' @rdname sm_secret_version_enable
#' @export
sm_secret_version_enable.sm_secret <- function(secret, version_id, project_id = sm_project_get(), ...) {
  if (is.null(secret$name)) {
    cli::cli_abort("Invalid {.cls sm_secret} object. Missing {.field name} to enable version.")
  }

  # Use the secret's own project_id for consistency
  if (!is.null(project_id) && project_id != secret$project_id) {
    cli::cli_alert_warning(
      "Provided {.arg project_id} ({.val {project_id}}) differs from object's project ({.val {secret$project_id}}). Using object's project to enable version."
    )
  }

  # Construct the resource name using the secret's name
  resource_name <- paste0(secret$name, "/versions/", version_id)

  cli::cli_alert_info("Enabling version {.val {version_id}} of secret {.val {secret$secret_id}}...")

  # Build the request
  req <- gargle::request_build(
    method = "POST",
    path = paste0("/v1/", resource_name, ":enable"),
    token = sm_token(),
    base_url = "https://secretmanager.googleapis.com"
  )

  # Make the request and process the response
  resp <- gargle::request_make(req) |>
    gargle::response_process()

  if (!is.list(resp)) {
    cli::cli_abort("API response for EnableSecretVersion was not a list. Received: {.val {typeof(resp)}}")
  }

  new_sm_secret_version(resp)
}

#' Disable a Secret Version
#'
#' Disables a Secret Version.
#'
#' @param secret The secret containing the version. Can be a secret ID (character string)
#'   or an existing `sm_secret` object.
#' @param version_id The version ID to disable.
#' @param project_id The Google Cloud Project ID. Defaults to `sm_project_get()`.
#' @param ... Additional arguments for methods.
#'
#' @return An `sm_secret_version` object representing the disabled version.
#' @export
sm_secret_version_disable <- function(secret, version_id, project_id = sm_project_get(), ...) {
  UseMethod("sm_secret_version_disable")
}

#' @rdname sm_secret_version_disable
#' @export
sm_secret_version_disable.character <- function(secret, version_id, project_id = sm_project_get(), ...) {
  secret_id <- secret

  if (is.null(project_id)) {
    cli::cli_abort("{.arg project_id} must be specified or set via {.fn sm_project_set}.")
  }

  # Construct the resource name
  resource_name <- paste0("projects/", project_id, "/secrets/", secret_id, "/versions/", version_id)

  cli::cli_alert_info("Disabling version {.val {version_id}} of secret {.val {secret_id}}...")

  # Build the request
  req <- gargle::request_build(
    method = "POST",
    path = paste0("/v1/", resource_name, ":disable"),
    token = sm_token(),
    base_url = "https://secretmanager.googleapis.com"
  )

  # Make the request and process the response
  resp <- gargle::request_make(req) |>
    gargle::response_process()

  if (!is.list(resp)) {
    cli::cli_abort("API response for DisableSecretVersion was not a list. Received: {.val {typeof(resp)}}")
  }

  new_sm_secret_version(resp)
}

#' @rdname sm_secret_version_disable
#' @export
sm_secret_version_disable.sm_secret <- function(secret, version_id, project_id = sm_project_get(), ...) {
  if (is.null(secret$name)) {
    cli::cli_abort("Invalid {.cls sm_secret} object. Missing {.field name} to disable version.")
  }

  # Use the secret's own project_id for consistency
  if (!is.null(project_id) && project_id != secret$project_id) {
    cli::cli_alert_warning(
      "Provided {.arg project_id} ({.val {project_id}}) differs from object's project ({.val {secret$project_id}}). Using object's project to disable version."
    )
  }

  # Construct the resource name using the secret's name
  resource_name <- paste0(secret$name, "/versions/", version_id)

  cli::cli_alert_info("Disabling version {.val {version_id}} of secret {.val {secret$secret_id}}...")

  # Build the request
  req <- gargle::request_build(
    method = "POST",
    path = paste0("/v1/", resource_name, ":disable"),
    token = sm_token(),
    base_url = "https://secretmanager.googleapis.com"
  )

  # Make the request and process the response
  resp <- gargle::request_make(req) |>
    gargle::response_process()

  if (!is.list(resp)) {
    cli::cli_abort("API response for DisableSecretVersion was not a list. Received: {.val {typeof(resp)}}")
  }

  new_sm_secret_version(resp)
}
