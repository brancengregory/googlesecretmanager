# R/secrets.R
#' @importFrom httr stop_for_status content http_type
#' @importFrom jsonlite fromJSON
#' @importFrom gargle request_build request_make response_process
#' @importFrom rlang %||%
NULL

# Internal constructor function (unexported)
# This function creates a new sm_secret object from raw API response data.
# It takes the parsed JSON response of a 'Secret' resource.
new_sm_secret <- function(x) {
  # Extract and structure relevant fields from the API's 'Secret' response.
  # We'll include key fields and some derived for convenience.

  # Example 'name' format: "projects/PROJECT_ID/secrets/SECRET_ID"
  # Or: "projects/PROJECT_ID/locations/LOCATION_ID/secrets/SECRET_ID"
  # We need to parse these out.
  resource_name <- x$name
  parts <- strsplit(resource_name, "/")[[1]]

  # Extract project_id and secret_id based on common patterns
  project_id <- if (length(parts) >= 2 && parts[1] == "projects") parts[2] else NA_character_
  secret_id <- if (length(parts) >= 4 && parts[3] == "secrets") parts[4] else NA_character_

  # For regional secrets, also extract location_id
  location_id <- if (length(parts) >= 4 && parts[3] == "locations") parts[4] else NA_character_


  obj_data <- list(
    # Core identifying information
    name = resource_name,
    secret_id = secret_id,
    project_id = project_id,
    location_id = location_id, # Will be NA if not a regional secret name

    # Key metadata fields from the Secret schema
    replication = x$replication,
    createTime = x$createTime,
    labels = x$labels,
    etag = x$etag,
    # Add other fields you deem important, e.g.:
    # topics = x$topics,
    # expireTime = x$expireTime,
    # rotation = x$rotation,
    # versionAliases = x$versionAliases,
    # annotations = x$annotations,

    # Placeholder for versions: initially empty, populated by listing or adding versions
    # versions = list(), # This will store metadata about its versions if fetched separately

    # Keep the full raw response for completeness/debugging
    raw_api_response = x
  )

  # Assign the S3 classes: 'sm_secret' first, then 'list' for list-like behavior.
  structure(obj_data, class = c("sm_secret", "list"))
}

new_sm_tbl <- function(x) {
  tbl <- dplyr::as_tibble(x)
  structure(x, class = c("sm_tbl", class(x)))
}

#' @title Print sm_secret
#' @param x A sm_secret object
#' @param ... Additional arguments passed to method
#' @export
print.sm_secret <- function(x, ...) {
  cli::cli_h1("sm_secret Object")
  cli::cli_li("Secret ID: {.val {x$secret_id}}")
  cli::cli_li("Project: {.val {x$project_id}}")
  cli::cli_li("Name: {.val {x$name}}")
  cli::cli_li("Created: {.val {x$createTime}}")
  if (!is.null(x$labels) && length(x$labels) > 0) {
    cli::cli_li("Labels: {.val {paste(names(x$labels), x$labels, sep = '=', collapse = '; ')}}")
  } else {
    cli::cli_li("Labels: {.emph None}")
  }
  invisible(x)
}

#' @title Print sm_tbl
#' @param x A sm_tbl object
#' @param ... Additional arguments passed to method
#' @export
print.sm_tbl <- function(x, ...) {
  cli::cli_h1("sm_tbl Object")
  cli::cli_text("A tibble of {nrow(x)} rows and {ncol(x)} columns.")
  print(dplyr::as_tibble(x)) # Use tibble's print method for actual data
  invisible(x)
}

#' As Secret
#'
#' @param x A string
#' @param ... Unused argument needed for method
#'
#' @export
as_secret <- function(x, ...) {
  UseMethod("as_secret")
}

#' As Secret Character
#'
#' @param x A string
#' @param ... Unused argument needed for method
#'
#' @export
as_secret.character <- function(x, ...) {
  new_sm_secret(x)
}

#' Get Secret Metadata
#'
#' Retrieves metadata for a specific Secret Manager secret.
#'
#' @param x The identifier for the secret. Can be a secret ID (character string)
#'   or an existing `sm_secret` object to refresh its metadata.
#' @param project_id The Google Cloud Project ID. Defaults to `sm_project_get()`.
#' @param ... Additional arguments for methods.
#'
#' @export
sm_secret_get <- function(x, project_id = sm_project_get(), ...) {
  UseMethod("sm_secret_get")
}

#' @rdname sm_secret_get
#' @export
sm_secret_get.character <- function(x, project_id = sm_project_get(), ...) {
  secret_id <- x # 'x' is the secret ID string

  if (is.null(project_id)) {
    cli::cli_abort("{.arg project_id} must be specified or set via {.fn sm_project_set}.")
  }

  # --- Inline API Call Logic ---
  # Construct the full resource name for the GET API call path
  resource_path <- paste0("projects/", project_id, "/secrets/", secret_id)

  cli::cli_alert_info("Retrieving secret {.val {secret_id}} from project {.val {project_id}}...")

  # Build the gargle request
  req <- gargle::request_build(
    method = "GET",
    path = paste0("/v1/", resource_path), # API path format: /v1/projects/.../secrets/...
    token = sm_token(),        # Get the gargle token
    base_url = "https://secretmanager.googleapis.com"
  )

  # Make the request and process the response
  resp <- gargle::request_make(req) |>
    gargle::response_process()

  # Error handling: Ensure the response is a list (expected for JSON)
  if (!is.list(resp)) {
    cli::cli_abort("API response for GetSecret was not a list. Received: {.val {typeof(resp)}}")
  }
  # --- End Inline API Call Logic ---

  # Create and return the sm_secret object using the internal constructor
  new_sm_secret(resp)
}

#' @rdname sm_secret_get
#' @export
sm_secret_get.sm_secret <- function(x, project_id = sm_project_get(), ...) {
  # 'x' is already an sm_secret object. We'll use its stored resource 'name'.
  resource_name <- x$name

  if (is.null(resource_name)) {
    cli::cli_abort("Cannot refresh secret metadata: {.arg secret} object is missing {.field name}.")
  }

  # Warn if a different project_id was passed, as we'll use the object's own project_id for consistency
  if (!is.null(project_id) && project_id != x$project_id) {
    cli::cli_alert_warning(
      "Provided {.arg project_id} ({.val {project_id}}) differs from object's project ({.val {x$project_id}}). Using object's project for refresh."
    )
  }
  # Use the project_id derived from the object's name for the API call
  project_id_from_object <- x$project_id

  # --- Inline API Call Logic ---
  cli::cli_alert_info("Refreshing secret {.val {x$secret_id}} in project {.val {project_id_from_object}}...")

  # Build the gargle request
  req <- gargle::request_build(
    method = "GET",
    path = paste0("/v1/", resource_name), # API path is the full resource name
    token = sm_token(),
    base_url = "https://secretmanager.googleapis.com"
  )

  # Make the request and process the response
  resp <- gargle::request_make(req) |>
    gargle::response_process()

  # Error handling
  if (!is.list(resp)) {
    cli::cli_abort("API response for GetSecret refresh was not a list. Received: {.val {typeof(resp)}}")
  }
  # --- End Inline API Call Logic ---

  # Create and return the refreshed sm_secret object
  new_sm_secret(resp)
}

#' List Secrets in a Project
#'
#' Lists metadata for all Secrets in a given Google Cloud Project.
#'
#' @param project_id The Google Cloud Project ID. Defaults to `sm_project_get()`.
#' @param filter Optional. A filter string, adhering to Secret Manager's
#'   [List-operation filtering rules](https://cloud.google.com/secret-manager/docs/filtering).
#' @param ... Additional arguments for methods.
#'
#' @export
sm_secret_ls <- function(project_id = sm_project_get(), filter = NULL, ...) {
  UseMethod("sm_secret_ls", project_id)
}

#' List Secrets in a Project
#'
#' Lists metadata for all Secrets in a given Google Cloud Project.
#'
#' @param project_id The Google Cloud Project ID. Defaults to `sm_project_get()`.
#' @param filter Optional. A filter string, adhering to Secret Manager's
#'   [List-operation filtering rules](https://cloud.google.com/secret-manager/docs/filtering).
#' @param ... Additional arguments for methods.
#'
#' @export
sm_secret_ls.character <- function(project_id = sm_project_get(), filter = NULL, ...) {
  if (is.null(project_id)) {
    cli::cli_abort("{.arg project_id} must be specified or set via {.fn sm_project_set} to list secrets.")
  }

  # Construct the parent resource name. This will be the value for the '{+parent}' part.
  parent_resource_name <- paste0("projects/", project_id)

  # --- CORRECTED API Call Logic ---
  cli::cli_alert_info("Listing secrets for project {.val {project_id}}...")

  # 1. Pre-construct the full API path using glue.
  #    This resolves the '{+parent}' placeholder before gargle::request_build sees it.
  api_path_full <- glue::glue("/v1/{parent_resource_name}/secrets")

  # 2. Build query parameters, ensuring NULLs are removed.
  #    These will now be the *only* parameters passed to the 'params' argument of request_build.
  query_params <- list(filter = filter)
  query_params <- query_params[!vapply(query_params, is.null, FUN.VALUE = logical(1))]

  # Build the gargle request
  req <- gargle::request_build(
    method = "GET",
    path = api_path_full, # Pass the fully constructed path
    params = query_params,  # Only query parameters go here
    token = sm_token(),
    base_url = "https://secretmanager.googleapis.com"
  )

  # Make the request and process the response
  resp <- gargle::request_make(req) |>
    gargle::response_process()

  # Check if 'secrets' field exists and is a list/data.frame as per ListSecretsResponse
  if (is.null(resp$secrets) || !is.list(resp$secrets)) {
    cli::cli_alert_warning("No secrets found or unexpected API response structure. Returning an empty {.cls sm_tbl}.")
    return(new_sm_tbl(dplyr::tibble())) # Return empty sm_tbl
  }
  # --- End Corrected API Call Logic ---

  # Convert each raw secret response into an sm_secret object, then combine into an sm_tbl
  secrets_list_of_objects <- resp$secrets |>
    purrr::map(new_sm_secret) # Map each raw secret response into an sm_secret object

  # Combine into a tibble, extracting relevant fields for columns
  secrets_tibble <- secrets_list_of_objects |>
    purrr::map_dfr(\(s) {
      dplyr::tibble(
        secret_id = s$secret_id,
        project_id = s$project_id,
        name = s$name,
        createTime = s$createTime,
        labels = list(s$labels), # Store labels as a list-column
        etag = s$etag
      )
    })

  # Create and return the sm_tbl object
  new_sm_tbl(secrets_tibble)
}

#' Create a Secret
#'
#' Creates a new Secret containing no SecretVersions.
#'
#' @param secret_id Required. A unique identifier for the secret within the project.
#'   Must be a string with a maximum length of 255 characters and can contain
#'   uppercase and lowercase letters, numerals, and the hyphen (`-`) and
#'   underscore (`_`) characters.
#' @param project_id The Google Cloud Project ID. Defaults to `sm_project_get()`.
#' @param replication Required. The replication policy for the secret data.
#'   Must be a list with either `automatic` or `user_managed` configuration.
#' @param labels Optional. Labels to attach to the secret.
#' @param ... Additional arguments for methods.
#'
#' @return An `sm_secret` object representing the created secret.
#' @export
sm_secret_create <- function(secret_id, project_id = sm_project_get(), replication = list(automatic = list()), labels = NULL, ...) {
  if (is.null(project_id)) {
    cli::cli_abort("{.arg project_id} must be specified or set via {.fn sm_project_set}.")
  }

  # Construct the parent resource name
  parent_resource_name <- paste0("projects/", project_id)

  # Validate replication configuration
  if (!is.list(replication) || (!"automatic" %in% names(replication) && !"user_managed" %in% names(replication))) {
    cli::cli_abort("Replication must be a list with either 'automatic' or 'user_managed' configuration.")
  }

  # Construct the request body
  body <- list(
    replication = replication,
    labels = labels
  )

  cli::cli_alert_info("Creating secret {.val {secret_id}} in project {.val {project_id}}...")

  # Build the request
  req <- gargle::request_build(
    method = "POST",
    path = paste0("/v1/", parent_resource_name, "/secrets"),
    params = list(secretId = secret_id),
    body = body,
    token = sm_token(),
    base_url = "https://secretmanager.googleapis.com"
  )

  # Make the request and process the response
  resp <- gargle::request_make(req) |>
    gargle::response_process()

  if (!is.list(resp)) {
    cli::cli_abort("API response for CreateSecret was not a list. Received: {.val {typeof(resp)}}")
  }

  new_sm_secret(resp)
}

#' Delete a Secret
#'
#' Deletes a Secret and all of its versions.
#'
#' @param secret The secret to delete. Can be a secret ID (character string)
#'   or an existing `sm_secret` object.
#' @param project_id The Google Cloud Project ID. Defaults to `sm_project_get()`.
#' @param ... Additional arguments for methods.
#'
#' @return Invisibly returns `NULL`.
#' @export
sm_secret_delete <- function(secret, project_id = sm_project_get(), ...) {
  UseMethod("sm_secret_delete")
}

#' @rdname sm_secret_delete
#' @export
sm_secret_delete.character <- function(secret, project_id = sm_project_get(), ...) {
  secret_id <- secret

  if (is.null(project_id)) {
    cli::cli_abort("{.arg project_id} must be specified or set via {.fn sm_project_set}.")
  }

  # Construct the resource name
  resource_name <- paste0("projects/", project_id, "/secrets/", secret_id)

  cli::cli_alert_info("Deleting secret {.val {secret_id}} from project {.val {project_id}}...")

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

  cli::cli_alert_success("Secret {.val {secret_id}} deleted successfully.")
  invisible(NULL)
}

#' @rdname sm_secret_delete
#' @export
sm_secret_delete.sm_secret <- function(secret, project_id = sm_project_get(), ...) {
  if (is.null(secret$name)) {
    cli::cli_abort("Invalid {.cls sm_secret} object. Missing {.field name} to delete.")
  }

  # Use the secret's own project_id for consistency
  if (!is.null(project_id) && project_id != secret$project_id) {
    cli::cli_alert_warning(
      "Provided {.arg project_id} ({.val {project_id}}) differs from object's project ({.val {secret$project_id}}). Using object's project for deletion."
    )
  }

  cli::cli_alert_info("Deleting secret {.val {secret$secret_id}} from project {.val {secret$project_id}}...")

  # Build the request using the secret's resource name
  req <- gargle::request_build(
    method = "DELETE",
    path = paste0("/v1/", secret$name),
    token = sm_token(),
    base_url = "https://secretmanager.googleapis.com"
  )

  # Make the request and process the response
  gargle::request_make(req) |>
    gargle::response_process()

  cli::cli_alert_success("Secret {.val {secret$secret_id}} deleted successfully.")
  invisible(NULL)
}

#' Update a Secret
#'
#' Updates metadata of an existing Secret.
#'
#' @param secret The secret to update. Can be a secret ID (character string)
#'   or an existing `sm_secret` object.
#' @param project_id The Google Cloud Project ID. Defaults to `sm_project_get()`.
#' @param replication Optional. The replication policy for the secret data.
#'   Must be a list with either `automatic` or `user_managed` configuration.
#' @param labels Optional. Labels to attach to the secret.
#' @param etag Optional. The etag of the secret. If provided, the update will
#'   only succeed if the secret's current etag matches this value.
#' @param ... Additional arguments for methods.
#'
#' @return An `sm_secret` object representing the updated secret.
#' @export
sm_secret_update <- function(secret, project_id = sm_project_get(), replication = NULL, labels = NULL, etag = NULL, ...) {
  UseMethod("sm_secret_update")
}

#' @rdname sm_secret_update
#' @export
sm_secret_update.character <- function(secret, project_id = sm_project_get(), replication = NULL, labels = NULL, etag = NULL, ...) {
  secret_id <- secret

  if (is.null(project_id)) {
    cli::cli_abort("{.arg project_id} must be specified or set via {.fn sm_project_set}.")
  }

  # First get the current secret to ensure it exists and get its etag
  current_secret <- sm_secret_get(secret_id, project_id)

  # Use the current secret's etag if none provided
  if (is.null(etag)) {
    etag <- current_secret$etag
  }

  # Construct the resource name
  resource_name <- paste0("projects/", project_id, "/secrets/", secret_id)

  # Build the update mask
  update_mask <- character()
  if (!is.null(replication)) update_mask <- c(update_mask, "replication")
  if (!is.null(labels)) update_mask <- c(update_mask, "labels")

  if (length(update_mask) == 0) {
    cli::cli_abort("No fields to update. Provide at least one of: replication, labels")
  }

  # Construct the request body
  body <- list(
    replication = replication,
    labels = labels,
    etag = etag
  )
  # Remove NULL values
  body <- body[!vapply(body, is.null, FUN.VALUE = logical(1))]

  cli::cli_alert_info("Updating secret {.val {secret_id}} in project {.val {project_id}}...")

  # Build the request
  req <- gargle::request_build(
    method = "PATCH",
    path = paste0("/v1/", resource_name),
    params = list(updateMask = paste(update_mask, collapse = ",")),
    body = body,
    token = sm_token(),
    base_url = "https://secretmanager.googleapis.com"
  )

  # Make the request and process the response
  resp <- gargle::request_make(req) |>
    gargle::response_process()

  if (!is.list(resp)) {
    cli::cli_abort("API response for UpdateSecret was not a list. Received: {.val {typeof(resp)}}")
  }

  new_sm_secret(resp)
}

#' @rdname sm_secret_update
#' @export
sm_secret_update.sm_secret <- function(secret, project_id = sm_project_get(), replication = NULL, labels = NULL, etag = NULL, ...) {
  if (is.null(secret$name)) {
    cli::cli_abort("Invalid {.cls sm_secret} object. Missing {.field name} to update.")
  }

  # Use the secret's own project_id for consistency
  if (!is.null(project_id) && project_id != secret$project_id) {
    cli::cli_alert_warning(
      "Provided {.arg project_id} ({.val {project_id}}) differs from object's project ({.val {secret$project_id}}). Using object's project for update."
    )
  }

  # Use the current secret's etag if none provided
  if (is.null(etag)) {
    etag <- secret$etag
  }

  # Build the update mask
  update_mask <- character()
  if (!is.null(replication)) update_mask <- c(update_mask, "replication")
  if (!is.null(labels)) update_mask <- c(update_mask, "labels")

  if (length(update_mask) == 0) {
    cli::cli_abort("No fields to update. Provide at least one of: replication, labels")
  }

  # Construct the request body
  body <- list(
    replication = replication,
    labels = labels,
    etag = etag
  )
  # Remove NULL values
  body <- body[!vapply(body, is.null, FUN.VALUE = logical(1))]

  cli::cli_alert_info("Updating secret {.val {secret$secret_id}} in project {.val {secret$project_id}}...")

  # Build the request
  req <- gargle::request_build(
    method = "PATCH",
    path = paste0("/v1/", secret$name),
    params = list(updateMask = paste(update_mask, collapse = ",")),
    body = body,
    token = sm_token(),
    base_url = "https://secretmanager.googleapis.com"
  )

  # Make the request and process the response
  resp <- gargle::request_make(req) |>
    gargle::response_process()

  if (!is.list(resp)) {
    cli::cli_abort("API response for UpdateSecret was not a list. Received: {.val {typeof(resp)}}")
  }

  new_sm_secret(resp)
}
