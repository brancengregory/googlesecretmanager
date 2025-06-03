#' @importFrom httr2 request req_method req_body_json req_url_query req_perform resp_body_json req_error
#' @importFrom gargle req_gargle_token
#' @importFrom jsonlite base64_enc
NULL

#' Add a New Secret Version
#'
#' Adds a new version with the given payload to an existing secret.
#' Implements `projects.secrets.addVersion`.
#'
#' @param project_id Your Google Cloud project ID.
#' @param secret_id The ID of the secret to add a version to.
#' @param payload_data The secret data as a character string or raw vector. Will be base64 encoded.
#' @param data_crc32c Optional. A CRC32C checksum of the `payload_data` (as a string).
#'   If provided, the Secret Manager API will verify it. You can use `digest::digest(payload_data, algo="crc32c", serialize=FALSE)`
#'   and ensure it's the string representation of the uint32 value.
#' @param version_labels A named list of labels to apply to the new version (optional).
#' @param token An authentication token. If `NULL`, `get_sm_token()` is called.
#' @return A list representing the metadata of the newly added secret version.
#' @export
#' @examples
#' \dontrun{
#'   secretmanager_auth()
#'   new_version <- add_secret_version("your-project-id", "my-secret",
#'                                     payload_data = "my new super secret value")
#'   print(new_version)
#'
#'   # With version labels
#'   # add_secret_version("your-project-id", "my-secret", "another value",
#'   #                    version_labels = list(release = "canary", "internal-id" = "v45-beta"))
#' }
add_secret_version <- function(project_id, secret_id, payload_data, data_crc32c = NULL, version_labels = NULL, token = NULL) {
  stopifnot(
    is.character(project_id) && nzchar(project_id),
    is.character(secret_id) && nzchar(secret_id)
    # payload_data checked in prepare_payload_body
  )
  if (is.null(token)) token <- get_sm_token()

  url <- sprintf("%s/%s:addVersion", BASE_URL_SM, sm_secret_name(project_id, secret_id))
  
  body <- prepare_payload_body(payload_data, data_crc32c, version_labels)

  req <- httr2::request(url) |>
    gargle::req_gargle_token(token = token) |>
    httr2::req_method("POST") |>
    httr2::req_body_json(data = body) |>
    httr2::req_error(body = sm_error_body)

  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp)
}

#' Get Secret Version Metadata
#'
#' Retrieves metadata for a specific secret version (not its payload).
#' To access the payload, use `get_secret_payload()`.
#' Implements `projects.secrets.versions.get`.
#'
#' @param project_id Your Google Cloud project ID.
#' @param secret_id The ID of the secret.
#' @param version_id The ID of the version (e.g., "1", "2", "latest").
#' @param token An authentication token. If `NULL`, `get_sm_token()` is called.
#' @return A list containing the secret version's metadata.
#' @export
#' @examples
#' \dontrun{
#'   secretmanager_auth()
#'   version_meta <- get_secret_version_metadata("your-project-id", "my-secret", "1")
#'   print(version_meta)
#'   latest_meta <- get_secret_version_metadata("your-project-id", "my-secret", "latest")
#'   print(latest_meta)
#' }
get_secret_version_metadata <- function(project_id, secret_id, version_id, token = NULL) {
  stopifnot(
    is.character(project_id) && nzchar(project_id),
    is.character(secret_id) && nzchar(secret_id),
    is.character(version_id) && nzchar(version_id)
  )
  if (is.null(token)) token <- get_sm_token()

  url <- sprintf("%s/%s", BASE_URL_SM, sm_version_name(project_id, secret_id, version_id))

  req <- httr2::request(url) |>
    gargle::req_gargle_token(token = token) |>
    httr2::req_error(body = sm_error_body)

  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp)
}

#' List Secret Versions
#'
#' Lists metadata of all versions for a given secret.
#' Implements `projects.secrets.versions.list`.
#'
#' @param project_id Your Google Cloud project ID.
#' @param secret_id The ID of the secret.
#' @param page_size The maximum number of results to return in a single page.
#'   Defaults to `NULL` (server-side default).
#' @param page_token Token to retrieve a specific page of results.
#' @param filter Optional. Filter string (e.g., "state=ENABLED").
#' @param token An authentication token. If `NULL`, `get_sm_token()` is called.
#' @return A data frame with metadata for each version, or `NULL` invisibly if none found.
#'   Includes a `nextPageToken` attribute if more results exist.
#' @export
#' @examples
#' \dontrun{
#'   secretmanager_auth()
#'   versions_df <- list_secret_versions("your-project-id", "my-secret")
#'   if(!is.null(versions_df)) print(versions_df)
#'
#'   # Filter for enabled versions
#'   # enabled_versions <- list_secret_versions("your-project-id", "my-secret", filter = "state=ENABLED")
#' }
list_secret_versions <- function(project_id, secret_id, page_size = NULL, page_token = NULL, filter = NULL, token = NULL) {
  stopifnot(
    is.character(project_id) && nzchar(project_id),
    is.character(secret_id) && nzchar(secret_id)
  )
  if (is.null(token)) token <- get_sm_token()

  url <- sprintf("%s/%s/versions", BASE_URL_SM, sm_secret_name(project_id, secret_id))
  
  query_params <- list()
  if (!is.null(page_size)) query_params$pageSize <- page_size
  if (!is.null(page_token)) query_params$pageToken <- page_token
  if (!is.null(filter)) query_params$filter <- filter

  req <- httr2::request(url) |>
    gargle::req_gargle_token(token = token) |>
    httr2::req_url_query(!!!query_params) |>
    httr2::req_error(body = sm_error_body)

  resp <- httr2::req_perform(req)
  response_content <- httr2::resp_body_json(resp)

  if (is.null(response_content$versions) || length(response_content$versions) == 0) {
    message("No versions found for secret '", secret_id, "' matching filter.")
    return(invisible(NULL))
  }

  versions_df_list <- lapply(response_content$versions, function(v) {
    data.frame(
      name = v$name,
      version_id = sub(paste0("^", sm_secret_name(project_id, secret_id), "/versions/"), "", v$name),
      create_time = v$createTime,
      state = v$state,
      replication_status = if(!is.null(v$replicationStatus)) jsonlite::toJSON(v$replicationStatus, auto_unbox=TRUE) else NA_character_,
      etag = v$etag,
      client_specified_payload_checksum = if(!is.null(v$clientSpecifiedPayloadChecksum)) v$clientSpecifiedPayloadChecksum else NA,
      scheduled_destroy_time = if(!is.null(v$scheduledDestroyTime)) v$scheduledDestroyTime else NA_character_,
      version_labels = if (!is.null(v$versionLabels)) jsonlite::toJSON(v$versionLabels, auto_unbox = TRUE) else NA_character_,
      stringsAsFactors = FALSE
    )
  })
  
  result_df <- do.call(rbind, versions_df_list)

  if (!is.null(response_content$nextPageToken)) {
    attr(result_df, "nextPageToken") <- response_content$nextPageToken
  }
  
  result_df
}

# --- Secret Version State Management ---

#' Disable a Secret Version
#'
#' Disables a specific secret version. Disabled versions cannot be accessed.
#' Implements `projects.secrets.versions.disable`.
#'
#' @param project_id Your Google Cloud project ID.
#' @param secret_id The ID of the secret.
#' @param version_id The ID of the version to disable.
#' @param etag Etag for optimistic concurrency control (optional).
#' @param token An authentication token. If `NULL`, `get_sm_token()` is called.
#' @return A list representing the metadata of the disabled secret version.
#' @export
#' @examples
#' \dontrun{
#'   secretmanager_auth()
#'   # disabled_version <- disable_secret_version("your-project-id", "my-secret", "2")
#'   # print(disabled_version)
#' }
disable_secret_version <- function(project_id, secret_id, version_id, etag = NULL, token = NULL) {
  stopifnot(
    is.character(project_id) && nzchar(project_id),
    is.character(secret_id) && nzchar(secret_id),
    is.character(version_id) && nzchar(version_id)
  )
  if (is.null(token)) token <- get_sm_token()

  url <- sprintf("%s/%s:disable", BASE_URL_SM, sm_version_name(project_id, secret_id, version_id))
  
  body <- list()
  if (!is.null(etag)) body$etag <- etag

  req <- httr2::request(url) |>
    gargle::req_gargle_token(token = token) |>
    httr2::req_method("POST") |>
    httr2::req_body_json(data = body) |> # API expects etag in body if provided
    httr2::req_error(body = sm_error_body)

  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp)
}

#' Enable a Secret Version
#'
#' Enables a specific secret version that was previously disabled.
#' Implements `projects.secrets.versions.enable`.
#'
#' @param project_id Your Google Cloud project ID.
#' @param secret_id The ID of the secret.
#' @param version_id The ID of the version to enable.
#' @param etag Etag for optimistic concurrency control (optional).
#' @param token An authentication token. If `NULL`, `get_sm_token()` is called.
#' @return A list representing the metadata of the enabled secret version.
#' @export
#' @examples
#' \dontrun{
#'   secretmanager_auth()
#'   # enabled_version <- enable_secret_version("your-project-id", "my-secret", "2")
#'   # print(enabled_version)
#' }
enable_secret_version <- function(project_id, secret_id, version_id, etag = NULL, token = NULL) {
  stopifnot(
    is.character(project_id) && nzchar(project_id),
    is.character(secret_id) && nzchar(secret_id),
    is.character(version_id) && nzchar(version_id)
  )
  if (is.null(token)) token <- get_sm_token()

  url <- sprintf("%s/%s:enable", BASE_URL_SM, sm_version_name(project_id, secret_id, version_id))
  
  body <- list()
  if (!is.null(etag)) body$etag <- etag

  req <- httr2::request(url) |>
    gargle::req_gargle_token(token = token) |>
    httr2::req_method("POST") |>
    httr2::req_body_json(data = body) |>
    httr2::req_error(body = sm_error_body)

  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp)
}

#' Destroy a Secret Version
#'
#' Destroys a specific secret version. The version's data is permanently deleted.
#' This is irreversible.
#' Implements `projects.secrets.versions.destroy`.
#'
#' @param project_id Your Google Cloud project ID.
#' @param secret_id The ID of the secret.
#' @param version_id The ID of the version to destroy.
#' @param etag Etag for optimistic concurrency control (optional).
#' @param token An authentication token. If `NULL`, `get_sm_token()` is called.
#' @return A list representing the metadata of the destroyed secret version (state will be DESTROYED).
#' @export
#' @examples
#' \dontrun{
#'   secretmanager_auth()
#'   # Be careful! This is permanent.
#'   # destroyed_version <- destroy_secret_version("your-project-id", "my-secret", "1")
#'   # print(destroyed_version)
#' }
destroy_secret_version <- function(project_id, secret_id, version_id, etag = NULL, token = NULL) {
  stopifnot(
    is.character(project_id) && nzchar(project_id),
    is.character(secret_id) && nzchar(secret_id),
    is.character(version_id) && nzchar(version_id)
  )
  if (is.null(token)) token <- get_sm_token()

  url <- sprintf("%s/%s:destroy", BASE_URL_SM, sm_version_name(project_id, secret_id, version_id))

  body <- list()
  if (!is.null(etag)) body$etag <- etag
  
  req <- httr2::request(url) |>
    gargle::req_gargle_token(token = token) |>
    httr2::req_method("POST") |>
    httr2::req_body_json(data = body) |>
    httr2::req_error(body = sm_error_body)

  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp)
}