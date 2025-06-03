#' @importFrom jsonlite base64_dec
#' @importFrom httr2 request req_perform resp_body_json req_error
#' @importFrom gargle req_gargle_token
NULL

#' Access a Secret Version Payload from Google Secret Manager
#'
#' Retrieves the payload of a specified secret version. The payload is
#' base64 decoded and returned as a character string. This is a convenience
#' function for `projects.secrets.versions.access`.
#'
#' @param project_id Your Google Cloud project ID.
#' @param secret_id The ID of the secret (the short name).
#' @param version_id The version of the secret. Defaults to "latest".
#'   Can be a version number (e.g., "1", "2") or "latest".
#' @param token A token object from `secretmanager_auth()` or `gargle::token_fetch()`.
#'              If `NULL`, `get_sm_token()` is called.
#' @return The secret payload as a character string.
#' @export
#' @examples
#' \dontrun{
#'   secretmanager_auth() # Ensure authenticated
#'   payload <- get_secret_payload("your-project-id", "my-secret", "latest")
#'   cat(payload)
#' }
get_secret_payload <- function(project_id, secret_id, version_id = "latest", token = NULL) {
  stopifnot(
    is.character(project_id) && length(project_id) == 1 && nzchar(project_id),
    is.character(secret_id) && length(secret_id) == 1 && nzchar(secret_id),
    is.character(version_id) && length(version_id) == 1 && nzchar(version_id)
  )

  if (is.null(token)) {
    token <- get_sm_token()
  }

  url <- sprintf(
    "%s/%s:access",
    BASE_URL_SM,
    sm_version_name(project_id, secret_id, version_id)
  )

  req <- httr2::request(url) |>
    gargle::req_gargle_token(token = token) |>
    httr2::req_error(body = sm_error_body)

  resp <- httr2::req_perform(req)
  response_content <- httr2::resp_body_json(resp)

  if (is.null(response_content$payload$data)) {
    stop("Secret payload data is missing in the API response for secret '", secret_id, "'.", call. = FALSE)
  }

  decoded_payload <- rawToChar(jsonlite::base64_dec(response_content$payload$data))
  decoded_payload
}

#' List Secrets in a Google Cloud Project
#'
#' Retrieves a list of secrets (metadata) available in the specified project.
#' Implements `projects.secrets.list`.
#'
#' @param project_id Your Google Cloud project ID.
#' @param page_size The maximum number of results to return in a single page.
#'   Defaults to `NULL` (server-side default, often 25).
#' @param page_token Token to retrieve a specific page of results.
#' @param filter Optional. Filter string. See Secret Manager documentation for syntax.
#'               Example: "labels.env=prod"
#' @param token A token object. If `NULL`, `get_sm_token()` is called.
#' @return A data frame with information about the secrets, or `NULL` invisibly
#'   if no secrets are found. Includes a `nextPageToken` attribute if more results exist.
#' @export
#' @examples
#' \dontrun{
#'   secretmanager_auth()
#'   secrets <- list_secrets("your-project-id")
#'   if (!is.null(secrets)) print(secrets)
#'
#'   # Paginate
#'   # page1 <- list_secrets("your-project-id", page_size = 5)
#'   # if (!is.null(attr(page1, "nextPageToken"))) {
#'   #   page2 <- list_secrets("your-project-id", page_size = 5,
#'   #                         page_token = attr(page1, "nextPageToken"))
#'   # }
#' }
list_secrets <- function(project_id, page_size = NULL, page_token = NULL, filter = NULL, token = NULL) {
  stopifnot(is.character(project_id) && length(project_id) == 1 && nzchar(project_id))

  if (is.null(token)) {
    token <- get_sm_token()
  }

  url <- sprintf("%s/%s/secrets", BASE_URL_SM, sm_project_name(project_id))
  
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

  if (is.null(response_content$secrets) || length(response_content$secrets) == 0) {
    message("No secrets found in project '", project_id, "' matching filter.")
    return(invisible(NULL))
  }

  secrets_list_df <- lapply(response_content$secrets, function(s) {
    data.frame(
      name = s$name,
      short_name = sub(paste0("^", sm_project_name(project_id), "/secrets/"), "", s$name),
      create_time = s$createTime,
      labels = if (!is.null(s$labels)) jsonlite::toJSON(s$labels, auto_unbox = TRUE) else NA_character_,
      replication = if(!is.null(s$replication)) jsonlite::toJSON(s$replication, auto_unbox = TRUE) else NA_character_,
      stringsAsFactors = FALSE
    )
  })
  
  result_df <- do.call(rbind, secrets_list_df)
  
  if (!is.null(response_content$nextPageToken)) {
    attr(result_df, "nextPageToken") <- response_content$nextPageToken
  }
  
  result_df
}

# R/secrets_manage.R
# Functions for managing secrets: create, get metadata, update, delete, IAM.

#' @importFrom httr2 request req_method req_body_json req_url_query req_perform resp_body_json req_error
#' @importFrom gargle req_gargle_token
#' @importFrom jsonlite toJSON
NULL

#' Create a New Secret
#'
#' Creates a new secret in the specified project with the given ID and replication policy.
#' Implements `projects.secrets.create`.
#'
#' @param project_id Your Google Cloud project ID.
#' @param secret_id The ID for the new secret.
#' @param replication A list describing the replication policy.
#'   Example for automatic: `list(automatic = list())`.
#'   Example for user-managed: `list(userManaged = list(replicas = list(list(location = "us-east1"))))`.
#' @param labels A named list of labels to apply to the secret (optional).
#' @param etag Etag for optimistic concurrency control (optional).
#' @param expire_time Timestamp in RFC3339 UTC "Zulu" format when the secret is scheduled to expire (optional).
#' @param ttl The TTL for the Secret (e.g., "172800s" for 48 hours). (optional)
#' @param topics A list of Pub/Sub topics for secret rotation notifications (optional). Each element should be `list(name="projects/PROJECT/topics/TOPIC")`.
#' @param version_aliases A named list of version aliases (optional). e.g. `list(prod = "3")`
#' @param annotations A named list of annotations (optional).
#' @param version_labels A named list of labels to apply to the initial version (optional).
#' @param token An authentication token. If `NULL`, `get_sm_token()` is called.
#' @return A list representing the created secret metadata.
#' @export
#' @examples
#' \dontrun{
#'   secretmanager_auth()
#'   # Automatic replication
#'   new_secret <- create_secret("your-project-id", "my-new-secret",
#'                               replication = list(automatic = list()),
#'                               labels = list(env = "dev", app = "billing"))
#'   print(new_secret)
#'
#'   # User-managed replication
#'   # repl <- list(userManaged = list(
#'   #   replicas = list(list(location = "us-central1"), list(location = "us-east1"))
#'   # ))
#'   # new_secret_user <- create_secret("your-project-id", "my-user-replicated-secret", repl)
#'   # print(new_secret_user)
#' }
create_secret <- function(project_id, secret_id, replication, labels = NULL,
                          etag = NULL, expire_time = NULL, ttl = NULL, topics = NULL,
                          version_aliases = NULL, annotations = NULL, version_labels = NULL,
                          token = NULL) {
  stopifnot(
    is.character(project_id) && nzchar(project_id),
    is.character(secret_id) && nzchar(secret_id),
    is.list(replication) && (("automatic" %in% names(replication)) || ("userManaged" %in% names(replication)))
  )

  if (is.null(token)) token <- get_sm_token()

  url <- sprintf("%s/%s/secrets", BASE_URL_SM, sm_project_name(project_id))
  
  body <- list(replication = replication)
  if (!is.null(labels)) body$labels <- labels
  if (!is.null(etag)) body$etag <- etag
  if (!is.null(expire_time)) body$expireTime <- expire_time
  if (!is.null(ttl)) body$ttl <- ttl
  if (!is.null(topics)) body$topics <- topics
  if (!is.null(version_aliases)) body$versionAliases <- version_aliases
  if (!is.null(annotations)) body$annotations <- annotations
  if (!is.null(version_labels)) body$versionLabels <- version_labels # Applied to the first auto-created version 0

  req <- httr2::request(url) |>
    gargle::req_gargle_token(token = token) |>
    httr2::req_url_query(secretId = secret_id) |>
    httr2::req_method("POST") |>
    httr2::req_body_json(data = body) |>
    httr2::req_error(body = sm_error_body)

  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp)
}

#' Get Secret Metadata
#'
#' Retrieves metadata for a specific secret (not its payload).
#' Implements `projects.secrets.get`.
#'
#' @param project_id Your Google Cloud project ID.
#' @param secret_id The ID of the secret.
#' @param token An authentication token. If `NULL`, `get_sm_token()` is called.
#' @return A list containing the secret's metadata.
#' @export
#' @examples
#' \dontrun{
#'   secretmanager_auth()
#'   metadata <- get_secret_metadata("your-project-id", "my-secret")
#'   print(metadata)
#' }
get_secret_metadata <- function(project_id, secret_id, token = NULL) {
  stopifnot(is.character(project_id) && nzchar(project_id), is.character(secret_id) && nzchar(secret_id))
  if (is.null(token)) token <- get_sm_token()

  url <- sprintf("%s/%s", BASE_URL_SM, sm_secret_name(project_id, secret_id))

  req <- httr2::request(url) |>
    gargle::req_gargle_token(token = token) |>
    httr2::req_error(body = sm_error_body)

  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp)
}

#' Update Secret Metadata
#'
#' Updates metadata (e.g., labels, replication) of an existing secret.
#' Implements `projects.secrets.patch`.
#'
#' @param project_id Your Google Cloud project ID.
#' @param secret_id The ID of the secret to update.
#' @param labels A named list of labels to set. To remove all labels, pass an empty list. (optional)
#' @param replication Replication policy object. (optional)
#' @param etag Etag for optimistic concurrency control (optional).
#' @param expire_time Timestamp for secret expiration (optional).
#' @param ttl Time To Live for the secret (optional).
#' @param topics List of Pub/Sub topics (optional).
#' @param version_aliases Version aliases (optional).
#' @param annotations Annotations (optional).
#' @param version_labels Version labels (optional).
#' @param update_mask A character vector of field names to update (e.g., "labels", "replication").
#'   If `NULL`, the function will try to infer it from non-NULL arguments (labels, replication etc.).
#' @param token An authentication token. If `NULL`, `get_sm_token()` is called.
#' @return A list representing the updated secret metadata.
#' @export
#' @examples
#' \dontrun{
#'   secretmanager_auth()
#'   updated_secret <- update_secret_metadata("your-project-id", "my-secret",
#'                                            labels = list(env = "prod", status = "active"),
#'                                            update_mask = "labels")
#'   print(updated_secret)
#'
#'   # To clear labels:
#'   # update_secret_metadata("your-project-id", "my-secret", labels = list(), update_mask = "labels")
#' }
update_secret_metadata <- function(project_id, secret_id,
                                   labels = NULL, replication = NULL, etag = NULL,
                                   expire_time = NULL, ttl = NULL, topics = NULL,
                                   version_aliases = NULL, annotations = NULL, version_labels = NULL,
                                   update_mask = NULL, token = NULL) {
  stopifnot(is.character(project_id) && nzchar(project_id), is.character(secret_id) && nzchar(secret_id))
  if (is.null(token)) token <- get_sm_token()

  url <- sprintf("%s/%s", BASE_URL_SM, sm_secret_name(project_id, secret_id))
  
  body_fields <- list(
    labels = labels, replication = replication, etag = etag,
    expireTime = expire_time, ttl = ttl, topics = topics,
    versionAliases = version_aliases, annotations = annotations, versionLabels = version_labels
  )
  
  # Filter out NULLs to only include fields intended for update in the body
  body <- Filter(Negate(is.null), body_fields)

  if (is.null(update_mask)) {
    # Infer update_mask from non-NULL arguments provided for the body
    # Translate R argument names to API field names if different (like expire_time to expireTime)
    api_field_names <- list(labels="labels", replication="replication", etag="etag",
                            expire_time="expireTime", ttl="ttl", topics="topics",
                            version_aliases="versionAliases", annotations="annotations", version_labels="versionLabels")
    
    provided_args <- names(which(!sapply(list(labels = labels, replication = replication, etag = etag,
                                               expire_time = expire_time, ttl = ttl, topics = topics,
                                               version_aliases = version_aliases, annotations = annotations, version_labels = version_labels), is.null)))
    update_mask <- unlist(api_field_names[provided_args])
  }
  
  if (length(body) == 0 || length(update_mask) == 0) {
    stop("No fields to update were specified either in arguments or via update_mask.", call. = FALSE)
  }
  
  # The API expects the 'secret' object to be nested under a 'secret' key in the PATCH body,
  # and the 'updateMask' as a query parameter.
  patch_body <- list(secret = body)

  req <- httr2::request(url) |>
    gargle::req_gargle_token(token = token) |>
    httr2::req_method("PATCH") |>
    httr2::req_url_query(updateMask = paste(update_mask, collapse = ",")) |>
    httr2::req_body_json(data = patch_body) |> # API expects { "secret": { ... }, "updateMask": "..." }
    httr2::req_error(body = sm_error_body)

  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp)
}


#' Delete a Secret
#'
#' Deletes an entire secret and all its versions. This is irreversible.
#' Implements `projects.secrets.delete`.
#'
#' @param project_id Your Google Cloud project ID.
#' @param secret_id The ID of the secret to delete.
#' @param etag Etag for optimistic concurrency control (optional).
#' @param token An authentication token. If `NULL`, `get_sm_token()` is called.
#' @return An empty list if successful (API returns empty body on success).
#' @export
#' @examples
#' \dontrun{
#'   secretmanager_auth()
#'   # delete_secret("your-project-id", "my-secret-to-delete")
#' }
delete_secret <- function(project_id, secret_id, etag = NULL, token = NULL) {
  stopifnot(is.character(project_id) && nzchar(project_id), is.character(secret_id) && nzchar(secret_id))
  if (is.null(token)) token <- get_sm_token()

  url <- sprintf("%s/%s", BASE_URL_SM, sm_secret_name(project_id, secret_id))
  
  query_params <- list()
  if (!is.null(etag)) query_params$etag <- etag

  req <- httr2::request(url) |>
    gargle::req_gargle_token(token = token) |>
    httr2::req_method("DELETE") |>
    httr2::req_url_query(!!!query_params) |>
    httr2::req_error(body = sm_error_body)

  resp <- httr2::req_perform(req)
  # Successful DELETE often returns 200 OK with empty body or 204 No Content
  # httr2::resp_body_json(resp) might error if body is truly empty and not "{}".
  if (httr2::resp_status(resp) %in% c(200, 204)) {
    return(invisible(list())) # Return an empty list, invisibly for successful deletion
  } else {
    # This case should ideally be caught by req_error, but as a fallback:
    return(httr2::resp_body_json(resp)) # Or handle as an error
  }
}
