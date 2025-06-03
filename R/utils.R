# R/utils.R

BASE_URL_SM <- "https://secretmanager.googleapis.com/v1"

# Custom error handling function for httr2 requests
# Parses Google API error JSON for a more informative message.
sm_error_body <- function(resp) {
  body <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
  gcp_error_message <- NULL

  if (!is.null(body) && !is.null(body$error) && !is.null(body$error$message)) {
    gcp_error_message <- body$error$message
    if (!is.null(body$error$details) && length(body$error$details) > 0) {
      # Append details if available
      details_text <- sapply(body$error$details, function(d) {
        paste(names(d), d, sep = ": ", collapse = ", ")
      })
      gcp_error_message <- paste0(gcp_error_message, " (Details: ", paste(details_text, collapse = "; "), ")")
    }
  }
  
  final_message <- paste0(
    "Google Secret Manager API Error (HTTP ", httr2::resp_status(resp), ")"
  )
  if (!is.null(gcp_error_message)) {
    final_message <- paste0(final_message, ": ", gcp_error_message)
  } else {
    # Fallback if parsing fails or structure is unexpected
    final_message <- paste0(final_message, ". ", httr2::resp_status_desc(resp))
  }
  final_message
}

# Helper to construct resource names
sm_project_name <- function(project_id) {
  sprintf("projects/%s", project_id)
}

sm_secret_name <- function(project_id, secret_id) {
  sprintf("projects/%s/secrets/%s", project_id, secret_id)
}

sm_version_name <- function(project_id, secret_id, version_id) {
  sprintf("projects/%s/secrets/%s/versions/%s", project_id, secret_id, version_id)
}

# Helper to prepare payload for addVersion
prepare_payload_body <- function(payload_data, data_crc32c = NULL, version_labels = NULL) {
  if (is.raw(payload_data)) {
    payload_data <- rawToChar(payload_data) # Ensure it's char for base64_enc
  }
  if (!is.character(payload_data)) {
    stop("payload_data must be a character string or raw vector.", call. = FALSE)
  }
  
  body <- list(
    payload = list(
      data = jsonlite::base64_enc(payload_data)
    )
  )
  if (!is.null(data_crc32c)) {
    body$payload$dataCrc32c <- as.character(data_crc32c)
  }
  if (!is.null(version_labels)) {
    if (!is.list(version_labels) || is.null(names(version_labels))) {
        stop("'version_labels' must be a named list.", call. = FALSE)
    }
    body$payload$versionLabels <- version_labels
  }
  body
}

# Helper for IAM policy body for setIamPolicy
prepare_iam_policy_body <- function(policy, update_mask = NULL) {
  if(!is.list(policy) || (("bindings" %in% names(policy) || "etags" %in% names(policy)) == FALSE)) {
    stop("'policy' must be a valid IAM policy list object, typically including 'bindings' and 'etag'.", call. = FALSE)
  }
  body <- list(policy = policy)
  if (!is.null(update_mask)) {
    body$updateMask <- update_mask
  }
  body
}
