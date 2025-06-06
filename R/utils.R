empty_object <- function() {
  stats::setNames(list(), character(0))
}

#' Set Default Google Cloud Project ID
#'
#' Sets the default Google Cloud Project ID for subsequent Secret Manager
#' operations within the current R session. Functions will automatically
#' use this project ID unless explicitly overridden.
#'
#' @param project_id A single character string specifying the Google Cloud
#'   Project ID (e.g., "my-gcp-project-123").
#'
#' @return The `project_id` character string, invisibly.
#'   Called for its side-effect of setting the default project.
#' @export
#'
#' @examples
#' \dontrun{
#' sm_project_set("my-production-project")
#' # Now, sm_secret_ls() will default to "my-production-project"
#'
#' sm_project_get() # "my-production-project"
#'
#' # You can always override the default for specific calls:
#' sm_secret_get("my-secret", project_id = "another-project")
#' }
sm_project_set <- function(project_id) {
  # --- Input Validation ---
  if (!is.character(project_id) || length(project_id) != 1) {
    cli::cli_abort(
      "{.arg project_id} must be a single character string, not {.obj_type_friendly {project_id}}."
    )
  }
  if (is.na(project_id) || nchar(trimws(project_id)) == 0) {
    cli::cli_abort("{.arg project_id} cannot be empty or {.val NA}.")
  }

  # --- Store the project ID in the internal environment ---
  old_project <- .sm_pkg_env$current_project
  .sm_pkg_env$current_project <- project_id

  # --- Provide clear user feedback ---
  if (!is.null(old_project) && old_project != project_id) {
    cli::cli_alert_success(
      "Default GCP project changed from {.val {old_project}} to {.val {project_id}}."
    )
  } else if (is.null(old_project)) {
    cli::cli_alert_success("Default GCP project set to {.val {project_id}}.")
  } else { # old_project == project_id
    cli::cli_alert_info("Default GCP project is already {.val {project_id}}. No change needed.")
  }

  # --- Return invisibly ---
  invisible(project_id)
}

sm_project_get <- function() {
  .sm_pkg_env$current_project
}

#' List Secret Manager endpoints
#'
#' @description
#' The secretmanager package stores a named list of Secret Manager API v1 endpoints (or
#' "methods", using Google's vocabulary) internally and these functions expose
#' this data.
#'   * `sm_endpoint()` returns one endpoint, i.e. it uses `[[`.
#'   * `sm_endpoints()` returns a list of endpoints, i.e. it uses `[`.
#'
#' The names of this list (or the `id` sub-elements) are the nicknames that can
#' be used to specify an endpoint in request_generate(). For each endpoint, we
#' store its nickname or `id`, the associated HTTP verb, the `path`, and details
#' about the parameters. This list is derived programmatically from the Secret Manager API v1 Discovery Document
#' (`https://www.googleapis.com/discovery/v1/apis/secretmanager/v1/rest`) using the
#' approach described in the [Discovery Documents
#' section](https://gargle.r-lib.org/articles/request-helper-functions.html#discovery-documents)
#' of the gargle vignette [Request helper
#' functions](https://gargle.r-lib.org/articles/request-helper-functions.html).
#'
#' @param i The name(s) or integer index(ices) of the endpoints to return. `i`
#'   is optional for `sm_endpoints()` and, if not given, the entire list is
#'   returned.
#'
#' @return One or more of the Secret Manager API v1 endpoints that are used internally by
#'   secretmanager.
#' @export
#'
#' @examples
#' str(head(sm_endpoints(), 3), max.level = 2)
#' sm_endpoint("secretmanager.projects.secrets.versions.destroy")
#' sm_endpoint(4)
sm_endpoints <- function(i = NULL) {
  if (is.null(i)) {
    i <- seq_along(.endpoints)
  }
  stopifnot(is.character(i) || (is.numeric(i)))
  .endpoints[i]
}

#' Secret Manager Endpoint
#'
#' @param i The index of the endpoint
#'
#' @export
#'
sm_endpoint <- function(i) {
  stopifnot(rlang::is_string(i) || (is.numeric(i) && length(i) == 1))
  .endpoints[[i]]
}
