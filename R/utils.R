empty_object <- function() {
  stats::setNames(list(), character(0))
}


#' List Secret Manager endpoints
#'
#' @description
#' The secretmanager package stores a named list of Secret Manager API v1 endpoints (or
#' "methods", using Google's vocabulary) internally and these functions expose
#' this data.
#'   * `secretmanager_endpoint()` returns one endpoint, i.e. it uses `[[`.
#'   * `secretmanager_endpoints()` returns a list of endpoints, i.e. it uses `[`.
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
#'   is optional for `secretmanager_endpoints()` and, if not given, the entire list is
#'   returned.
#'
#' @return One or more of the Secret Manager API v1 endpoints that are used internally by
#'   secretmanager.
#' @export
#'
#' @examples
#' str(head(secretmanager_endpoints(), 3), max.level = 2)
#' secretmanager_endpoint("secretmanager.projects.secrets.versions.destroy")
#' secretmanager_endpoint(4)
secretmanager_endpoints <- function(i = NULL) {
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
secretmanager_endpoint <- function(i) {
  stopifnot(rlang::is_string(i) || (is.numeric(i) && length(i) == 1))
  .endpoints[[i]]
}
