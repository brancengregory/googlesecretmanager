# R/request.R
#' @importFrom httr stop_for_status content http_type
#' @importFrom jsonlite fromJSON
#' @importFrom gargle request_build request_make response_process
#' @importFrom rlang %||%
NULL

# Internal function to form Secret Manager API requests
# This function now hardcodes endpoint details directly.
# @noRd
secretmanager_request_generate <- function(
    endpoint = character(),
    params = list(),
    key = NULL,
    token = secretmanager_token()
) {
  ept <- secretmanager_endpoint(endpoint)
  if (is.null(ept)) {
    rlang::abort(
      paste0(
        "Endpoint not recognized: '",
        endpoint,
        "'. Please ensure the endpoint ID is correct and sysdata.rda is up to date."
      )
    )
  }

  service_root_url <- attr(.endpoints, "base_url")

  req <- gargle::request_develop(
    endpoint = ept,
    params = params,
    base_url = service_root_url
  )

  # Defensive check for the URL returned by request_develop.
  # If req$url is character(0), extract_path_names (called by request_build's internals)
  # will fail with "subscript out of bounds" on m[[1L]] because gregexpr on character(0) returns list().
  if (is.null(req$url) || !is.character(req$url) || length(req$url) != 1 || !nzchar(req$url[1])) {
    debug_info <- paste0(
      "Debug Info: endpoint_id='", endpoint,
      "', endpoint_path_template='", if(!is.null(ept) && !is.null(ept$path)) ept$path else "UNAVAILABLE",
      "', service_root_url='", service_root_url %||% "NULL_OR_EMPTY",
      "', params_parent_exists='", !is.null(params$parent),
      "', params_parent_class='", if(!is.null(params$parent)) class(params$parent)[1] else "N/A",
      "', params_parent_nzchar='", if(!is.null(params$parent) && is.character(params$parent)) nzchar(params$parent[1]) else "N/A" # check nzchar of first element
    )
    rlang::abort(
      message = paste0(
        "Failed to develop a valid URL for endpoint '", endpoint, "'. ",
        "URL from gargle::request_develop() was NULL, not a single non-empty string, or was an empty string. ",
        debug_info
      ),
      class = "secretmanager_url_develop_error"
    )
  }

  built_req <- gargle::request_build(
    path = req$url,
    method = req$method,
    params = req$params,  # These are query/body parameters from request_develop
    body = req$body,
    token = token,
    key = key
    # base_url argument to request_build is ignored if `path` is an absolute URL.
  )

  return(built_req)
}


# Make a request for the Google Secret Manager API
#'
#' This function serves as a thin wrapper around `gargle::request_make()`.
#' It adds the package's user agent and executes the HTTP request, including
#' handling retries. The input `x` is typically created with
#' [secretmanager_request_generate()] and the output should then be processed
#' with [gargle::response_process()].
#'
#' @param x List, holding the components for an HTTP request, presumably created
#'   with [secretmanager_request_generate()]. Should contain the `method`, `url`, `body`,
#'   and `token`.
#' @param ... Optional arguments passed through to the HTTP method.
#'
#' @return An `httr::response` object.
#' @export
secretmanager_request_make <- function(x, ...) {
  # FIX: Use gargle::request_make directly, which handles retries and the HTTP call.
  res <- gargle::request_retry(
    x,
    user_agent = "secretmanager-r-package", # Add your package's user agent
    ... # Pass any additional arguments through
  )

  processed_res <- gargle::response_process(response)

  return(processed_res)
}

