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

  built_req <- gargle::request_build(
    path = req$path,
    method = req$method,
    params = req$params,
    body = req$body,
    token = token,
    key = key,
    base_url = req$base_url
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

