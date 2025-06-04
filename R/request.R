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
      "Endpoint not recognized:",
      endpoint
    )
  }

  req <- gargle::request_develop(
    endpoint = ept,
    params = params
  )

  gargle::request_build(
    path = req$path,
    method = req$method,
    params = req$params,
    body = req$body,
    token = token
  )
}

#' Make a request for the Google Drive v3 API
#'
#' Low-level functions to execute one or more Drive API requests and, perhaps,
#' process the response(s). Most users should, instead, use higher-level
#' wrappers that facilitate common tasks, such as uploading or downloading Drive
#' files. The functions here are intended for internal use and for programming
#' around the Drive API. Three functions are documented here:
#'   * `request_make()` does the bare minimum: calls [gargle::request_make()],
#'     only adding the googledrive user agent. Typically the input is created
#'     with [request_generate()] and the output is processed with
#'     [gargle::response_process()].
#'   * `do_request()` is simply
#'     `gargle::response_process(request_make(x, ...))`. It exists only because
#'     we had to make `do_paginated_request()` and it felt weird to not make the
#'     equivalent for a single request.
#'   * `do_paginated_request()` executes the input request **with page
#'     traversal**. It is impossible to separate paginated requests into a "make
#'     request" step and a "process request" step, because the token for the
#'     next page must be extracted from the content of the current page.
#'     Therefore this function does both and returns a list of processed
#'     responses, one per page.
#'
#' @param x List, holding the components for an HTTP request, presumably created
#'   with [request_generate()] Should contain the `method`, `url`, `body`,
#'   and `token`.
#' @param ... Optional arguments passed through to the HTTP method.
#'
#' @return `request_make()`: Object of class `response` from [httr].
#' @export
#' @family low-level API functions
secretmanager_request_make <- function(x, ...) {
  gargle::request_retry(x, ...)
}

