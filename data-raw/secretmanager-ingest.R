library(tidyverse)

devtools::load_all()

source(
  system.file("data-raw", "ingest-functions.R", package = "secretmanager")
)

existing <- list_discovery_documents("secretmanager:v1")
if (length(existing) > 1) {
  rlang::warn("MULTIPLE DISCOVERY DOCUMENTS FOUND. FIX THIS!")
}

if (length(existing) < 1) {
  rlang::inform("Downloading Discovery Document")
  discovery_doc_path <- download_discovery_document("secretmanager:v1")
} else {
  msg <- glue::glue("
    Using existing Discovery Document:
      * {existing}
    ")
  rlang::inform(msg)
  discovery_doc_path <- here::here("data-raw", existing)
}

discovery_doc <- read_discovery_document(discovery_doc_path)

find_all_named_elements <- function(dd, element_name) {
  results <- list()

  # If the current item is a list, check its direct elements
  if (is.list(dd)) {
    # 1. Check if the current list 'dd' contains the element_name
    if (element_name %in% names(dd)) {
      results <- c(results, list(dd[[element_name]]))
    }

    # 2. Recursively call this function on all sub-lists
    # Use `map` to iterate over elements of `dd` that are lists
    sub_list_results <- purrr::map(dd, function(x) {
      if (is.list(x)) {
        find_all_named_elements(x, element_name) # Recursive call
      } else {
        NULL # Return NULL for non-list elements, to be compacted later
      }
    }) |>
      purrr::compact() # Remove NULLs from non-list elements

    # Combine results from current level and sub-levels
    results <- c(results, sub_list_results)
  }

  # Flatten the results to get a single long list of all found elements
  purrr::flatten(results)
}

methods <- find_all_named_elements(discovery_doc, "methods") |>
  (\(x) {
    purrr::set_names(x, purrr::map_chr(x, \(x) { x$id }))
  })() |>
  map(groom_properties,  discovery_doc) |>
  map(add_schema_params, discovery_doc) |>
  map(add_global_params, discovery_doc) |>
  map(\(method) {
    if (!is.null(method$path)) {
      method$path <- gsub("\\{(\\+)([^}]+)\\}", "{\\2}", method$path)
    }
    return(method)
  })

.endpoints <- methods
attr(.endpoints, "base_url") <- discovery_doc$rootUrl # baseUrl??

usethis::use_data(.endpoints, internal = TRUE, overwrite = TRUE)
