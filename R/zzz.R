.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "Welcome to secretmanagerR v", utils::packageVersion("secretmanagerR"), " (using httr2)!\n",
    "Use `secretmanager_auth()` to authenticate with Google Cloud.\n",
    "Find examples with `?get_secret_payload` or `?list_secrets`."
  )
}
