devtools::load_all()

library(gargle)
library(purrr)

scopes <- c("https://www.googleapis.com/auth/cloud-platform")
token <- gargle::token_fetch(scopes = scopes)

secretmanager_auth(token = token)

project <- "cjac-332215"

secrets <- list_secrets(project)

secret_version_latest(secrets[3], project)
