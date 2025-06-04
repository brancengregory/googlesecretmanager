devtools::load_all()

library(gargle)
library(purrr)

scopes <- c("https://www.googleapis.com/auth/cloud-platform")
token <- gargle::token_fetch(scopes = scopes)

secretmanager_auth(token = token)

project <- "cjac-332215"

list_secrets(project)
