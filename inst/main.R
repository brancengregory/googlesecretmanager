devtools::load_all()

library(gargle)

scopes <- c("https://www.googleapis.com/auth/cloud-platform")
token <- gargle::token_fetch(scopes = scopes)

secretmanager_auth(token = token)

project <- "cjac-332215"

secrets <- list_secrets(project)

secret_version_latest(secrets[7], project)

create_secret(
  project_id = project,
  secret_id = "cjac-test-secret"
)

add_secret_version(
  project_id = project,
  secret_id = "cjac-test-secret",
  payload = "Suhhhhhh, i'm a secret--don't tell!"
)
