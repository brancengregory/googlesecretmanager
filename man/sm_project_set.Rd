% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{sm_project_set}
\alias{sm_project_set}
\title{Set Default Google Cloud Project ID}
\usage{
sm_project_set(project_id)
}
\arguments{
\item{project_id}{A single character string specifying the Google Cloud
Project ID (e.g., "my-gcp-project-123").}
}
\value{
The \code{project_id} character string, invisibly.
Called for its side-effect of setting the default project.
}
\description{
Sets the default Google Cloud Project ID for subsequent Secret Manager
operations within the current R session. Functions will automatically
use this project ID unless explicitly overridden.
}
\examples{
\dontrun{
sm_project_set("my-production-project")
# Now, sm_secret_ls() will default to "my-production-project"

sm_project_get() # "my-production-project"

# You can always override the default for specific calls:
sm_secret_get("my-secret", project_id = "another-project")
}
}
