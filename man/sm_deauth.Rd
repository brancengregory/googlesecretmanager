% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/auth.R
\name{sm_deauth}
\alias{sm_deauth}
\title{De-authenticate from Secret Manager}
\usage{
sm_deauth()
}
\value{
Invisibly returns \code{NULL}.
}
\description{
Clears the current Secret Manager token. This means the next API request
that requires authentication will trigger the authentication process anew
(e.g., by calling \code{\link[=sm_auth]{sm_auth()}}).

Since Secret Manager generally requires authentication for all its significant
operations (and doesn't typically use API keys for accessing secrets),
de-authentication primarily serves to clear the current user's session or
force a re-authentication.
}
\examples{
\dontrun{
sm_deauth()
# Next API call will re-trigger auth
# list_secrets() # (Assuming this is a function in your package)
}
}
