# secretmanager

An R package for interacting with Google Cloud Secret Manager, providing a secure way to manage and access secrets in your R applications.

## Installation

You can install the development version of secretmanager from GitHub using:

```r
# install.packages("remotes")
remotes::install_github("brancengregory/secretmanager")
```

## Authentication

This package uses the [gargle](https://gargle.r-lib.org/) package for authentication with Google Cloud services. Before using the package, you'll need to:

1. Set up authentication credentials for Google Cloud
2. Ensure you have the necessary permissions to access Secret Manager

### Authentication Methods

There are three ways to authenticate with Google Cloud Secret Manager:

#### 1. Explicit Token Authentication
```r
library(secretmanager)
library(gargle)

# Define required scopes and fetch token
scopes <- c("https://www.googleapis.com/auth/cloud-platform")
token <- gargle::token_fetch(scopes = scopes)

# Initialize the package with the token
secretmanager_auth(token = token)
```

#### 2. Application Default Credentials
```r
library(secretmanager)
library(gargle)

# This will open a browser window for authentication
gargle::credentials_app_default()
```

#### 3. Service Account Key
```r
library(secretmanager)
library(gargle)

# Set the path to your service account key file
gargle::credentials_service_account(
  path = "path/to/service-account-key.json"
)
```

Choose the authentication method that best fits your use case. The explicit token method provides the most control, application default credentials are convenient for local development, and service account keys are useful for automated environments.

## Usage

Here's a basic example of how to use the package:

```r
library(secretmanager)

project <- "test-project"

secrets <- list_secrets(project)

secret_version_latest(secrets[1], project)

create_secret(
  project_id = project,
  secret_id = "test-secret"
)

add_secret_version(
  project_id = project,
  secret_id = "test-secret",
  payload = "Shhhhhh, i'm a secret--don't tell!"
)
```

## Features

- Secure access to Google Cloud Secret Manager
- Easy integration with gargle authentication
- Support for creating, reading, updating, and deleting secrets
- Base64 encoding/decoding of secret values
- Comprehensive error handling

## Development

This package uses the following development tools:

- testthat for testing
- roxygen2 for documentation
- renv for dependency management

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## Security

This package handles sensitive information. Please ensure you:

1. Secrets may be printed to the R console and live in logs
2. Never commit credentials or secrets to version control
2. Use appropriate access controls in Google Cloud
3. Follow security best practices when handling secrets

## Getting Help

If you encounter any issues or have questions, please:

1. Check the [documentation](https://brancengregory.github.io/secretmanager/)
2. Review the [gargle documentation](https://gargle.r-lib.org/)
3. Open an [issue](https://github.com/brancengregory/secretmanager/issues)

