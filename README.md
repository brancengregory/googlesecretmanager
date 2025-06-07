# googlesecretmanager 🔐

<!-- badges: start -->
[![R-CMD-check](https://github.com/brancengregory/secretmanager/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/brancengregory/secretmanager/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

> Keep your secrets safe and sound in the cloud! 🚀

An R package that makes it easy and secure to manage your secrets using Google Cloud Secret Manager. Whether you're building a Shiny app, running R scripts in production, or managing sensitive configuration, `secretmanager` has got you covered!

## Quick Start 🚀

```r
# Install the package
remotes::install_github("brancengregory/secretmanager")

# Load and authenticate (opens browser)
library(secretmanager)
gargle::credentials_app_default()

# Start managing secrets!
sm_project_set("my-project-id")
sm_secret_create("api-key", replication = list(automatic = list()))
sm_secret_version_add("api-key", "your-secret-value")
```

## Installation

You can install the development version of secretmanager from GitHub using:

```r
# install.packages("remotes")
remotes::install_github("brancengregory/secretmanager")
```

## Authentication 🔑

This package uses [gargle](https://gargle.r-lib.org/) for secure authentication with Google Cloud. Choose your preferred method:

### 1. Quick & Easy: Application Default Credentials
```r
library(secretmanager)
gargle::credentials_app_default()  # Opens browser for authentication
```

### 2. Production Ready: Service Account
```r
library(secretmanager)
gargle::credentials_service_account(
  path = "path/to/service-account-key.json"
)
```

### 3. Advanced: Explicit Token
```r
library(secretmanager)
token <- gargle::token_fetch(
  scopes = "https://www.googleapis.com/auth/cloud-platform"
)
secretmanager_auth(token = token)
```

## Common Use Cases 🎯

### 1. Managing API Keys
```r
# Store an API key
sm_secret_create("stripe-api-key")
sm_secret_version_add("stripe-api-key", "sk_live_...")

# Retrieve in your application
api_key <- sm_secret_version_get("stripe-api-key")
```

### 2. Configuration Management
```r
# Store database credentials
db_config <- list(
  host = "db.example.com",
  user = "admin",
  password = "secret123"
)
sm_secret_create("db-config")
sm_secret_version_add("db-config", jsonlite::toJSON(db_config))

# Use in your application
config <- jsonlite::fromJSON(sm_secret_version_get("db-config"))
```

### 3. Rotating Secrets
```r
# Add a new version
sm_secret_version_add("api-key", "new-secret-value")

# List versions
versions <- sm_secret_version_ls("api-key")

# Delete old versions
sm_secret_version_delete("api-key", "old-version-id")
```

## Features ✨

- 🔒 Secure secret management with Google Cloud
- 🔄 Version control for secrets
- 🔐 Multiple authentication methods
- 📦 Easy integration with R applications
- 🛠️ Comprehensive API coverage
- ⚡ Efficient secret retrieval
- 🎯 Project-level operations
- 📝 Detailed error messages

## Security Best Practices 🛡️

1. **Never** commit secrets to version control
2. Use appropriate IAM roles and permissions
3. Rotate secrets regularly
4. Enable audit logging in Google Cloud
5. Use service accounts for production
6. Keep your R session secure
7. Be cautious with secret output in logs

## Troubleshooting 🔧

### Common Issues

1. **Authentication Failed**
   - Check your Google Cloud credentials
   - Verify project permissions
   - Ensure correct scopes are set

2. **Secret Not Found**
   - Verify project ID
   - Check secret name spelling
   - Confirm secret exists in Google Cloud Console

3. **Permission Denied**
   - Review IAM roles
   - Check service account permissions
   - Verify project access

## Development 🛠️

This package is built with:

- [testthat](https://testthat.r-lib.org/) for testing
- [roxygen2](https://roxygen2.r-lib.org/) for documentation
- [renv](https://rstudio.github.io/renv/) for dependency management

## Contributing 🤝

Contributions are welcome! Please:

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Submit a pull request

## Getting Help 💬

- 📚 [Package Documentation](https://brancengregory.github.io/secretmanager/)
- 🔍 [Google Cloud Secret Manager Docs](https://cloud.google.com/secret-manager)
- 🐛 [Report Issues](https://github.com/brancengregory/secretmanager/issues)

## License 📄

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

