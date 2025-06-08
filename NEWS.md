# googlesecretmanager 0.1.0

* Breaking change: Renamed all functions to use `sm_` prefix for consistency
  * `create_secret()` → `sm_secret_create()`
  * `get_secret()` → `sm_secret_get()`
  * `list_secrets()` → `sm_secret_ls()`
  * `add_version()` → `sm_secret_version_add()`
  * `get_version()` → `sm_secret_version_get()`
  * `list_versions()` → `sm_secret_version_ls()`
  * `delete_version()` → `sm_secret_version_delete()`
  * `delete_secret()` → `sm_secret_delete()`
* Added S3 class system for better object-oriented handling of secrets
* Added project management functions `sm_project_set()` and `sm_project_get()`
* Enhanced error handling with more informative messages
* Improved secret metadata handling and type validation
* Fixed issues with secret version retrieval and deletion
* Streamlined authentication process

# googlesecretmanager 0.0.2

* Added `secret` class with specialized methods
* Implemented secure print method to hide secret values in R output
* Added support for secret metadata
* Enhanced error messages and token handling
* Improved documentation and examples

# googlesecretmanager 0.0.1

* Initial release with core Google Cloud Secret Manager integration
* Basic secret management: creation, versioning, and retrieval
* Integration with gargle for authentication
* Support for multiple auth methods (Application Default Credentials, Service Account, Explicit Token)
* Basic project management functionality
