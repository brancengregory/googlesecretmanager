# Set Default Google Cloud Project ID

Sets the default Google Cloud Project ID for subsequent Secret Manager
operations within the current R session. Functions will automatically
use this project ID unless explicitly overridden.

## Usage

``` r
sm_project_set(project_id)
```

## Arguments

- project_id:

  A single character string specifying the Google Cloud Project ID
  (e.g., "my-gcp-project-123").

## Value

The `project_id` character string, invisibly. Called for its side-effect
of setting the default project.

## Examples

``` r
if (FALSE) { # \dontrun{
sm_project_set("my-production-project")
# Now, sm_secret_ls() will default to "my-production-project"

sm_project_get() # "my-production-project"

# You can always override the default for specific calls:
sm_secret_get("my-secret", project_id = "another-project")
} # }
```
