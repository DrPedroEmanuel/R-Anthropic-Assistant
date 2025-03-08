# anthropic.R - Core Client Implementation
#' Anthropic API Client
#'
#' @description R interface to Anthropic's Claude AI models
#' @param api_key Your Anthropic API key (defaults to ANTHROPIC_API_KEY environment variable)
#' @param base_url The base URL for the Anthropic API
#' @param api_version The Anthropic API version to use
#' @return An anthropic_client object
#' @export
anthropic_client <- function(api_key = Sys.getenv("ANTHROPIC_API_KEY"),
                             base_url = "https://api.anthropic.com",
                             api_version = "2023-06-01") {

  # Validate the API key
  validate_api_key(api_key)

  client <- list(
    api_key = api_key,
    base_url = base_url,
    api_version = api_version,
    models = NULL,
    selected_model = NULL
  )

  class(client) <- "anthropic_client"

  # Initialize available models
  client$models <- list_models(client)

  return(client)
                             }

#' Print method for anthropic_client
#'
#' @param x An anthropic_client object
#' @param ... Additional arguments passed to print
#' @return The original object, invisibly
#' @export
print.anthropic_client <- function(x, ...) {
  cat("Anthropic API Client\\n")
  cat("Base URL:", x$base_url, "\\n")
  cat("API Version:", x$api_version, "\\n")

  if (!is.null(x$selected_model)) {
    cat("Selected Model:", x$selected_model, "\\n")
  } else {
    cat("No model selected\\n")
  }

  cat("Available Models:\\n")
  print(x$models[, c("name", "id")])

  invisible(x)
}
