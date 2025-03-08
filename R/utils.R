#' Validate API Key
#'
#' @param api_key The Anthropic API key to validate
#' @return TRUE if the key appears valid, error message otherwise
#' @keywords internal
validate_api_key <- function(api_key) {
  if (is.null(api_key) || api_key == "") {
    stop("No API key provided. Set your API key with Sys.setenv(ANTHROPIC_API_KEY='your-key')")
  }

  # Basic format validation (actual validation happens when making API calls)
  if (!grepl("^sk-", api_key)) {
    warning("API key doesn't match expected format (should start with 'sk-')")
  }

  return(TRUE)
}
