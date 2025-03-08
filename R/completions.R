#' Generate Text with Claude
#'
#' @param client An anthropic_client object
#' @param prompt The prompt to generate text from
#' @param model The model to use (defaults to client's selected model)
#' @param max_tokens Maximum number of tokens to generate
#' @param temperature Controls randomness (0-1)
#' @return The generated text
#' @importFrom httr POST add_headers content http_error
#' @importFrom jsonlite toJSON
#' @export
generate_text <- function(client,
                          prompt,
                          model = client$selected_model,
                          max_tokens = 1000,
                          temperature = 0.7) {

  if (is.null(model)) {
    stop("No model selected. Use select_model() or specify a model.")
  }

  # Format prompt according to Claude's expectations
  # Claude expects a specific format with Human: and Assistant:
  if (!grepl("Human:", prompt)) {
    prompt <- paste0("Human: ", prompt, "\\n\\nAssistant: ")
  }

  # Prepare request body for messages API
  body <- list(
    model = model,
    max_tokens = max_tokens,  # Changed from max_tokens_to_sample
    messages = list(
      list(
        role = "user",
        content = prompt
      )
    ),
    temperature = temperature
  )

  # Make API call
  response <- httr::POST(
    url = paste0(client$base_url, "/v1/messages"),  # Using messages endpoint
    httr::add_headers(
      "x-api-key" = client$api_key,
      "anthropic-version" = client$api_version,
      "Content-Type" = "application/json"
    ),
    body = jsonlite::toJSON(body, auto_unbox = TRUE)
  )

  # Parse the response
  if (httr::http_error(response)) {
    error_content <- httr::content(response, "parsed")
    stop(paste("API error:", if(!is.null(error_content$error$type)) error_content$error$type else "Unknown error"))
  }

  content <- httr::content(response, "parsed")
  # Extract text from the new response format
  response_text <- content$content[[1]]$text

  return(response_text)
}
