#' Create a Chat with Claude
#'
#' @param client An anthropic_client object
#' @param messages List of message objects with role and content
#' @param model The model to use (defaults to client's selected model)
#' @param max_tokens Maximum number of tokens to generate
#' @param temperature Controls randomness (0-1)
#' @return The response message
#' @importFrom httr POST add_headers content http_error
#' @importFrom jsonlite toJSON
#' @export
chat <- function(client,
                 messages,
                 model = client$selected_model,
                 max_tokens = 1000,
                 temperature = 0.7) {
  if (is.null(model)) {
    stop("No model selected. Use select_model() or specify a model.")
  }

  # Validate messages format
  if (!is.list(messages)) {
    stop("Messages must be a list of message objects")
  }

  # Prepare request body
  body <- list(
    model = model,
    messages = messages,
    max_tokens = max_tokens,
    temperature = temperature
  )

  # Make API call
  response <- httr::POST(
    url = paste0(client$base_url, "/v1/messages"),
    httr::add_headers(
      "x-api-key" = client$api_key,
      "anthropic-version" = client$api_version,
      "Content-Type" = "application/json"
    ),
    body = jsonlite::toJSON(body, auto_unbox = TRUE)
  )

  # Parse response
  if (httr::http_error(response)) {
    error_content <- httr::content(response, "parsed")
    if (!is.null(error_content$error) && !is.null(error_content$error$message)) {
      stop(paste("API error:", error_content$error$message))
    } else {
      stop(paste("API error:", httr::http_status(response)$message))
    }
  }

  content <- httr::content(response, "parsed")

  # Return just the text from the response
  return(content$content[[1]]$text)
}


#' In-line Claude chat
#'
#' A simpler interface for in-line chats with Claude
#'
#' @param prompt The text to send to Claude
#' @param api_key Anthropic API key (defaults to ANTHROPIC_API_KEY environment variable)
#' @param model Claude model to use
#' @param temperature Controls randomness (0-1)
#' @return Claude's response text
#' @export
ask_claude <- function(prompt,
                       api_key = Sys.getenv("ANTHROPIC_API_KEY"),
                       model = "claude-3.5-sonnet-20240307",
                       temperature = 0.7) {

  # Create temporary client
  client <- anthropic_client(api_key)

  # Format messages
  messages <- list(
    list(role = "user", content = prompt)
  )

  # Get response
  response <- chat(client,
                   messages = messages,
                   model = model,
                   temperature = temperature)

  return(response)
}

#' Stream Chat with Claude
#'
#' @param client An anthropic_client object
#' @param messages List of message objects with role and content
#' @param model The model to use (defaults to client's selected model)
#' @param max_tokens Maximum number of tokens to generate
#' @param temperature Controls randomness (0-1)
#' @param callback Function to process each chunk of the response
#' @return The complete response text after streaming
#' @importFrom httr POST add_headers content http_error
#' @importFrom jsonlite toJSON
#' @export
chat_stream <- function(client,
                        messages,
                        model = client$selected_model,
                        max_tokens = 1000,
                        temperature = 0.7,
                        callback = function(chunk) {
                          cat(chunk)
                          utils::flush.console()
                        }) {

  if (is.null(model)) {
    stop("No model selected. Use select_model() or specify a model.")
  }

  # Validate messages format
  if (!is.list(messages)) {
    stop("Messages must be a list of message objects")
  }

  # Extract just the values we need from the client
  api_key <- client$api_key
  base_url <- client$base_url
  api_version <- client$api_version

  # Validate messages format
  if (!is.list(messages)) {
    stop("Messages must be a list of message objects")
  }

  # Make sure each message has role and content
  for (msg in messages) {
    if (is.null(msg$role) || is.null(msg$content)) {
      stop("Each message must have 'role' and 'content' fields")
    }
  }

  # Prepare request body with streaming enabled
  body <- list(
    model = model,
    messages = messages,
    max_tokens = max_tokens,
    temperature = temperature,
    stream = TRUE
  )

  # Create JSON string directly
  request_json <- jsonlite::toJSON(body, auto_unbox = TRUE)
  cat("Request body:", request_json, "\n")

  # Make API call
  url <- paste0(base_url, "/v1/messages")

  full_response <- ""

  # Use httr for streaming
  handle <- curl::new_handle()
  curl::handle_setheaders(handle,
                          "x-api-key" = api_key,
                          "anthropic-version" = api_version,
                          "Content-Type" = "application/json")

  curl::handle_setopt(handle,
                      customrequest = "POST",
                      postfields = request_json)

  curl::curl_fetch_stream(url, handle = handle, fun = function(chunk) {
    chunk_text <- rawToChar(chunk)

    # Process each line
    lines <- strsplit(chunk_text, "\n")[[1]]
    for (line in lines) {
      if (trimws(line) == "") next
      if (grepl("^data:", line)) {
        data_json <- sub("^data: ", "", line)

        if (data_json == "[DONE]") {
          next
        }

        tryCatch({
          parsed <- jsonlite::fromJSON(data_json)

          # Check if this is a content delta
          if (!is.null(parsed$type) && parsed$type == "content_block_delta") {
            if (!is.null(parsed$delta) && !is.null(parsed$delta$text)) {
              text_chunk <- parsed$delta$text
              full_response <<- paste0(full_response, text_chunk)
              callback(text_chunk)
            }
          }
        }, error = function(e) {
          cat("Error parsing JSON:", data_json, "\n")
        })
      }
    }
  })

  return(full_response)
}

#' Simple Streaming Chat Helper
#'
#' A simpler interface for quick streaming chats with Claude
#'
#' @param prompt The text to send to Claude
#' @param api_key Anthropic API key (defaults to ANTHROPIC_API_KEY environment variable)
#' @param model Claude model to use
#' @param temperature Controls randomness (0-1)
#' @return Claude's complete response text after streaming
#' @export
ask_claude_stream <- function(prompt,
                              api_key = Sys.getenv("ANTHROPIC_API_KEY"),
                              model = "claude-3.5-sonnet-20240307",
                              temperature = 0.7) {

  # Create temporary client
  client <- anthropic_client(api_key)

  # Format messages
  messages <- list(
    list(role = "user", content = prompt)
  )

  # Stream response
  cat("Claude is thinking...\n")
  response <- chat_stream(
    client,
    max_tokens = 100,
    messages = messages,
    model = model,
    temperature = temperature
  )

  return(invisible(response))
}

