#' List Available Claude Models
#'
#' @param client An anthropic_client object
#' @return A data frame of available models
#' @export
list_models <- function(client) {
  # In a production API, we would call the models endpoint
  # For now, we'll hardcode the known models

  models <- data.frame(
    num_id = c(
      1,
      2,
      3,
      4
    ),
    id = c(
      "claude-3-opus-20240229",
      "claude-3-5-sonnet-20240307",
      "claude-3-7-sonnet-20250219",
      "claude-3-5-haiku-20241022"
    ),
    name = c(
      "Claude 3 Opus",
      "Claude 3.5 Sonnet",
      "Claude 3.7 Sonnet",
      "Claude 3.5 Haiku"
    ),
    description = c(
      "Excells at writing and complex tasks",
      "Standard model",
      "Most intelligent model",
      "Fast for daily tasks (cheap for completions)"
    ),
    stringsAsFactors = FALSE
  )

  return(models)
}

#' Select a Claude Model
#'
#' @param client An anthropic_client object
#' @param model_number The number ID of the model to use
#' @return The client with the selected model
#' @export
select_model <- function(client, model_number) {
  print(model_number)
  print(client$models$num_id)
  available_models <- client$models$num_id

  if (model_number %in% available_models) {
    client$selected_model <- client$models$id[model_number]
    message("Selected model: ", client$models$id[model_number])
    return(client)
  } else {
    stop(paste("Model not found. Available models:", paste(available_models, collapse=", ")))
  }
}
