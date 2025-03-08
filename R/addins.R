#' @importFrom shiny fluidPage titlePanel sidebarLayout sidebarPanel mainPanel div textAreaInput actionButton icon dialogViewer paneViewer runGadget selectInput sliderInput checkboxInput helpText observeEvent removeUI insertUI HTML verbatimTextOutput reactive reactiveVal renderText renderUI tags observeEvent invalidateLater
#' @importFrom rstudioapi getActiveDocumentContext insertText getSourceEditorContext
#' @importFrom shinyjs useShinyjs runjs extendShinyjs
#' @importFrom DT datatable renderDT
#' @importFrom diffobj diffPrint
NULL

#' Run Claude Code Completion Addin
#'
#' @export
runClaudeCompletion <- function() {

  # loggin point 1
  claude_log("Logging 1 -> Code Completion addin started", addin = "Completion")

  # Check for required packages
  required_packages <- c("shiny", "rstudioapi", "shinyjs", "diffobj")
  missing <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  if (length(missing) > 0) {
    stop("The following packages are required: ", paste(missing, collapse = ", "))

  }



  # JavaScript for key handling and cursor monitoring
  js_code <- "
  shinyjs.init = function() {
    // Track cursor movements and idle time
    var idleTimer = null;
    var cursorPosition = null;

    $(document).on('keydown', function(e) {
      // Handle TAB key press for completion acceptance
      if (e.keyCode === 9 && $('#completion').text().trim() !== '') {
        e.preventDefault();
        Shiny.setInputValue('accept_completion', true, {priority: 'event'});
      }
    });

    // Function to track cursor activity in RStudio
    function setupCursorTracking() {
      clearTimeout(idleTimer);

      // Get cursor position from RStudio (requires RStudio API integration)
      // This is a placeholder for the actual implementation
      var newPosition = 'placeholder_for_cursor_position';

      if (newPosition !== cursorPosition) {
        cursorPosition = newPosition;
      }

      // Reset idle timer
      idleTimer = setTimeout(function() {
        Shiny.setInputValue('cursor_idle', true, {priority: 'event'});
      }, window.idleTimeMs);
    }

    // Set interval to check cursor position
    setInterval(setupCursorTracking, 1000);
  };

  // Function to set idle time
  shinyjs.setIdleTime = function(params) {
    window.idleTimeMs = params * 1000; // Convert seconds to milliseconds
  };
  "

  # UI Definition
  ui <- shiny::fluidPage(
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(text = js_code, functions = c("setIdleTime")),

    shiny::titlePanel("Claude Code Completion"),

    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput("model", "Claude Model",
                           choices = c(
                             "claude-3-opus-20240229",
                             "claude-3-5-sonnet-20240307",
                             "claude-3-7-sonnet-20250219",
                             "claude-3-5-haiku-20241022"
                           ),
                           selected = "claude-3-5-haiku-20241022"),

        shiny::sliderInput("temperature", "Temperature",
                           min = 0, max = 1, value = 0.5, step = 0.1),

        shiny::sliderInput("idle_time", "Idle Time (seconds)",
                           min = 1, max = 10, value = 2, step = 1),

        shiny::checkboxInput("auto_complete", "Enable Auto-Completion on Idle", value = TRUE),

        shiny::helpText("Press TAB to accept a completion"),

        shiny::hr(),

        shiny::checkboxInput("insert", "Insert at cursor position", value = TRUE),

        shiny::helpText("You can edit the prompt below to customize the completion request:")
      ),

      shiny::mainPanel(
        shiny::textAreaInput("prompt", "Prompt", value = "", height = "150px"),

        shiny::h4("Generated Code"),
        shiny::verbatimTextOutput("completion"),

        shiny::actionButton("insert_code", "Insert Code",
                            icon = shiny::icon("code"),
                            class = "btn-success"),

        shiny::actionButton("generate", "Generate Manually",
                            class = "btn-primary"),

        shiny::textOutput("status")
      )
    )
  )

  # Server logic
  server <- function(input, output, session) {
    completion <- shiny::reactiveVal("")
    status <- shiny::reactiveVal("")
    last_context <- shiny::reactiveVal("")

    # Logging 2
    claude_log(sprintf("Logging 2 -> Setting initial idle time to %d seconds", 2), addin = "Completion")

    # Set initial idle time
    shinyjs::runjs(paste0("shinyjs.setIdleTime(", 2, ")"))

    # Update idle time when slider changes
    shiny::observeEvent(input$idle_time, {
      shinyjs::runjs(paste0("shinyjs.setIdleTime(", input$idle_time, ")"))

      # Logging 3
      claude_log(sprintf("Logging 3 -> Idle time changed to %d seconds", input$idle_time), addin = "Completion")
    })

    # Function to get current code context
    get_code_context <- function() {

      context <- rstudioapi::getActiveDocumentContext()
      selection <- context$selection[[1]]$text

      # Logging 4
      claude_log(sprintf("Logging 4 -> Got context: ", context, " And selection: ", selection), addin = "Completion")

      # If nothing is selected, get the current line and some context
      if (selection == "") {
        current_row <- context$selection[[1]]$range$start[1]
        content <- context$contents

        # Get up to 10 lines before current position for context
        start_row <- max(1, current_row - 10)
        selection <- paste(content[start_row:current_row], collapse = "\n")

        # Logging 5
        claude_log(sprintf("Logging 5 -> No selection, got the surrounding content: ", content, " And selection: ", selection), addin = "Completion")
      }

      # Generate prompt for code completion
      prompt <- paste0(
        "I have the following R code and need help completing it. ",
        "Please provide the next logical part of the code based on what you see:",
        "\n\n```r\n", selection, "\n```\n\n",
        "Continue the code where I left off. Only return the code, no explanations."
      )

      return(list(selection = selection, prompt = prompt))
    }

    # Initialize with current context
    shiny::observe({
      ctx <- get_code_context()
      last_context(ctx$selection)
      shiny::updateTextAreaInput(session, "prompt", value = ctx$prompt)
    })

    # Logging 6
    claude_log("Logging 6 -> Initialized with current editor context", addin = "Completion")

    # Handle cursor idle event
    shiny::observeEvent(input$cursor_idle, {

      # Logging 7
      claude_log("Logging 7 -> Cursor idle detected", addin = "Completion")

      if (!input$auto_complete) return()

      # Get current context
      ctx <- get_code_context()

      # Only generate if context has changed
      if (ctx$selection != last_context()) {
        last_context(ctx$selection)
        shiny::updateTextAreaInput(session, "prompt", value = ctx$prompt)

        # Trigger completion generation
        generate_completion()
      }
    })

    # Handle TAB key for accepting completion
    shiny::observeEvent(input$accept_completion, {
      code <- completion()
      if (code != "") {
        rstudioapi::insertText(code)
        status("Completion accepted and inserted")
      }
    })

    # Generate completion function
    generate_completion <- function() {
      status("Generating completion...")

      tryCatch({
        # Create client and message
        client <- anthropic_client()
        client$selected_model <- input$model

        messages <- list(
          list(role = "user", content = input$prompt)
        )

        # Get completion
        response <- chat(
          client,
          messages = messages,
          temperature = input$temperature
        )

        # Extract code from response if needed
        if (grepl("```r", response)) {
          # Extract code block
          code <- gsub(".*```r\\n(.*)\\n```.*", "\\1", response, perl = TRUE)
          completion(code)
        } else {
          completion(response)
        }

        status("Completion generated successfully!")

        # Auto-insert if option is checked and not in auto-complete mode
        if (input$insert && !input$auto_complete) {
          rstudioapi::insertText(completion())
          status("Code inserted at cursor position")
        }

      }, error = function(e) {
        status(paste("Error:", e$message))
      })
    }

    # Manual generate completion button
    shiny::observeEvent(input$generate, {
      generate_completion()
    })

    # Insert code button
    shiny::observeEvent(input$insert_code, {
      code <- completion()
      if (code != "") {
        rstudioapi::insertText(code)
        status("Code inserted at cursor position")
      } else {
        status("No code to insert")
      }
    })

    # Display completion
    output$completion <- shiny::renderText({
      completion()
    })

    # Display status
    output$status <- shiny::renderText({
      status()
    })
  }

  # Run the addin
  shiny::runGadget(ui, server, viewer = shiny::paneViewer(minHeight = 500))
}

#' Run Claude Assistant Addin
#'
#' @export
runClaudeAssistant <- function() {
  # Check for required packages
  required_packages <- c("shiny", "shinyjs", "diffobj", "DT")
  missing <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  if (length(missing) > 0) {
    stop("The following packages are required: ", paste(missing, collapse = ", "))
  }

  # UI Definition
  ui <- shiny::fluidPage(
    shinyjs::useShinyjs(),
    shiny::titlePanel("Claude AI Assistant"),
    shiny::tags$style(shiny::HTML("
      .chat-container { height: 400px; overflow-y: scroll; border: 1px solid #ddd; padding: 10px; margin-bottom: 10px; }
      .user-message { background-color: #f1f1f1; padding: 8px; border-radius: 10px; margin: 5px 0; text-align: right; }
      .claude-message { background-color: #e3f2fd; padding: 8px; border-radius: 10px; margin: 5px 0; }
      .model-select { margin-bottom: 15px; }
      .diff-view { border: 1px solid #ddd; padding: 10px; margin: 10px 0; max-height: 200px; overflow-y: auto; }
    ")),

    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::textInput("api_key", "Anthropic API Key",
                         value = Sys.getenv("ANTHROPIC_API_KEY")),

        shiny::selectInput("model", "Claude Model",
                           choices = c(
                             "claude-3-opus-20240229",
                             "claude-3-5-sonnet-20240307",
                             "claude-3-7-sonnet-20250219",
                             "claude-3-5-haiku-20241022"
                           ),
                           selected = "claude-3-5-haiku-20241022"),

        shiny::sliderInput("temperature", "Temperature",
                           min = 0, max = 1, value = 0.7, step = 0.1),

        shiny::sliderInput("max_tokens", "Max Output Tokens",
                           min = 100, max = 4000, value = 1000, step = 100),

        shiny::checkboxInput("include_selection", "Include current selection", value = TRUE),

        shiny::checkboxInput("show_diff", "Show diff of code changes", value = TRUE),

        shiny::checkboxInput("insert_to_doc", "Insert response into document", value = FALSE),

        shiny::helpText("Higher temperatures produce more creative outputs,
                       lower temperatures produce more deterministic outputs.")
      ),

      shiny::mainPanel(
        shiny::div(id = "chat_container", class = "chat-container"),

        # Diff view (conditionally shown)
        shiny::uiOutput("diff_container"),

        # Apply changes button (conditionally shown)
        shiny::uiOutput("apply_changes_ui"),

        shiny::textAreaInput("prompt", "Your Message", height = "100px"),

        shiny::actionButton("send", "Send to Claude",
                            icon = shiny::icon("paper-plane"),
                            class = "btn-primary"),

        shiny::actionButton("clear", "Clear Chat",
                            icon = shiny::icon("trash-alt")),

        shiny::textOutput("status")
      )
    )
  )

  # Server logic
  server <- function(input, output, session) {
    messages <- shiny::reactiveVal(list())
    status <- shiny::reactiveVal("")
    selected_code <- shiny::reactiveVal("")
    modified_code <- shiny::reactiveVal("")
    show_diff_view <- shiny::reactiveVal(FALSE)

    # Get current selection when addin is launched
    shiny::observe({
      if (requireNamespace("rstudioapi", quietly = TRUE)) {
        context <- rstudioapi::getActiveDocumentContext()
        if (length(context$selection) > 0) {
          selection <- context$selection[[1]]$text
          if (selection != "") {
            selected_code(selection)

            # Automatically include selection in prompt if option is checked
            if (input$include_selection) {
              current_prompt <- input$prompt
              if (current_prompt == "") {
                updated_prompt <- paste0("Here's my code:\n\n```r\n", selection, "\n```\n\nI'd like you to review this code and suggest improvements.")
                shiny::updateTextAreaInput(session, "prompt", value = updated_prompt)
              }
            }
          }
        }
      }
    })

    # Render chat messages
    render_chat <- function() {
      msgs <- messages()
      chat_html <- ""

      for (msg in msgs) {
        if (msg$role == "user") {
          chat_html <- paste0(chat_html,
                              "<div class='user-message'><strong>You:</strong> ",
                              msg$content, "</div>")
        } else {
          chat_html <- paste0(chat_html,
                              "<div class='claude-message'><strong>Claude:</strong> ",
                              msg$content, "</div>")
        }
      }

      shiny::removeUI(selector = "#chat_container > *")
      shiny::insertUI(
        selector = "#chat_container",
        where = "beforeEnd",
        ui = shiny::HTML(chat_html),
        immediate = TRUE
      )
    }

    # Function to extract code blocks from Claude's response
    extract_code <- function(text) {
      # Look for R code blocks
      pattern <- "```r\\n(.+?)\\n```"
      matches <- regmatches(text, gregexpr(pattern, text, perl = TRUE))

      if (length(matches[[1]]) > 0) {
        code <- matches[[1]][1]  # Take the first code block
        # Remove the ```r and ``` markers
        code <- gsub("```r\\n|\\n```", "", code)
        return(code)
      }

      # If no R block found, try generic code block
      pattern <- "```\\n(.+?)\\n```"
      matches <- regmatches(text, gregexpr(pattern, text, perl = TRUE))

      if (length(matches[[1]]) > 0) {
        code <- matches[[1]][1]  # Take the first code block
        # Remove the ``` markers
        code <- gsub("```\\n|\\n```", "", code)
        return(code)
      }

      return(NULL)  # No code block found
    }

    # Generate diff view between original and modified code
    output$diff_container <- shiny::renderUI({
      if (!show_diff_view()) {
        return(NULL)
      }

      original <- selected_code()
      modified <- modified_code()

      if (original != "" && modified != "") {
        diff_output <- diffobj::diffPrint(original, modified,
                                          format = "html",
                                          mode = "sidebyside")

        shiny::div(
          class = "diff-view",
          shiny::h4("Code Differences"),
          shiny::HTML(as.character(diff_output))
        )
      } else {
        NULL
      }
    })

    # Render apply changes button
    output$apply_changes_ui <- shiny::renderUI({
      if (show_diff_view() && modified_code() != "") {
        shiny::div(
          shiny::actionButton("apply_changes", "Apply Changes",
                              icon = shiny::icon("check"),
                              class = "btn-success"),
          style = "margin-bottom: 15px;"
        )
      } else {
        NULL
      }
    })

    # Apply changes handler
    shiny::observeEvent(input$apply_changes, {
      code <- modified_code()
      if (code != "" && requireNamespace("rstudioapi", quietly = TRUE)) {
        context <- rstudioapi::getActiveDocumentContext()
        if (length(context$selection) > 0) {
          # Replace the selection with modified code
          rstudioapi::modifyRange(
            context$selection[[1]]$range,
            code,
            context$id
          )
          status("Changes applied successfully")
          show_diff_view(FALSE)  # Hide diff view after applying
        } else {
          status("No active selection to replace")
        }
      } else {
        status("No modified code to apply")
      }
    })

    # Send message to Claude
    shiny::observeEvent(input$send, {
      # Validate inputs
      if (input$api_key == "") {
        status("Please enter your Anthropic API key")
        return()
      }

      if (input$prompt == "") {
        status("Please enter a message")
        return()
      }

      status("Sending to Claude...")

      # Add user message to chat
      user_message <- list(
        role = "user",
        content = input$prompt
      )

      current_messages <- messages()
      current_messages[[length(current_messages) + 1]] <- user_message
      messages(current_messages)
      render_chat()

      # Prepare all messages for the API call
      msgs <- lapply(messages(), function(m) {
        list(role = m$role, content = m$content)
      })

      # Make API call
      tryCatch({
        # Create client
        client <- anthropic_client(input$api_key)

        # Make chat request
        response_text <- chat(
          client,
          messages = msgs,
          model = input$model,
          max_tokens = input$max_tokens,
          temperature = input$temperature
        )

        # Add Claude response to chat
        assistant_message <- list(
          role = "assistant",
          content = response_text
        )

        current_messages <- messages()
        current_messages[[length(current_messages) + 1]] <- assistant_message
        messages(current_messages)
        render_chat()

        # Check if response contains code and the original selection exists
        if (input$show_diff && selected_code() != "") {
          code_block <- extract_code(response_text)
          if (!is.null(code_block)) {
            modified_code(code_block)
            show_diff_view(TRUE)
          }
        }

        # Insert into document if requested
        if (input$insert_to_doc && requireNamespace("rstudioapi", quietly = TRUE)) {
          rstudioapi::insertText(response_text)
        }

        # Clear prompt
        shiny::updateTextAreaInput(session, "prompt", value = "")
        status("Response received")

      }, error = function(e) {
        status(paste("Error:", e$message))
      })
    })

    # Clear chat
    shiny::observeEvent(input$clear, {
      messages(list())
      render_chat()
      show_diff_view(FALSE)
      status("Chat cleared")
    })

    # Status output
    output$status <- shiny::renderText({
      status()
    })
  }

  # Run the app
  shiny::runGadget(ui, server, viewer = shiny::dialogViewer("Claude AI Assistant", width = 800, height = 800))
}

#' Simple logging utility for Claude addins
#'
#' @param message The message to log
#' @param level The log level (INFO, DEBUG, WARNING, ERROR)
#' @param addin The name of the addin generating the log
#' @export
claude_log <- function(message, level = "INFO", addin = "Claude") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_message <- sprintf("[%s] [%s] [%s] %s", timestamp, addin, level, message)
  cat(log_message, "\n")
}
