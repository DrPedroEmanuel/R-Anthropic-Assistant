# README.md
# ranthropicR: R Interface to Anthropic Claude AI Models

## Overview

ranthropicR provides an R interface to Anthropic's Claude AI models, allowing you to integrate Claude's capabilities into your R workflows. This package supports text generation, chat-based interactions, and comes with a convenient RStudio addin for interactive use.

## Features

- Access to all available Claude models (Opus, Sonnet, Haiku)
- Support for both text completions and chat-based interactions
- Full control over generation parameters (temperature, max tokens, etc.)
- Interactive RStudio addin for easy chat with Claude
- Environment variable support for API key management

## Installation

You can install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("DrPedroEmanuel/ranthropicR")
```

## Authentication

You'll need an API key from Anthropic. You can set it in your environment:

```r
Sys.setenv(ANTHROPIC_API_KEY = "your-api-key-here")
```

For security, consider adding this to your `.Renviron` file or using the `dotenv` package.

## Basic Usage

```r
library(ranthropicR)

# Initialize client
client <- anthropic_client()

# List available models
models <- list_models(client)
print(models)

# Select a model
client <- select_model(client, "claude-3.5-sonnet-20240307")

# Generate text
response <- generate_text(
  client, 
  prompt = "Human: Explain R programming language in simple terms.\n\nAssistant:",
  max_tokens = 500,
  temperature = 0.7
)

# Chat interface
messages <- list(
  list(role = "user", content = "Explain R programming language in simple terms.")
)

response <- chat(
  client,
  messages = messages,
  temperature = 0.7
)

print(response)
```

## Using the RStudio Addin

The package includes an RStudio addin that provides a graphical interface for interacting with Claude:

1. Install the package
2. In RStudio, go to Addins → Claude Assistant
3. Enter your API key (or have it set in your environment)
4. Select your preferred model and parameters
5. Start chatting with Claude!

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This package is released under the MIT License.
