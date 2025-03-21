% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/completions.R
\name{generate_text}
\alias{generate_text}
\title{Generate Text with Claude}
\usage{
generate_text(
  client,
  prompt,
  model = client$selected_model,
  max_tokens = 1000,
  temperature = 0.7
)
}
\arguments{
\item{client}{An anthropic_client object}

\item{prompt}{The prompt to generate text from}

\item{model}{The model to use (defaults to client's selected model)}

\item{max_tokens}{Maximum number of tokens to generate}

\item{temperature}{Controls randomness (0-1)}
}
\value{
The generated text
}
\description{
Generate Text with Claude
}
