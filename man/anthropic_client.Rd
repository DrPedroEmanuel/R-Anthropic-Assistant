% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/anthropic.R
\name{anthropic_client}
\alias{anthropic_client}
\title{Anthropic API Client}
\usage{
anthropic_client(
  api_key = Sys.getenv("ANTHROPIC_API_KEY"),
  base_url = "https://api.anthropic.com",
  api_version = "2023-06-01"
)
}
\arguments{
\item{api_key}{Your Anthropic API key (defaults to ANTHROPIC_API_KEY environment variable)}

\item{base_url}{The base URL for the Anthropic API}

\item{api_version}{The Anthropic API version to use}
}
\value{
An anthropic_client object
}
\description{
R interface to Anthropic's Claude AI models
}
