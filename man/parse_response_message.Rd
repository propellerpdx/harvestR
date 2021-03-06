% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_request.R
\name{parse_response_message}
\alias{parse_response_message}
\title{Get Response Messages}
\usage{
parse_response_message(response, content, table)
}
\arguments{
\item{response}{harvest API response}

\item{content}{content from harvest API response

importFrom glue glue}
}
\description{
Sends messages to console regarding the response. This is implemented because,
depending on the selected strategy for the future::plan, messages from httr::GET may not be
surfaced to the console due to multiple R sessions running and stderror relaying issues.
}
