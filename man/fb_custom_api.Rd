% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fb_custom_api.R
\name{fb_custom_api}
\alias{fb_custom_api}
\title{Get custom API request string}
\usage{
fb_custom_api(request, method = "GET", api_version = "v10.0", body = NULL)
}
\arguments{
\item{request}{A Custom request path for the API}

\item{method}{The request method}

\item{api_version}{The version of the API to use}

\item{body}{What to include in the request body}
}
\description{
Custom request to the Facebook API with json encoded string
}
\examples{
fb_custom_api(request, method="GET", api_version="10.0")
}
