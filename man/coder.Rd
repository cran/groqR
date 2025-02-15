% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coder.R
\name{coder}
\alias{coder}
\title{'Groq': Finish code}
\usage{
coder(code = NULL, language = "R", ...)
}
\arguments{
\item{code}{The code to be completed by 'Groq'. If not provided,
it will use what's copied on the clipboard.}

\item{language}{The programming language to code in. Defaults to R.}

\item{...}{Following arguments can be set manually or in .Renviron:
\code{GROQ_API_KEY}is the 'Groq API' key.
\code{model} Model choice. Default is mistral-7b-instruct.
\code{systemRole} System role; Default is: "You are a helpful assistant
with extensive knowledge of R programming."
\code{maxTokens} The maximum integer of completion tokens returned.
\code{temperature} The amount of randomness in the response,
valued between 0 inclusive and 2 exclusive. Higher values are more
random, and lower values are more deterministic.
\code{top_p} Nucleus sampling threshold, valued between 0 and 1.
\code{proxy} Default value is NULL.}
}
\value{
A character value with the response generated by 'Groq'.
}
\description{
'Groq': Finish code
}
\examples{
\dontrun{
cat(coder("# A function to square a vector\nsquare_each <- function("))
}

}
