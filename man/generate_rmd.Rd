% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper.R
\name{generate_rmd}
\alias{generate_rmd}
\title{Generate the Rmd file for the report}
\usage{
generate_rmd(x_seq, ubb, export = TRUE, file_name = "generated_document.Rmd")
}
\arguments{
\item{x_seq}{Sequence}

\item{ubb}{UBB}

\item{export}{Export}

\item{file_name}{File name}
}
\value{
Returns a plot
}
\description{
Functions combines the template and adds the Rmd content
from the get_rmd function
}
