% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/indent.R
\name{Rd_indent}
\alias{Rd_indent}
\title{Indent Rd}
\usage{
Rd_indent(rd, indent.with = getOption("Rd::indent.with", "  "),
  recursive = TRUE, ..., no.first = is(rd, "Rd_tag"), .check = TRUE)
}
\arguments{
\item{rd}{an \link{Rd} container or an \link[=Rd_tag]{Rd tag}.}

\item{indent.with}{What to indent with. See \code{\link[=Rd_clean_indent]{Rd_clean_indent()}}.}

\item{recursive}{Indent recursively?}

\item{...}{Ignored but included for forward compatibility
and to force full names on subsequent parameters.}

\item{no.first}{if the first element should be indented.}

\item{.check}{check for valid Rd?}
}
\description{
Indent Rd
}
\examples{
(x <- Rd_description("line 1\\n", "line 2\\n"))
Rd_indent(x)
}
