% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transformData.R
\name{transformData}
\alias{transformData}
\title{Transform column of data}
\usage{
transformData(data, value_col, transform_type = NA, transform_col = NA)
}
\arguments{
\item{data}{Data set to analyze. Must have one column: \code{value_col}
which contains the values to transform}

\item{value_col}{Column name that contains values for transforming}

\item{transform_type}{type of transformation to perform. If \code{transform_type =
"logtransform"}, then values are log transformed. If \code{transform_type =
NA}, then an unchanged data set is returned. See Details for other transformations.}

\item{transform_col}{Column name to store transformed data. If left as NA, then
\code{value_col} will be overwritten.}
}
\value{
data set with transformed column
}
\description{
Transform column of data
}
\details{
This function transforms a column of data from a table. Allowable transformations are
\itemize{
\item "logtransform": natural log transformation
\item "exptransform": exp(x) transform
\item "logbase10": log base 10 
\item "antilog10": 10^ (anti-log for base 10 logarithms)
}

Negative values in \code{value_col} are not permitted for "logtransform" and "logbase10" options.
}
\examples{
df <- data.frame(id_row = 1:10, values_in = runif(10))
df <- transformData(df, "values_in", "logtransform", "values_out")
df

}
\seealso{
\code{\link{calcQuanClass}}
}
\keyword{internal}
