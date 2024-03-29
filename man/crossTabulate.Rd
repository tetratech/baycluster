% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crossTabulate.R
\name{crossTabulate}
\alias{crossTabulate}
\title{Cross tabulate data.}
\usage{
crossTabulate(c.spec = NULL, data, ret_data = 1, ...)
}
\arguments{
\item{c.spec}{list for storing specifications for cluster analysis}

\item{data}{input data to be cross tabulated. See output from \code{\link{createBasePred}}.}

\item{ret_data}{1: return wide data, 2: return list of wide and long data}

\item{...}{alternative variable passing}
}
\value{
cross tabulate data table
}
\description{
Cross tabulate data. Includes transformations, averaging over
  id_var, and data centering
}
\details{
The user can pass arguments \code{id_var}, \code{prof_var},
\code{data_transform}, and \code{data_center} for this function via the list
\code{c.spec} (see \code{\link{setSpec}}); or by specifying variables
individually.

\itemize{
\item \strong{id_var} -- items that are to be clustered
\item \strong{prof_var} -- attribute to cluster by
\item \strong{data_transform} -- data transformation
\item \strong{data_center} -- data centering
} 

The incoming \code{data} are intended to have the following fields:
"station", "wq_parm",  "wq_layer", "yearCal", "year", "month",  "day"
"value". See output from \code{\link{createBasePred}}.

Processing includes the following steps:  

\itemize{
\item {\code{value} is transformed based on value stored in
\code{data_transform}.}
\item {If \code{id_var} identifies more than one variable, a new column is
constructed by concatenating the columns identified by \code{id_var}.}
\item {code{value} is averaged over \code{id_var} (or the newly constructed
column).}
\item {code{value} is centered based on the value of \code{data_center} and
performed on an \code{id_var} basis.}
\item {code{value} is cross tabulated such that \code{id_var} is unique in column
1 of the returned table and the remaining columns correspond to varying
levels of \code{prof_var}.}
}
}
\examples{
\dontrun{
#TBD

}

}
\seealso{
\code{\link{chkRDAfiles}} \code{\link{createBasePred}}
  \code{\link{createPredGAM}} \code{\link{crossTabulate}}
  \code{\link{transformData}} \code{\link{centerData}}
  \code{\link{clusterData}}
}
