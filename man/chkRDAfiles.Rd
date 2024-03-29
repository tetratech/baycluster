% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chkRDAfiles.R
\name{chkRDAfiles}
\alias{chkRDAfiles}
\title{Check RDA files to confirm availability of GAM results}
\usage{
chkRDAfiles(c.spec = NULL, ...)
}
\arguments{
\item{c.spec}{cluster specification file}

\item{...}{alternative variable passing}
}
\value{
data table with a summary of files and GAM result availability
}
\description{
Based on a station list, year range, water quality variable,
  layer, and GAM option, confirm that the `gamResults` files from
  baytrends are correctly positioned.
}
\details{
It is presumed that a series of baytrends operations
have been run and the files for individual "station|parameter|layer" results
have been stored as separate rda files in a folder passed as the variable
\code{gam_folder} as depicted below.

\code{gam_folder/tn/TF3.1E_tn_surf.rda}\cr  
\code{gam_folder/tn/TF3.2E_tn_surf.rda}\cr 
\code{gam_folder/tn/...}\cr
\code{gam_folder/do/TF3.1E_do_surf.rda}\cr
\code{gam_folder/do/TF3.2E_do_surf.rda}\cr 
\code{gam_folder/do/...}\cr
           
The user can either specify the arguments for this function via the list
\code{c.spec} (see \code{\link{setSpec}}); or by specifying all of the below
variables individually.
           
\itemize{
\item \strong{stat_vec} -- vector of stations to analyze 
\item \strong{wq_parm} -- parameter abbreviation to evaluate (subfolder within \code{gam_folder})
\item \strong{wq_layer} -- layer abbreviation to evaluate
\item \strong{gam_numbr} -- GAM option to evaluate 
\item \strong{start_year} -- Begin year of analysis (scalar)
\item \strong{end_year} -- End year of analysis (scalar)
\item \strong{gam_folder} -- Folder location of GAM results from baytrends
\item \strong{month_grace_period} -- Threshold for flagging acceptable data range
}        
           
The values of \code{stat_vec}, \code{wq_parm}, and \code{wq_layer} are used to 
construct the file name convention of: "station_parameter_layer.rda". Under the 
folder \code{gam_folder}, subfolders are organized by \code{wq_parm}. 

\code{gam_numbr} is used to define which GAM formula to use in the analysis.
Setting \code{gam_numbr = c(23, 3, 2)} would result in preferentially
selecting \code{gamOutput23}, then \code{gamOutput3}, and finally \code{gamOutput2}.

\code{start_year} and \code{end_year} are the planned beginning and ending
period of the cluster analysis. This function checks to confirm that a 
particular combination of \code{station}, \code{wq_parm}, and \code{wq_layer} 
had monitoring data for the full period or record, i.e., \code{start_year}-01-01 
to \code{end_year}-12-31. 

\code{month_grace_period} provides a grace period in the interpretation of a full
period of record.
}
\examples{
\dontrun{
# TBD

}

}
\seealso{
\code{\link{chkRDAfiles}} \code{\link{createBasePred}}
  \code{\link{createPredGAM}} \code{\link{crossTabulate}}
  \code{\link{transformData}} \code{\link{centerData}}
  \code{\link{clusterData}}
}
