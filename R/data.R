# c.spec_qc
#' @title Variable list for performing cluster analysis.
#' 
#' @description Variable list for performing cluster analysis using
#'   predictions from GAM results. (Future updates for WRTDS files.)
#' 
#' @format A data frame with the following variables:
#' \describe{
#'   \item{Variable}{Variable name}
#'   \item{Description}{short description of variable}
#'   \item{warnGAM}{warnGAM}
#'   \item{warnWRTDS}{warnWRTDS}
#'   \item{Type}{Type}
#'   \item{Where}{Where}
#'   \item{DescriptionDetailed}{DescriptionDetailed}
#' }
"c.spec_qc"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# shp_cbseg ####
#' @title Base layer for making maps in the tidal waters of the 
#' Chesapeake Bay.
#' 
#' @description Base layer for making maps in the tidal waters of the 
#' Chesapeake Bay.
#' 
#' @format A data frame with the following variables:
#' \describe{
#'   \item{long}{longitude}
#'   \item{lat}{latitude}
#'   \item{order}{order with in a piece}
#'   \item{hole}{hole}
#'   \item{piece}{piece}
#'   \item{id}{id}
#'   \item{group}{group}
#' }
"shp_cbseg"