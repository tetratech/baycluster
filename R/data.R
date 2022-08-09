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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# cbp_stat_list ####
#' @title Station vectors for selections portions of Chesapeake Bay
#' tidal waters.
#' 
#' @description Station vectors for selections portions of Chesapeake Bay
#' tidal waters.
#' 
#' @format A list with the following variables:
#' \describe{
#'   \item{summary}{Summary table of abbreviations, descriptions and stations}            
#'   \item{cbp}{Main Bay Stations}            
#'   \item{u_cbp}{Upper Bay Stations}           
#'   \item{md_cbp}{MD Bay Stations}              
#'   \item{va_cbp}{VA Bay Stations}              
#'   \item{md_wt}{MD Western Tributary Stations}
#'   \item{md_et}{MD Eastern Tributary Stations}
#'   \item{va_we}{VA Western Embayment Stations}
#'   \item{va_ee}{VA Eastern Embayment Stations}
#'   \item{pax}{Patuxent Stations}            
#'   \item{pot}{Potomac Stations}             
#'   \item{rpp}{Rappahannock Stations}        
#'   \item{yrk}{York Stations}                
#'   \item{jms}{James Stations}               
#'   \item{ebe}{Elizabeth River Stations} 
#' }
"cbp_stat_list"








