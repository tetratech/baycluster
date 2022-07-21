# ####
#' @title Cross tabulate data. 
#' 
#' @description Cross tabulate data. Includes transformations, averaging over
#'   idVar., and data centering
#'   
#' @details The incoming data is intended to have the following fields:
#'   "station", "wqParm",  "wqLayer", "yearCal", "year", "month",  "day"
#'   "value". NOTE: need to clarify minimum data set and what happens with extra variables.
#' 
#' Processing include the following steps:  
#' 
#' Values are logtransformed if c.spec$dataTrans == "logtrans"
#' 
#' If idVar has more than one variable, a new column is constructed by
#' concatenating the columns in idVar.
#' 
#' Values are averaged over idVar (or the newly constructed column).
#' 
#' Values are cross tabulated such that idVar is unique and columns correspond
#' to profVar
#' 
#' If c.spec$dataCenter is TRUE then the data are centered by subtracting the
#' row mean from each value in the row
#' 
#' 
#' @param c.spec list for storing specifications for cluster analysis 
#' @param data input data to be cross tabulated
#' 
#' @examples 
#' \dontrun{
#' #TBD
#' 
#' }
#' 
#' @return cross tabulate data table
#' 
#' @seealso \code{\link{calcQuanClass}}
#' 
#' @importFrom rlang .data := 
#' @importFrom lubridate %m+% %m-% ymd decimal_date yday year month make_date floor_date ceiling_date is.Date
#' @importFrom dplyr %>% mutate select filter bind_rows case_when rename group_by
#' @importFrom dplyr distinct relocate left_join arrange between pull summarise ungroup
#' @importFrom tibble tibble as_tibble 
#' 
#' @export
#'
crossTabulate <- function(c.spec, data) {
  
  # ----< FUTURE DEVELOPMENT limitations >----
  {
  # FUTURE DEVELOPMENT: Notify user of 1 wqParm limitation
  if (length(unique(data$wqParm)) > 1) {
    c.spec$wqParm <- c.spec$wqParm[1]
    c.spec$pltVar <- c.spec$pltVar[1] 
    warning("Processing multiple variables: future development\n"
      , c.spec$wqParm, " selected for further steps.")
  }
  
  # FUTURE DEVELOPMENT: Notify user of 1 wqLayer limitation
  if (length(unique(data$wqLayer)) > 1) {
    c.spec$wqLayer <- c.spec$wqLayer[1] 
    warning("Processing multiple layers: future development\n"
      , c.spec$wqLayer, " selected for further steps.")
  }
  }
  
  # ----< data transformations >----
  dataTrans <- c.spec$dataTrans
  if (!is.null(dataTrans) && !is.na(dataTrans) && dataTrans == "logtrans") {
    data$value <- log(data$value)
  }
  
  # ----< cross tabulation >----
  {
  # Extract/Prep variables for next bit
  wqParm    <- c.spec$wqParm
  wqLayer   <- c.spec$wqLayer
  prof      <- vec.strg(c.spec$profVar, sep = "_")
  id        <- vec.strg(c.spec$idVar, sep = "_")  
  pltVar    <- paste0(c.spec$pltVar, ".")  
  
  # cross tabulate
  data1 <- data %>%
    filter(., wqParm == {{wqParm}}) %>%       # trim when more than 1 wqParm
    filter(., wqLayer == {{wqLayer}}) %>%     # trim when more than 1 wqLayer
    unite({{prof}}, c.spec$profVar, sep = "_", remove = FALSE) %>% # concat >1 profVar
    unite({{id}}, c.spec$idVar, sep = "_", remove = FALSE) %>%     # concat >1 idVar
    select(., {{prof}}, {{id}}, value) %>%                          # keep needed var
    group_by(., .data[[prof]], .data[[id]]) %>%                    # group
    summarise(., value=mean(value), .groups = "keep") %>%            # calc. average
    pivot_wider(., names_from = .data[[prof]], values_from = value, names_prefix = {{pltVar}} )
  }
  
  # ----< data centering >----
  dataCenter <- c.spec$dataCenter
  if (!is.null(dataCenter) && !is.na(dataCenter) && dataCenter == TRUE) {
    mn <- apply(data1[, 2:NCOL(data1)],1,mean, na.rm=TRUE)
    data1[, 2:NCOL(data1)] = sweep(data1[, 2:NCOL(data1)], MARGIN = 1, STATS = rowMeans(data1[, 2:NCOL(data1)]))
  }
  
  return(data1)
  
}

