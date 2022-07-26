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
#' @param retData 1: return wide data, 2: return list of wide and long data
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
#' @importFrom tidyr unite
#' 
#' @export
#'
crossTabulate <- function(c.spec, data, retData=1) {
  
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
  
  # ----< filter and summarize by mean at the prof and id level >----
  {
    # Extract/Prep variables for next bit
    wqParm    <- c.spec$wqParm
    wqLayer   <- c.spec$wqLayer
    prof      <- vec.strg(c.spec$profVar, sep = "_")
    id        <- vec.strg(c.spec$idVar, sep = "_")  
    pltVar    <- paste0(c.spec$pltVar, ".")  
    
    # filter and summarize by mean by profVar and idVar level
    data <- data %>%
      filter(., wqParm == {{wqParm}}) %>%       # trim when more than 1 wqParm
      filter(., wqLayer == {{wqLayer}}) %>%     # trim when more than 1 wqLayer
      unite({{prof}}, c.spec$profVar, sep = "_", remove = FALSE) %>% # concat >1 profVar
      unite({{id}}, c.spec$idVar, sep = "_", remove = FALSE) %>%     # concat >1 idVar
      select(., {{prof}}, {{id}}, value) %>%                          # keep needed var
      group_by(., .data[[prof]], .data[[id]]) %>%                    # group
      summarise(., value=mean(value), .groups = "keep")
    
    # substitute prof var idLev for idLab
    profLev <- as.character(c.spec$idLev[[prof]])
    profLab <- as.character(c.spec$idLab[[prof]])
    
    if (is.numeric(data[[prof]])) {
      data[[prof]] <- as.character(data[[prof]])
    }

    Lev2Lab <- function(x) profLab[x == profLev]
    data[[prof]] <- sapply(data[[prof]],Lev2Lab)  
  }
  
  # ----< center data if requested >----
  {
    dataCenter <- c.spec$dataCenter
    if (!is.null(dataCenter) && !is.na(dataCenter) && dataCenter == TRUE) {
      
      # calculate mean by idVar
      data.mn <- data %>%                     
        group_by(., .data[[id]]) %>%                        
        summarise(., mn=mean(value)) #, .groups = "keep")
      
      # subtract mean      
      data <- left_join(data, data.mn, by = {{id}} ) %>%
        mutate(., value = value - mn) %>%
        select(., {{prof}}, {{id}}, value) 
    }
  }
  
  # ----< make cluster ready data table >----
  {
    # convert from long to wide format
    data1 <- data %>%             
      pivot_wider(., names_from = .data[[prof]], values_from = value, names_prefix = {{pltVar}} )
    
    # convert to data frame and add rownames 
    data1 <- as.data.frame(data1)
    rownames(data1) <- data1[ ,1]
    
    # sort data columns to match prof var order
    data1 <- data1[, c(id, paste0(pltVar, profLab))]
  }
  
  # ----< return wide and long formatted data >----
 
  if (retData == 1) {
    lst <- data1
  } else {
    lst <- list(dataW=data1, dataL = data, data.mn=data.mn)
  }
  
  return(lst)
  
}