# ####
#' @title Cross tabulate data. 
#' 
#' @description Cross tabulate data. Includes transformations, averaging over
#'   id_var., and data centering
#'   
#' @details The incoming data is intended to have the following fields:
#'   "station", "wq_parm",  "wq_layer", "yearCal", "year", "month",  "day"
#'   "value". NOTE: need to clarify minimum data set and what happens with extra variables.
#' 
#' Processing include the following steps:  
#' 
#' Values are logtransformed if c.spec$data_transform == "logtrans"
#' 
#' If id_var has more than one variable, a new column is constructed by
#' concatenating the columns in id_var.
#' 
#' Values are averaged over id_var (or the newly constructed column).
#' 
#' Values are cross tabulated such that id_var is unique and columns correspond
#' to prof_var
#' 
#' If c.spec$data_center is TRUE then the data are centered by subtracting the
#' row mean from each value in the row
#' 
#' 
#' @param c.spec list for storing specifications for cluster analysis 
#' @param data input data to be cross tabulated
#' @param ret_data 1: return wide data, 2: return list of wide and long data
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
crossTabulate <- function(c.spec, data, ret_data=1) {
  
  # ----< FUTURE DEVELOPMENT limitations >----
  {
    # FUTURE DEVELOPMENT: Notify user of 1 wq_parm limitation
    if (length(unique(data$wq_parm)) > 1) {
      c.spec$wq_parm <- c.spec$wq_parm[1]
      c.spec$plt_var <- c.spec$plt_var[1] 
      warning("Processing multiple variables: future development\n"
        , c.spec$wq_parm, " selected for further steps.")
    }
    
    # FUTURE DEVELOPMENT: Notify user of 1 wq_layer limitation
    if (length(unique(data$wq_layer)) > 1) {
      c.spec$wq_layer <- c.spec$wq_layer[1] 
      warning("Processing multiple layers: future development\n"
        , c.spec$wq_layer, " selected for further steps.")
    }
  }
  
  # ----< data transformations >----
  data_transform <- c.spec$data_transform
  if (!is.null(data_transform) && !is.na(data_transform) && data_transform == "logtrans") {
    data$value <- log(data$value)
  }
  
  # ----< filter and summarize by mean at the prof and id level >----
  {
    # Extract/Prep variables for next bit
    wq_parm    <- c.spec$wq_parm
    wq_layer   <- c.spec$wq_layer
    prof      <- vec.strg(c.spec$prof_var, sep = "_")
    id        <- vec.strg(c.spec$id_var, sep = "_")  
    plt_var    <- paste0(c.spec$plt_var, ".")  
    
    # filter and summarize by mean by prof_var and id_var level
    data <- data %>%
      filter(., wq_parm == {{wq_parm}}) %>%       # trim when more than 1 wq_parm
      filter(., wq_layer == {{wq_layer}}) %>%     # trim when more than 1 wq_layer
      unite({{prof}}, c.spec$prof_var, sep = "_", remove = FALSE) %>% # concat >1 prof_var
      unite({{id}}, c.spec$id_var, sep = "_", remove = FALSE) %>%     # concat >1 id_var
      select(., {{prof}}, {{id}}, value) %>%                          # keep needed var
      group_by(., .data[[prof]], .data[[id]]) %>%                    # group
      summarise(., value=mean(value), .groups = "keep")
    
    # substitute prof var id_lev for id_lab
    prof_lev <- as.character(c.spec$id_lev[[prof]])
    prof_lab <- as.character(c.spec$id_lab[[prof]])
    
    if (is.numeric(data[[prof]])) {
      data[[prof]] <- as.character(data[[prof]])
    }

    Lev2Lab <- function(x) prof_lab[x == prof_lev]
    data[[prof]] <- sapply(data[[prof]],Lev2Lab)  
  }
  
  # ----< center data if requested >----
  {
    data_center <- c.spec$data_center
    if (!is.null(data_center) && !is.na(data_center) && data_center == TRUE) {
      
      # calculate mean by id_var
      data_mn <- data %>%                     
        group_by(., .data[[id]]) %>%                        
        summarise(., mn=mean(value)) #, .groups = "keep")
      
      # subtract mean      
      data <- left_join(data, data_mn, by = {{id}} ) %>%
        mutate(., value = value - mn) %>%
        select(., {{prof}}, {{id}}, value) 
    }
  }
  
  # ----< make cluster ready data table >----
  {
    # convert from long to wide format
    data1 <- data %>%             
      pivot_wider(., names_from = .data[[prof]], values_from = value, names_prefix = {{plt_var}} )
    
    # convert to data frame and add rownames 
    data1 <- as.data.frame(data1)
    rownames(data1) <- data1[ ,1]
    
    # sort data columns to match prof var order
    data1 <- data1[, c(id, paste0(plt_var, prof_lab))]
  }
  
  # ----< return wide and long formatted data >----
 
  if (ret_data == 1) {
    lst <- data1
  } else {
    lst <- list(data_wide=data1, data_long = data, data_mn=data_mn)
  }
  
  return(lst)
  
}