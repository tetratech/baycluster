# ####
#' @title Cross tabulate data. 
#' 
#' @description Cross tabulate data. Includes transformations, averaging over
#'   id_var., and data centering
#'   
#' @details The incoming data are intended to have the following fields:
#'   "station", "wq_parm",  "wq_layer", "yearCal", "year", "month",  "day"
#'   "value". JBH NOTE: need to clarify minimum data set and what happens with
#'   extra variables.
#' 
#' Processing include the following steps:  
#' 
#' Values are transformed based on value stored in c.spec$data_transform
#' 
#' If c.spec$id_var identifies more than one variable, a new column is
#' constructed by concatenating the columns identified by id_var.
#' 
#' Values are averaged over id_var (or the newly constructed column).
#' 
#' Values are centered based on the value of c.spec$data_center and performed
#' on an id_var basis
#' 
#' Values are cross tabulated such that id_var is unique and columns correspond
#' to prof_var
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
#' @seealso \code{\link{calcQuanClass}} \code{\link{transformData}} \code{\link{centerData}}
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

  # ----< transform if option is selected >----
  transform_type <- c.spec$data_transform
  if (!is.na(transform_type)) {
    data <- transformData(data, "value", transform_type)
  }
  
  # ----< filter and summarize by mean at the prof and id level >----
  {
    
    # Extract/Prep variables for next bit
    prof       <- vec.strg(c.spec$prof_var, sep = "_")
    id         <- vec.strg(c.spec$id_var, sep = "_")  
    plt_var    <- "v."  
    
    # filter and down-select variables by prof_var and id_var 
    data0 <- data %>%
      unite({{prof}}, c.spec$prof_var, sep = "_", remove = FALSE) %>% # concat >1 prof_var
      unite({{id}}, c.spec$id_var, sep = "_", remove = FALSE) %>%     # concat >1 id_var
      select(., all_of({{prof}}), all_of({{id}}), value) 
    
    # determine column order of final table based on appearance in data table at this time
    prof_lev <- as.character(unique(data0[[prof]]))
    prof_ord <- paste0(plt_var, prof_lev)
    id_lev     <- as.character(unique(data0[[id]]))
    
    # calculate mean by group
    data1 <- data0 %>%
      group_by(., .data[[prof]], .data[[id]]) %>%       
      summarise(., value=mean(value), .groups = "keep")    
    
  }
  
  # ----< center data if requested >----
  data_center <- c.spec$data_center
  if (!is.na(data_center)) {
    data1 <- centerData(data1, id, prof, "value", data_center)
  }
  
  # ----< make cluster ready data table >----
  {
    # convert from long to wide format
    data2 <- data1 %>%             
      pivot_wider(., names_from = .data[[prof]], values_from = value, names_prefix = {{plt_var}} )
    
    # convert to data frame and add rownames 
    data2 <- as.data.frame(data2)
    rownames(data2) <- data2[ ,1]
    
    # sort rows to match id var order and columns to match prof var order
    find_loc2 <- function(x, id_lev) which(x == id_lev)
    idx <- sapply(id_lev, FUN=find_loc2, data2[ ,1])
    data3 <- data2[idx, c(id, paste0(plt_var, prof_lev))]
  }
  
  # ----< return wide and long formatted data >----
 
  if (ret_data == 1) {
    ret.cT <- data3
  } else {
    ret.cT <- list(data_wide=data3, data_long = data1)
  }
  
  return(ret.cT)
  
} # end ~ function: crossTabulate