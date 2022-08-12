# ####
#' @title Cross tabulate data. 
#' 
#' @description Cross tabulate data. Includes transformations, averaging over
#'   id_var, and data centering
#'   
#' @details 
#' 
#' The user can pass arguments \code{id_var}, \code{prof_var},
#' \code{data_transform}, and \code{data_center} for this function via the list
#' \code{c.spec} (see \code{\link{setSpec}}); or by specifying variables
#' individually.
#' 
#' \itemize{
#' \item \strong{id_var} -- items that are to be clustered
#' \item \strong{prof_var} -- attribute to cluster by
#' \item \strong{data_transform} -- data transformation
#' \item \strong{data_center} -- data centering
#' } 
#' 
#' The incoming \code{data} are intended to have the following fields:
#' "station", "wq_parm",  "wq_layer", "yearCal", "year", "month",  "day"
#' "value". See output from \code{\link{createBasePred}}.
#' 
#' Processing includes the following steps:  
#' 
#' \itemize{
#' \item {\code{value} is transformed based on value stored in
#' \code{data_transform}.}
#' \item {If \code{id_var} identifies more than one variable, a new column is
#' constructed by concatenating the columns identified by \code{id_var}.}
#' \item {code{value} is averaged over \code{id_var} (or the newly constructed
#' column).}
#' \item {code{value} is centered based on the value of \code{data_center} and
#' performed on an \code{id_var} basis.}
#' \item {code{value} is cross tabulated such that \code{id_var} is unique in column
#' 1 of the returned table and the remaining columns correspond to varying
#' levels of \code{prof_var}.}
#' }
#' 
#' @param c.spec list for storing specifications for cluster analysis 
#' @param data input data to be cross tabulated. See output from \code{\link{createBasePred}}.
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
#' @seealso  \code{\link{chkRDAfiles}} \code{\link{createBasePred}}
#'   \code{\link{createPredGAM}} \code{\link{crossTabulate}}
#'   \code{\link{transformData}} \code{\link{centerData}}
#'   \code{\link{clusterData}}
#' 
#' @importFrom rlang .data := 
#' @importFrom lubridate %m+% %m-% ymd decimal_date yday year month make_date floor_date ceiling_date is.Date
#' @importFrom dplyr %>% mutate select filter bind_rows case_when rename group_by all_of desc count last_col
#' @importFrom dplyr distinct relocate left_join arrange between pull summarise ungroup
#' @importFrom tibble tibble as_tibble 
#' @importFrom tidyr unite
#' 
#' @export
#'
crossTabulate <- function(
    c.spec = NULL
  , data 
  , ret_data = 1
  , ...) {
  
  # ----< load c.spec settings if provided >----
  vars = c("id_var", "prof_var", "data_transform", "data_center")
  if (!is.null(c.spec)) {
    pry(c.spec, v = vars)
  } else {
    pry(list(...), v = vars)
  }
  
  # ----< error trap >----
  stopifnot(
    !is.null(data)
    , !is.null(id_var)
    , !is.null(prof_var)
    , !is.null(data_transform)
    , !is.null(data_center)
  )

  # ----< transform if option is selected >----
  if (!is.na(data_transform)) {
    data <- transformData(data, "value", data_transform)
  }
  
  # ----< filter and summarize by mean at the prof and id level >----
  {
    # Extract/Prep variables for this section
    prof       <- vec.strg(prof_var, sep = "_")
    id         <- vec.strg(id_var, sep = "_")  
    plt_var    <- "v."  
    
    # filter and down-select variables by prof_var and id_var 
    data0 <- data %>%
      unite({{prof}}, prof_var, sep = "_", remove = FALSE) %>% # concat >1 prof_var
      unite({{id}}, id_var, sep = "_", remove = FALSE) %>%     # concat >1 id_var
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