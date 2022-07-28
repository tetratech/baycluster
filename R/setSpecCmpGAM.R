# ####
#' @title Complete c.spec with GAM-specific supporting variables
#' 
#' @description Complete c.spec with GAM-specific supporting variables
#'   
#' @details 
#' ... 
#' 
#' @param c.spec list of cluster analysis specifications
#'  
#' @examples 
#' # TBD
#' 
#' @return list
#' 
#' @keywords internal
#' 
#' @seealso \code{\link{readTextFile}}
#' 
#' 
#' @importFrom tibble tibble as_tibble 
#' @importFrom scales col_numeric hue_pal
#' @importFrom assertthat not_empty see_if
#' @importFrom dplyr %>% mutate select filter bind_rows case_when rename group_by
#' 
#' @export
#' 
setSpecCmpGAM <- function(c.spec) {
  
  # ----< extract needed variables >----
  varsNeeded <- c("stat_vec", "start_year", "end_year", "month_grid", "day_grid"
    , "grp_cnt", "wq_parm", "wq_layer", "id_var", "prof_var", "month_adj"
    , "analysis_title", "analysis_date", "filename", "data_out", "ex_cov_class")
  pry(c.spec, varsNeeded)
  
  # ----< Plot Variable >----
  plt_var <- paste(wq_parm,"pred",sep=".") 
  
  # ----< Cluster group and colors >----
  grp_df  <- tibble(lab = paste("Group",1:grp_cnt)) %>%
    mutate(., grp_col = rev(scales::hue_pal()(grp_cnt))) %>%
    mutate(., grp_col = apply(as.data.frame(grp_col), 1, hexColor2Name)) 

  # ----< Exogenous Covariate Cluster group and colors >----
  ex_cov_col_fct <- scales::col_numeric(
    palette = c("red","lightblue","blue")
    , na.color = NA
    , domain = c(1,ex_cov_class))
  ex_cov_df <- tibble(lab = paste("Class",1:ex_cov_class)) %>%
    mutate(., ex_cov_col = ex_cov_col_fct(1:ex_cov_class)) %>%
    mutate(., ex_cov_col = apply(as.data.frame(ex_cov_col), 1, hexColor2Name)) 
  
  # ----< ID variable label >----
  if (length(id_var)==2) {
    id_var_lab <- vec.strg(id_var,sep='&')
  } else {
    id_var_lab <- paste(id_var)
  }
  
  # ----< output file names >----
  if (is.null(filename)) {
    filename <- paste("Cluster",analysis_title,wq_layer,wq_parm,"of"
      , id_var_lab,"by",prof_var,start_year,end_year,sep="_")
  }
  
  if (is.null(data_out)) {
    dataOut <- paste("ClusterGroup",analysis_title,wq_layer,wq_parm,"of"
      , id_var_lab,"by",prof_var,start_year,end_year,sep="_")
  }
  
  # ----< build base prediction data set >----
  
  base_pred <- createBasePred(
    start_year = start_year,
    end_year = end_year,
    month_grid = month_grid,
    day_grid = day_grid,
    month_adj = month_adj
  ) %>%
    structure( out.attrs = NULL
      , date_created = NULL
      , start_year = NULL
      , end_year = NULL
      , month_grid = NULL
      , day_grid = NULL
      , month_adj = NULL
      , first_day = NULL
      , last_day = NULL
      , month_order = NULL
    )
  
  
  # ----< append variables to list for return >----
  vars2append <- c("plt_var", "id_var_lab", "filename", "data_out"
    , "grp_df", "ex_cov_df", "base_pred")

  for (var in vars2append) {
    c.spec[[var]] <- eval(parse(text=var))
  }  
  
  return(c.spec)
  
} # end ~ function: setSpecCmpGAM