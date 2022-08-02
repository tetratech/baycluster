# ####
#' @title Complete c.spec with supporting variables
#' 
#' @description Complete c.spec with supporting variables
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
#' @importFrom scales col_numeric
#' @importFrom assertthat not_empty see_if
#' @importFrom dplyr %>% mutate select filter bind_rows case_when rename group_by
#' 
#' @export
#' 
setSpecCmp <- function(c.spec) {
  
  # ----< extract needed variables >----
  varsNeeded <- c("stat_vec", "start_year", "end_year", "month_grid", "day_grid"
    , "grp_cnt", "wq_parm", "wq_layer", "id_var", "prof_var", "month_adj"
    , "analysis_title", "analysis_date", "file_name", "data_out", "ex_cov_class")
  pry(c.spec, varsNeeded)

  # ----< Station setup: labels and order based on stat_vec >----
  stat_df <- tibble(stat_ord = 1:length(stat_vec)
    , stat_vec
    , stat_lab = stat_vec)
  
  if ("stations" %in% names(c.spec)) {
    stations <- c.spec$stations
    varFound <- grep("station", names(stations), ignore.case = TRUE , value = TRUE)
    stat_df <- merge(stat_df, stations, by.x = "stat_vec", by.y = varFound, all.x = TRUE)
  } 
  
  stat_df <- stat_df %>%
    arrange(., stat_ord) 
  
  # ----< Year setup: labels and order based on start_year and end_year >----
  year_vec = start_year:end_year
  year_df <- tibble(year_ord = 1:length(year_vec)
    , year_vec
    , year_lab =  paste(year_vec))
  
  # ----< Month setup: vector, labels and order based on month_grid and month_adj >----
  month_ord <- month_vec <- month_grid
  if (any(!is.na(month_adj))) {
    if (month_adj[1] > 0) {
      month_ord <- c(month_vec[month_vec %in% month_adj], month_vec[!(month_vec %in% month_adj)])
    } else {
      month_ord <- c(month_vec[!(month_vec %in% abs(month_adj))], month_vec[(month_vec %in% abs(month_adj))])
    }
  }
  month_df <- tibble(month_ord.1 = 1:length(month_ord)
    , month_vec = month_ord
    , month_lab = month.abb[month_ord]) %>%
    rename(., month_ord = month_ord.1)
  
  # ----< creating these lists makes it easy to switch between clustering by years, months, or stations. >----
  id_lev <- list(
    year = year_df$year_vec
    , month = month_df$month_vec
    , station = stat_df$stat_vec)
  
  id_lab <- list(
    year = year_df$year_lab
    , month = month_df$month_lab
    , station = stat_df$stat_lab)
  
  # ----< append variables to list for return >----
  vars2append <- c("stat_df", "year_df", "month_df", "id_lev", "id_lab")
  
  for (var in vars2append) {
    c.spec[[var]] <- eval(parse(text=var))
  }  
  
  # ----< datSource specific supplemental variables >----
  if(c.spec$dat_source == "gam") {
    c.spec <- setSpecCmpGAM(c.spec) 
  } else if (c.spec$datSource == "WRTDS") {
    c.spec <- setSpecCmpWRTDS(c.spec) 
  }
   
  return(c.spec)
  
} # end ~ function: setSpecCmp