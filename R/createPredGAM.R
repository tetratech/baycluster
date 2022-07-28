# ####
#' @title Create prediction data 
#' 
#' @description Create prediction data 
#'   
#' @details 
#' ...
#' 
#' @param c.spec list of cluster analysis specifications
#'  
#' @examples 
#' \dontrun{
#' # TBD
#' }
#' 
#' @return data table with full prediction data set
#' \itemize{
#' \item station - 
#' \item wq_parm - 
#' \item wq_layer - 
#' \item year_cal - year (calendar basis)   
#' \item year - year for cluster 
#' \item month - month   
#' \item day - day of month   
#' \item value - prediction
#' }
#' 
#' @seealso \code{\link{readTextFile}}
#' 
#' @importFrom rlang .data := 
#' @importFrom lubridate %m+% %m-% ymd decimal_date yday year month make_date floor_date ceiling_date is.Date
#' @importFrom dplyr %>% mutate select filter bind_rows case_when rename group_by
#' @importFrom dplyr distinct relocate left_join arrange between pull summarise ungroup
#' @importFrom tibble tibble as_tibble
#' @importFrom knitr kable 
#' @importFrom mgcv gam
#' 
#' @export
#' 
createPredGAM <- function(c.spec) {
  
  # ----< extract needed variables from c.spec >----
  varsNeeded <- c("gam_folder", "chk_rda", "base_pred")
  pry(c.spec, varsNeeded)

  # ----< create predictions >----
  {
    # downselect base_pred to minimum data ####
    base_pred0 <- base_pred %>%
      select(., year, year_adj, month, day, doy, dyear)
    
    # for each row in chk_rda ####
    for (k1 in 1:NROW(chk_rda)) {
      
      # load gamResult from baytrends output
      load(file.path(gam_folder, chk_rda$wq_parm[k1], chk_rda$file_name[k1]))
      
      # expand base prediction data set for ith row ####
      pred0 <- base_pred0 %>%
        mutate(.
          , station  = chk_rda$station[k1]
          , wq_parm   = chk_rda$wq_parm[k1]
          , wq_layer  = chk_rda$wq_layer[k1]
          , cyear    = dyear - gamResult$iSpec$centerYear
          , intervention = tail(gamResult$iSpec$intervenList$intervention,1)
          , flw_sal  = 0)
      
      # make prediction ####
      pred0$value <- predict(gamResult[[paste0("gamOutput",chk_rda$gam_option_sel[k1])]][["gamRslt"]]
        , newdata = pred0)
      
      # compile predictions ####
      if (k1 == 1) {
        pred <- pred0
      } else {
        pred <- rbind(pred, pred0)
      }
      
    } # end ~ for (k1
    
    # down select final prediction data set to minimum columns ####
    pred <- pred %>%
      select(., station, wq_parm, wq_layer, year, year_adj, month, day, value) %>%
      rename(., year_cal = year, year=year_adj)
    
  } # end ~ create predictions
  
  return(pred)
  
} # end ~ function: createPredictions
