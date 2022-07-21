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
#' \item wqParm - 
#' \item wqLayer - 
#' \item yearCal - year (calendar basis)   
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
  varsNeeded <- c("gamFolder", "chkRDA", "basePred")
  extract(c.spec, varsNeeded)

  # ----< create predictions >----
  {
    # downselect basePred to minimum data ####
    basePred0 <- basePred %>%
      select(., year, yearAdj, month, day, doy, dyear)
    
    # for each row in chkRDA ####
    for (k1 in 1:NROW(chkRDA)) {
      
      # load gamResult from baytrends output
      load(file.path(gamFolder, chkRDA$wqParm[k1], chkRDA$fileName[k1]))
      
      # expand base prediction data set for ith row ####
      pred0 <- basePred0 %>%
        mutate(.
          , station  = chkRDA$station[k1]
          , wqParm   = chkRDA$wqParm[k1]
          , wqLayer  = chkRDA$wqLayer[k1]
          , cyear    = dyear - gamResult$iSpec$centerYear
          , intervention = tail(gamResult$iSpec$intervenList$intervention,1)
          , flw_sal  = 0)
      
      # make prediction ####
      pred0$value <- predict(gamResult[[paste0("gamOutput",chkRDA$gamOptionSel[k1])]][["gamRslt"]]
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
      select(., station, wqParm, wqLayer, year, yearAdj, month, day, value) %>%
      rename(., yearCal = year, year=yearAdj)
    
  } # end ~ create predictions
  
  return(pred)
  
} # end ~ function: createPredictions
