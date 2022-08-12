# ####
#' @title Create GAM prediction data set
#' 
#' @description Create GAM prediction data set
#'   
#' @details 
#' 
#' The user can either specify the arguments for this function via the list
#' \code{c.spec} (see \code{\link{setSpec}}); or by specifying all of the below
#' variables individually.
#' 
#' \itemize{
#' \item \strong{chk_rda} -- data table with a summary of files and GAM result availability. See output from \code{\link{chkRDAfiles}}.
#' \item \strong{base_pred} -- data table with base prediction data set. See output from \code{\link{createBasePred}}.
#' }
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
#' \item station - station 
#' \item wq_parm - water quality parameter
#' \item wq_layer - water quality layer
#' \item year_cal - year (calendar basis)   
#' \item year - year for cluster analysis
#' \item month - month   
#' \item day - day of month   
#' \item value - prediction
#' }
#' 
#' @seealso  \code{\link{chkRDAfiles}} \code{\link{createBasePred}}
#'   \code{\link{createPredGAM}} \code{\link{crossTabulate}}
#'   \code{\link{transformData}} \code{\link{centerData}}
#'   \code{\link{clusterData}}
#' 
#' @importFrom rlang .data := 
#' @importFrom lubridate %m+% %m-% ymd decimal_date yday year month make_date floor_date ceiling_date is.Date
#' @importFrom dplyr %>% mutate select filter bind_rows case_when rename group_by
#' @importFrom dplyr distinct relocate left_join arrange between pull summarise ungroup
#' @importFrom tibble tibble as_tibble
#' @importFrom knitr kable 
#' @importFrom mgcv gam
#' @importFrom stats predict
#' @importFrom utils tail
#' 
#' @export
#' 
createPredGAM <- function(c.spec = NULL, ...) {
  
  # ----< load c.spec settings if provided >----
  vars = c("chk_rda", "base_pred")
  if (!is.null(c.spec)) {
    pry(c.spec, v = vars)
  } else {
    pry(list(...), v = vars)
  }

  # ----< error trap >----
  stopifnot(
    !is.null(chk_rda)
    , !is.null(base_pred)
  )
  
  # ----< create predictions >----
  {
    # downselect base_pred to minimum data ####
    base_pred0 <- base_pred %>%
      select(., year, year_adj, month, day, doy, dyear)
    
    # for each row in chk_rda ####
    for (k1 in 1:NROW(chk_rda)) {
      
      # load gamResult from baytrends output 
      # (assumes r object loaded will have be the variable: gamResult, a default of baytrends)
      load(file.path(chk_rda$file_folder[k1], chk_rda$wq_parm[k1], chk_rda$file_name[k1]))
      
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
