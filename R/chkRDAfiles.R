# ####
#' @title Check RDA files to confirm availability of GAM results
#' 
#' @description Based on a station list, year range, water quality variable,
#'   layer, and GAM option, confirm that the `gamResults` from
#'   baytrends are correctly positioned.
#'   
#' @details 
#' It is presumed that a series of baytrends operations
#' have been run and the results for an individual "station|parameter|layer"
#' have been stored as separate rda files in a folder passed as the variable
#' \code{rdaFolder} as depicted below.
#' 
#' \code{rdaFolder/tn/TF3.1E_tn_surf.rda}\cr  
#' \code{rdaFolder/tn/TF3.2E_tn_surf.rda}\cr 
#' \code{rdaFolder/tn/...}\cr
#' \code{rdaFolder/do/TF3.1E_do_surf.rda}\cr
#' \code{rdaFolder/do/TF3.2E_do_surf.rda}\cr 
#' \code{rdaFolder/do/...}\cr
#'            
#' The values of \code{station}, \code{wqParm}, and \code{layer} are used to 
#' construct the file name convention of: "station_wqParm_layer.rda". Under the 
#' folder \code{rdaFolder}, subfolders are organized by \code{wqParm}. 
#' 
#' \code{gamNumbr} is used to define which GAM formula to use in the analysis.
#' Setting \code{gamNumbr = c(23, 3, 2)} would result in preferentially
#' selecting \code{gamOutput23}, then \code{gamOutput3}, and finally \code{gamOutput2}.
#' 
#' \code{startYear} and \code{endYear} are the planned beginning and ending
#' period of the cluster analysis. This function checks to confirm that a 
#' particular combination of \code{station}, \code{wqParm}, and \code{layer} 
#' had monitoring data for the full period or record, i.e., \code{startYear}-01-01 
#' to \code{endYear}-12-31. 
#' 
#' \code{month_threshold} provides a grace period in the interpretation of a full
#' period of record.
#' 
#' @param stationVec Vector of stations to analyze 
#' @param wqParm Parameter abbreviation to evaluate 
#' @param layer Layer abbreviation to evaluate
#' @param gamNumbr GAM option to evaluate 
#' @param startYear Begin year of analysis (scalar)
#' @param endYear End year of analysis (scalar)
#' @param rdaFolder Folder location of GAM results from baytrends
#' @param month_threshold Threshold for flagging acceptable data range (see details)
#'  
#' @examples 
#' \dontrun{
#' # TBD
#' 
#' }
#' 
#' @return data table with a summary of files and GAM results
#' 
#' @seealso \code{\link{readTextFile}}
#' 
#' @importFrom rlang .data
#' @importFrom lubridate %m+% %m-% ymd decimal_date yday year month make_date floor_date ceiling_date is.Date
#' @importFrom dplyr %>% mutate select filter bind_rows case_when rename group_by
#' @importFrom dplyr distinct relocate left_join arrange between pull summarise ungroup
#' @importFrom tibble tibble as_tibble
#' @importFrom knitr kable 
#' @importFrom ggplot2 ggplot aes theme_bw ylab xlab scale_y_date theme
#' @importFrom ggplot2 geom_hline geom_point scale_shape_manual scale_fill_manual 
#' @importFrom ggplot2 geom_vline ggtitle labs expansion element_text guide_legend
#' @importFrom ggplot2 alpha
#' @importFrom knitr kable
#' 
#' @export
#' 
chkRDAfiles <- function(
    stationVec
  , wqParm 
  , layer
  , gamNumbr = c(23, 3, 2)
  , startYear = 1990
  , endYear = year(Sys.Date()) 
  , rdaFolder = ".."
  , month_threshold = 2) {  
  
  # ----< Create data table of files to look for >----
  {
    file.log <- 
      tibble(expand.grid(station = stationVec
        , wqParm = wqParm
        , layer = layer)) %>%
      mutate(.
        , fileBase = paste(station, wqParm, layer, sep = "_")
        , fileName = paste0(fileBase,".rda")
        , fileExists = file.exists(file.path(rdaFolder, wqParm, fileName))
        , gamOptionEvalAvail = NA_character_
        , gamOptionSel = NA_character_
        , dataMinDate = as.Date(NA)
        , dataMaxDate = as.Date(NA)
      )
  } # end ~ Create data table of files to look for
  
  # ----< File available | GAM # | dates >----
  {
    # loop through each file
    for (k1 in 1:NROW(file.log)) {
      
      if (file.log$fileExists[k1]) {
        # When a gamResult file exists: ####
        
        # Load the gam result 
        load(file.path(rdaFolder, unlist(file.log[k1,"wqParm"]), file.log[k1,"fileName"]))
        
        # Find all gamOption## in gamResult ####
        x1 <- names(gamResult)[substr(names(gamResult), 1, 9) == "gamOutput"]
        
        # Determine which gamOption## were evaluated ####
        x2 <- NA
        for (k2 in 1:length(x1)) {
          x2[k2] <- length(gamResult[[x1[k2]]]) > 1
        }
        gamOptionEval <- x1[x2]
        file.log[k1,"gamOptionEvalAvail"] <- paste0(gsub("gamOutput", "", gamOptionEval), "", collapse = ", ")   
        
        # Pick the gam option in preferential order as listed in gamNumbr ####
        k3    <- 1
        gotit <- FALSE
        gamOptionSel <- NA_character_
        
        while (!gotit & k3 <= length(gamNumbr)) {
          if (paste0("gamOutput", gamNumbr[k3]) %in% gamOptionEval) {
            gamOptionSel <- paste0("gamOutput", gamNumbr[k3])
            gotit <- TRUE
          } else {
            k3 <- k3 + 1
          }
        } # end ~ while (!gotit)
        
        # populate data table with which GAM option to use and dates ####
        file.log[k1,"gamOptionSel"] <- gsub("gamOutput", "", gamOptionSel)
        file.log[k1,"dataMinDate"] <- gamResult$iSpec$dateBegin
        file.log[k1,"dataMaxDate"] <- gamResult$iSpec$dateEnd
        
      } else {
        # When a gamResult file does not exists:
        next
      }
      
    } # end ~ for (k1 in 1:NROW(file.log))
    
    # Determine Concerns about date range of data ####
    file.log <- file.log %>%
        mutate(.
          , dataMinDateQC = case_when(dataMinDate <= make_date(startYear,1,1) %m+% months(month_threshold) ~ "Ok"
          , dataMinDate > make_date(startYear,1,1) %m+% months(month_threshold) ~ "Concern"
            , TRUE ~ NA_character_)
          , dataMaxDateQC = case_when(dataMaxDate >= make_date(endYear,12,31) %m-% months(month_threshold) ~ "Ok"
            , dataMaxDate < make_date(endYear,12,31) %m-% months(month_threshold) ~ "Concern"
            , TRUE ~ NA_character_) 
        )
    
  } # end ~ File available | GAM # | dates
  
  # ----< Organize plot settings >----
  {
    # compute min | max date range ####
    ylim <- range(file.log$dataMinDate
      , file.log$dataMaxDate
      , make_date(startYear,1,1)
      , make_date(endYear,12,31)
      , na.rm = TRUE)
    
    # identify files not found ####
    site0 <- file.log %>%
      filter(., !fileExists)
    
    # identify stations where file found but no eligible gam found ####
    site1 <- file.log %>%
      filter(., fileExists & is.na(gamOptionSel))
    
    
    # Determine Concerns about date range of data ####
    file.log.toPlot <- bind_rows(
      # data not collected early enough
      file.log %>%
        mutate(.
          , Date = dataMinDate
          , Type = "Minimum"
          , Warning = dataMinDateQC
        ) %>%
        select(., station, fileBase, Date, Type, Warning)
      
      # data not collected recently enough
      , file.log %>%
        mutate(.
          , Date = dataMaxDate
          , Type = "Maximum"
          , Warning = dataMaxDateQC
        ) %>%
        select(., station, fileBase, Date, Type, Warning)
    )
    
    file.log.toPlot$fileBase <- factor(file.log.toPlot$fileBase
      , levels = file.log$fileBase)
    
  } # end ~ Organize plot settings
  
  # ----< plot data >----
  {
    p <- ggplot(file.log.toPlot, aes(x=fileBase, y=Date, shape = Type, fill = Warning)) +
      theme_bw() +
      ylab("Date") +
      xlab("Station + wqParm + layer") +
      scale_y_date(limits = ylim, expand = expansion(mult = c(0.2, 0.2))) +
      theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
      # add startYear and endYear for reference 
      geom_hline(aes(yintercept = c(make_date(startYear,1,1))), color="black") +
      geom_hline(aes(yintercept = c(make_date(endYear,12,31))), color="black", linetype=2) + 
      # add station dates
      geom_point(size = 3) +
      scale_shape_manual(values = c(25, 24)) + 
      scale_fill_manual(values=c("red", "white"),
        guide = guide_legend(override.aes = list(shape=21, size=4))) + 
      # Highlight stations with no data file found in magenta
      geom_vline(data= site0, aes(xintercept = fileBase), color=alpha("magenta", 0.3), size=5) + 
      # Highlight stations with a data file but no eligble gam found in yellow
      geom_vline(data= site1, aes(xintercept = fileBase), color=alpha("yellow", 0.3), size=5) + 
      # Fill in station/dates that do not meet date threshold
      labs(title = "GAM File Availability"
        , subtitle = paste0("Parameter: "
          , paste0(wqParm, "", collapse = ", ")
          , "   Layer: "
          , paste0(layer, "", collapse = ", ")
          )
        , caption = "Puple: File not found.\nYellow: Correct GAM not found.") +
      labs(shape = "Date", fill = "Condition")
  } # end ~ plot data
  
  .F("GAM file availability.")
  suppressWarnings(print(p))
  
  # ----< output report >----
  {
    .T(paste0("GAM file availability (parameter: "
      , paste0(wqParm, "", collapse = ", ")
      , ",   layer: "
      , paste0(layer, "", collapse = ", ")
      , ").") , t="e")
    print(kable(file.log[,c("station", "fileName", "fileExists"
      , "gamOptionEvalAvail", "gamOptionSel", "dataMinDate", "dataMinDateQC"
      , "dataMaxDate", "dataMaxDateQC")]
      , col.names = c("Station", "File", "Exist?", "Available", "Selected"
        , "Min. Date", "QC",  "Max. Date", "QC")
      , align=c("l","l","l","l","l","r","l","r","l")))
  } # end ~ output report
  
  return(file.log)
  
} # end ~ function: chkRDAfiles
