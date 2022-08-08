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
#' \code{gam_folder} as depicted below.
#' 
#' \code{gam_folder/tn/TF3.1E_tn_surf.rda}\cr  
#' \code{gam_folder/tn/TF3.2E_tn_surf.rda}\cr 
#' \code{gam_folder/tn/...}\cr
#' \code{gam_folder/do/TF3.1E_do_surf.rda}\cr
#' \code{gam_folder/do/TF3.2E_do_surf.rda}\cr 
#' \code{gam_folder/do/...}\cr
#'            
#' The values of \code{station}, \code{wq_parm}, and \code{wq_layer} are used to 
#' construct the file name convention of: "station_parameter_layer.rda". Under the 
#' folder \code{gam_folder}, subfolders are organized by \code{wq_parm}. 
#' 
#' \code{gam_numbr} is used to define which GAM formula to use in the analysis.
#' Setting \code{gam_numbr = c(23, 3, 2)} would result in preferentially
#' selecting \code{gamOutput23}, then \code{gamOutput3}, and finally \code{gamOutput2}.
#' 
#' \code{start_year} and \code{end_year} are the planned beginning and ending
#' period of the cluster analysis. This function checks to confirm that a 
#' particular combination of \code{station}, \code{wq_parm}, and \code{wq_layer} 
#' had monitoring data for the full period or record, i.e., \code{start_year}-01-01 
#' to \code{end_year}-12-31. 
#' 
#' \code{month_threshold} provides a grace period in the interpretation of a full
#' period of record.
#' 
#' @param c.spec cluster specification file
#' @param station_vec Vector of stations to analyze 
#' @param wq_parm Parameter abbreviation to evaluate 
#' @param wq_layer Layer abbreviation to evaluate
#' @param gam_numbr GAM option to evaluate 
#' @param start_year Begin year of analysis (scalar)
#' @param end_year End year of analysis (scalar)
#' @param gam_folder Folder location of GAM results from baytrends
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
#' @importFrom rlang .data := 
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
    c.spec = NULL
  , station_vec = NULL
  , wq_parm  = NULL
  , wq_layer = NULL
  , gam_numbr  = NULL
  , start_year  = NULL
  , end_year  = NULL
  , gam_folder  = NULL
  , month_threshold  = NULL) {  

  # ----< Determine whether to use c.spec or other specifications
  args <- grabFunctionArguments()   # create list of function arguments
  
  if ("c.spec" %in% names(args)) {
   station_vec       = c.spec$stat_df$stat_vec
   wq_parm           = c.spec$wq_parm
   wq_layer          = c.spec$wq_layer
   gam_numbr         = c.spec$gam_numbr
   start_year        = c.spec$start_year
   end_year        = c.spec$end_year
   gam_folder        = c.spec$gam_folder
   month_threshold  = c.spec$month_grace_period
  }
    
  # ----< Create data table of files to look for >----
  {
    file_log <- 
      tibble(expand.grid(station = station_vec
        , wq_parm = wq_parm
        , wq_layer = wq_layer)) %>%
      mutate(.
        , file_base = paste(station, wq_parm, wq_layer, sep = "_")
        , file_name = paste0(file_base,".rda")
        , file_exists = file.exists(file.path(gam_folder, wq_parm, file_name))
        , gam_option_eval_avail = NA_character_
        , gam_option_sel = NA_character_
        , data_min_date = as.Date(NA)
        , data_max_date = as.Date(NA)
      )
  } # end ~ Create data table of files to look for
  
  # ----< File available | GAM # | dates >----
  {
    # loop through each file
    for (k1 in 1:NROW(file_log)) {
      
      if (file_log$file_exists[k1]) {
        # When a gamResult file exists: ####
        
        # Load the gam result 
        load(file.path(gam_folder, unlist(file_log[k1,"wq_parm"]), file_log[k1,"file_name"]))
        
        # Find all gamOption## in gamResult ####
        x1 <- names(gamResult)[substr(names(gamResult), 1, 9) == "gamOutput"]
        
        # Determine which gamOption## were evaluated ####
        x2 <- NA
        for (k2 in 1:length(x1)) {
          x2[k2] <- length(gamResult[[x1[k2]]]) > 1
        }
        gam_option_eval <- x1[x2]
        file_log[k1,"gam_option_eval_avail"] <- paste0(gsub("gamOutput", "", gam_option_eval), "", collapse = ", ")   
        
        # Pick the gam option in preferential order as listed in gam_numbr ####
        k3    <- 1
        gotit <- FALSE
        gam_option_sel <- NA_character_
        
        while (!gotit & k3 <= length(gam_numbr)) {
          if (paste0("gamOutput", gam_numbr[k3]) %in% gam_option_eval) {
            gam_option_sel <- paste0("gamOutput", gam_numbr[k3])
            gotit <- TRUE
          } else {
            k3 <- k3 + 1
          }
        } # end ~ while (!gotit)
        
        # populate data table with which GAM option to use and dates ####
        file_log[k1,"gam_option_sel"] <- gsub("gamOutput", "", gam_option_sel)
        file_log[k1,"data_min_date"] <- gamResult$iSpec$dateBegin
        file_log[k1,"data_max_date"] <- gamResult$iSpec$dateEnd
        
      } else {
        # When a gamResult file does not exists:
        next
      }
      
    } # end ~ for (k1 in 1:NROW(file_log))
    
    # Determine Concerns about date range of data ####
    file_log <- file_log %>%
        mutate(.
          , data_min_date_qc = case_when(data_min_date <= make_date(start_year,1,1) %m+% months(month_threshold) ~ "Ok"
          , data_min_date > make_date(start_year,1,1) %m+% months(month_threshold) ~ "Concern"
            , TRUE ~ NA_character_)
          , data_max_date_qc = case_when(data_max_date >= make_date(end_year,12,31) %m-% months(month_threshold) ~ "Ok"
            , data_max_date < make_date(end_year,12,31) %m-% months(month_threshold) ~ "Concern"
            , TRUE ~ NA_character_) 
        )
    
  } # end ~ File available | GAM # | dates
  
  # ----< Organize plot settings >----
  {
    # compute min | max date range ####
    ylim <- range(file_log$data_min_date
      , file_log$data_max_date
      , make_date(start_year,1,1)
      , make_date(end_year,12,31)
      , na.rm = TRUE)
    
    # identify files not found ####
    site0 <- file_log %>%
      filter(., !file_exists)
    
    # identify stations where file found but no eligible gam found ####
    site1 <- file_log %>%
      filter(., file_exists & is.na(gam_option_sel))
    
    
    # Determine Concerns about date range of data ####
    file_log_to_plot <- bind_rows(
      # data not collected early enough
      file_log %>%
        mutate(.
          , date = data_min_date
          , type = "Minimum"
          , warning = data_min_date_qc
        ) %>%
        select(., station, file_base, date, type, warning)
      
      # data not collected recently enough
      , file_log %>%
        mutate(.
          , date = data_max_date
          , type = "Maximum"
          , warning = data_max_date_qc
        ) %>%
        select(., station, file_base, date, type, warning)
    )
    
    file_log_to_plot$file_base <- factor(file_log_to_plot$file_base
      , levels = file_log$file_base)
    
  } # end ~ Organize plot settings
  
  # ----< plot data >----
  {
    p <- ggplot(file_log_to_plot, aes(x=file_base, y=date, shape = type, fill = warning)) +
      theme_bw() +
      ylab("date") +
      xlab("station + wq_parm + wq_layer") +
      scale_y_date(limits = ylim, expand = expansion(mult = c(0.2, 0.2))) +
      theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
      # add start_year and end_year for reference 
      geom_hline(aes(yintercept = c(make_date(start_year,1,1))), color="black") +
      geom_hline(aes(yintercept = c(make_date(end_year,12,31))), color="black", linetype=2) + 
      # add station dates
      geom_point(size = 3) +
      scale_shape_manual(values = c(25, 24)) + 
      scale_fill_manual(values=c("red", "white"),
        guide = guide_legend(override.aes = list(shape=21, size=4))) + 
      # Highlight stations with no data file found in magenta
      geom_vline(data= site0, aes(xintercept = file_base), color=alpha("magenta", 0.3), size=5) + 
      # Highlight stations with a data file but no eligble gam found in yellow
      geom_vline(data= site1, aes(xintercept = file_base), color=alpha("yellow", 0.3), size=5) + 
      # Fill in station/dates that do not meet date threshold
      labs(title = "GAM File Availability"
        , subtitle = paste0("Parameter: "
          , paste0(wq_parm, "", collapse = ", ")
          , "   Layer: "
          , paste0(wq_layer, "", collapse = ", ")
          )
        , caption = "Puple: File not found.\nYellow: Correct GAM not found.") +
      labs(shape = "Date", fill = "Condition")
  } # end ~ plot data
  
  .F("GAM file availability.")
  suppressWarnings(print(p))
  
  # ----< output report >----
  {
    file_log_to_table <- file_log %>%
      select(.
        , station, file_name, file_exists
        , gam_option_eval_avail, gam_option_sel, data_min_date
        , data_min_date_qc, data_max_date, data_max_date_qc) %>%
      rename(.
        , Station=station, File=file_name
        , Exists=file_exists
        , Available = gam_option_eval_avail
        , Selected=gam_option_sel
        , MinDate = data_min_date
        , MinQC = data_min_date_qc
        , MaxDate = data_max_date
        , MaxQC = data_max_date_qc)
    
    FT <- tblFT1(file_log_to_table
      , tbl_title = "GAM file availability"  
    )
    
  } # end ~ output report
  
  attr(file_log, "out.attrs") <- NULL
  
  return(file_log)
  
} # end ~ function: chkRDAfiles
