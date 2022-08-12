# ####
#' @title Check RDA files to confirm availability of GAM results
#' 
#' @description Based on a station list, year range, water quality variable,
#'   layer, and GAM option, confirm that the `gamResults` files from
#'   baytrends are correctly positioned.
#'   
#' @details 
#' It is presumed that a series of baytrends operations
#' have been run and the files for individual "station|parameter|layer" results
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
#' The user can either specify the arguments for this function via the list
#' \code{c.spec} (see \code{\link{setSpec}}); or by specifying all of the below
#' variables individually.
#'            
#' \itemize{
#' \item \strong{stat_vec} -- vector of stations to analyze 
#' \item \strong{wq_parm} -- parameter abbreviation to evaluate (subfolder within \code{gam_folder})
#' \item \strong{wq_layer} -- layer abbreviation to evaluate
#' \item \strong{gam_numbr} -- GAM option to evaluate 
#' \item \strong{start_year} -- Begin year of analysis (scalar)
#' \item \strong{end_year} -- End year of analysis (scalar)
#' \item \strong{gam_folder} -- Folder location of GAM results from baytrends
#' \item \strong{month_grace_period} -- Threshold for flagging acceptable data range
#' }        
#'            
#' The values of \code{stat_vec}, \code{wq_parm}, and \code{wq_layer} are used to 
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
#' \code{month_grace_period} provides a grace period in the interpretation of a full
#' period of record.
#' 
#' @param c.spec cluster specification file
#' @param ... alternative variable passing
#'  
#' @examples 
#' \dontrun{
#' # TBD
#' 
#' }
#' 
#' @return data table with a summary of files and GAM result availability
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
#' @importFrom ggplot2 ggplot aes theme_bw ylab xlab scale_y_date theme
#' @importFrom ggplot2 geom_hline geom_point scale_shape_manual scale_fill_manual 
#' @importFrom ggplot2 geom_vline ggtitle labs expansion element_text guide_legend
#' @importFrom ggplot2 alpha
#' @importFrom knitr kable
#' 
#' @export
#' 
chkRDAfiles <- function(c.spec = NULL, ...) {  

  # ----< declare variables >----
  stat_vec <- wq_parm <- wq_layer <- gam_numbr <- start_year <- end_year <- gam_folder <- NULL
  month_grace_period <- station <- file_base <- file_name <- gamResult <- file_exists <- NULL
  data_min_date <- data_min_date_qc <- type <- data_max_date <- data_max_date_qc <- gam_option_eval_avail <- NULL
  
  # ----< load c.spec settings if provided >----
  vars = c("stat_vec", "wq_parm", "wq_layer", "gam_numbr", "start_year"
    , "end_year", "gam_folder", "month_grace_period")
  if (!is.null(c.spec)) {
    pry(c.spec, v = vars)
  } else {
    pry(list(...), v = vars)
  }

  # ----< error trap >----
  stopifnot(
    !is.null(stat_vec)
    , !is.null(wq_parm)
    , !is.null(wq_layer)
    , !is.null(gam_numbr)
    , !is.null(start_year)
    , !is.null(end_year)
    , !is.null(gam_folder)
    , !is.null(month_grace_period)
  )
    
  # ----< Create data table of files to look for >----
  {
    file_log <- 
      tibble(expand.grid(station = stat_vec
        , wq_parm = wq_parm
        , wq_layer = wq_layer)) %>%
      mutate(.
        , file_folder = gam_folder
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
          , data_min_date_qc = case_when(data_min_date <= make_date(start_year,1,1) %m+% months(month_grace_period) ~ "Ok"
          , data_min_date > make_date(start_year,1,1) %m+% months(month_grace_period) ~ "Concern"
            , TRUE ~ NA_character_)
          , data_max_date_qc = case_when(data_max_date >= make_date(end_year,12,31) %m-% months(month_grace_period) ~ "Ok"
            , data_max_date < make_date(end_year,12,31) %m-% months(month_grace_period) ~ "Concern"
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
