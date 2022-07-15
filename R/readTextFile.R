# ####
#' @title Read text file into variable
#' 
#' @description This function facilitates reading in text files with standard 
#' and non-standard headers. The two non-standard file types are 1) USGS' WRTDS
#' reporting file for estimated concentrations and loads and 2) USGS
#' flow retrievals.
#'   
#' @details ...
#' 
#' 
#' @param fileName Name of input file as character array
#' @param filePath File path as character array. Setting \code{filePath = ".."}
#'   reads from the current working directory
#' @param fileType Indicates what type of file structure is expected. Set
#'   \code{fileType = "WRTDS"} or \code{fileType = "flow"} to use specialized
#'   settings built into the function.  \code{fileType = "standard"} is the default
#'   setting for typical comma or tab delimited text files with the header
#'   as the first line.
#' @param colNames Vector of columns names to use for column names in the
#'   returned data set. Set \code{colNames = NA} to use column headers from
#'   input file.
#' @param report Summary information about data after reading. Set
#'   \code{report = FALSE} to suppress information.
#' 
#' @examples 
#' # TBD
#' 
#' @return data table
#' 
#' @seealso \code{\link{calcQuanClass}}
#' 
#' @importFrom rlang .data
#' @importFrom lubridate ymd decimal_date yday year month is.Date
#' @importFrom lubridate %m+% %m-% make_date  floor_date ceiling_date
#' @importFrom dplyr %>% mutate select filter bind_rows case_when rename group_by
#' @importFrom dplyr distinct relocate left_join arrange between pull summarise ungroup
#' @importFrom tibble tibble as_tibble is_tibble
#' @importFrom readr read_lines read_delim
#' 
#' @export
#' 
readTextFile <- function(fileName
  , filePath = ".."
  , fileType = "standard"
  , colNames = NA
  , report = TRUE) {
  
  # ----< testing >----
  {
    if (FALSE) {
      fileName <- "NTN_2018_MonLoadTab_ver_2_0_WRTDS output.csv"
      filePath <- "C:/Users/jon.harcum/OneDrive - Tetra Tech, Inc/work/CBP/cbpTrends_hold/work21b_cluster functions/2022.07.06a_fromElgin_rap/data_files"
      fileType = "WRTDS"
      colNames <- NA
      report <- TRUE
    }
  } # end ~ testing  
  
  # ----< Initialize customized settings for different file types >----
  {
    sk <- tibble(type = c("standard", "WRTDS", "flow")
      , chk  = c(NA_character_, '"STAID",' , "USGS")
      , chk0 = c(0, 8, 4)
      , chk1 = c(1, 0, -1)
      , chk2 = c(0, -1, -3))
  } # end ~ Initialize customized settings

  # ----< Error trap >----
  {
    # correct type of file must be specified
    stopifnot(
      fileType %in% sk$type
    )
  } # end ~ error trap
  
  # ----< Initialize type, full file name, and skip variable >----
  {
    iType <- which(fileType == sk$type)
    fname <- file.path(filePath, fileName)  
    skip  <- 0
  } # end ~ Initialize type, full ...  

  # ----< read in raw data if non-traditional >----
  { 
    if (fileType != sk[1,1]) {
      tmp.fil <- read_lines(fname)
    }
  } # end ~ read in raw data
  
  # ----< determine first line of data file if non-traditional>----
  {
    if (fileType != sk[1,1]) {
      skip <- 0; 
      agent=''
      while (agent != sk[iType,"chk"]) {
        skip <- skip+1
        agent <- substr(tmp.fil[skip], 1, sk[iType,"chk0"])
      }
    }
  } # end ~ determine first line of data file
  
  # ----< read generic delimited file into tibble >----
  {
    data <- read_delim(file = fname
      , col_names = NA
      , skip      = pull(skip + sk[iType,"chk1"])
      , na        = c("", "NA")    
      , trim_ws   = TRUE
      , show_col_types = FALSE
    )
    
    # Set column names based on file or user supplied values ####    
    if (any(is.na(colNames))) {
      
      # when user-supplied column names are *NOT* provided
      data1 <- read_delim(file = fname
        , col_names = NA
        , skip    =  pull(skip + sk[iType,"chk2"])
        , na      = c("", "NA")   
        , trim_ws = TRUE
        , show_col_types = FALSE
        , n_max = 1
      )
      stopifnot(length(names(data)) == length(unlist(data1)))
      names(data) <- make.names(unlist(data1), unique = TRUE)
    } else {
      # when user-supplied column names are provided
      stopifnot(length(names(data)) == length(colNames))
      names(data) <- colNames
    } # end ~ Set column names
    
  } # end ~ read generic delimited file
  
  # ----< summary data report >----
  {
    
    if (report)  print("Report: Future development.")
    
  } # end ~ summary data report
  
  return(data)
  
} # end ~ function: readTextFile
