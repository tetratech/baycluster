# ####
#' @title Center data
#' 
#' @description Center data
#'   
#' @details This function centers data from a table. Allowable transformations are
#' \itemize{
#' \item "meanadjust": subtract mean
#' \item "standnorm": z-score (subtract mean and divide by standard deviation)
#' \item "percent": percent (express response as a percent of the mean) 
#' }
#' 
#' Negative values in \code{value_col} are not permitted for "percent" option.
#' 
#' @param data Data table to analyze. Must have two columns: \code{value_col}
#'   which contains the values to transform and \code{id_col} which has the 
#'   id variable.
#' @param id_col Column name that contains id variable (e.g., "by" variable)
#' @param prof_col Column name that contains prof variable
#' @param value_col Column name that contains values for transforming
#' @param center_type type of transformation to perform. If \code{center_type =
#'   "meanadjust"}, then the values are adjusted by subtracting the mean. If
#'   \code{transform_type = NA}, then an unchanged data table is returned.
#' @param center_col Column name to store transformed data. If left as NA, then
#'   \code{value_col} will be overwritten.
#' 
#' @examples 
#' \dontrun{
#' # TBD
#' 
#' }
#' 
#' @keywords internal
#' 
#' @return data table with transformed column
#' 
#' @seealso \code{\link{calcQuanClass}}
#' 
#' @importFrom stats sd
#' 
#' @export
#' 
centerData <- function(data
  , id_col
  , prof_col
  , value_col
  , center_type = NA
  , center_col = NA) {

  # ----< Do nothing if transform_type is NA >----  
  if (is.na(center_type)) {
    return(data)
  }
  
  # ----< Error trap >----
  {
    # value_col: exist and numeric field; valid transform_type
    stopifnot(
      value_col %in% names(data)
      , prof_col %in% names(data)
      , id_col %in% names(data)
      , is.numeric(pull(data[ , value_col]))
      , is.na(center_type) || center_type %in% c("meanadjust", "standnorm", "percent")
    )
    
    # no negative values for log* transforms 
    numNegValues <- sum(data[[value_col]] < 0) 
    if (numNegValues > 0 && center_type %in% c("percent")) {
      warning(paste(numNegValues, "values found in data --", center_type, "not valid option.\n"
        , "Transformation not performed."))
      return(data) 
    }
    
  } # end ~ error trap
  
  # Figure out where to store transformed values ####
  if (is.na(center_col)) {
    center_col = value_col
  }
  
  # Do centering ####
  
  # calculate mean and sd by id_var
  data_mn <- data %>%                     
    group_by(., .data[[id_col]]) %>%                        
    summarise(., mn_id = mean(value)
      , sd_id = sd(value)) #, .groups = "keep")
  
  # center data
  data <- left_join(data, data_mn, by = {{id_col}} )
  
  if (center_type == "meanadjust") {
    data[[center_col]] <- data[[value_col]] - data[["mn_id"]]
  } else if (center_type == "standnorm") {
    data[[center_col]] <- (data[[value_col]] - data[["mn_id"]]) / data[["sd_id"]]
  } else if (center_type == "percent") {
    data[[center_col]] <- 100 * (data[[value_col]] / data[["mn_id"]])
  } else {
    stop("Transformation error")
  }

  # only return needed columns columns
  data <- data %>%
    select(., all_of({{prof_col}}), all_of({{id_col}}), all_of({{center_col}}) )  #  value) 
  
  return(data)
  
} # end ~ function: centerData