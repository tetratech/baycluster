# ####
#' @title Transform column of data
#' 
#' @description Transform column of data
#'   
#' @details This function transforms a column of data from a table. Allowable transformations are
#' \itemize{
#' \item "logtransform": natural log transformation
#' \item "exptransform": exp(x) transform
#' \item "logbase10": log base 10 
#' \item "antilog10": 10^ (anti-log for base 10 logarithms)
#' \item "standnorm": z-score (subtract mean and divide by standard deviation)
#' \item "percent": percent (express response as a percent of the mean)
#' }
#' 
#' @param data Data table to analyze. Must have one column: \code{value_col}
#'   which contain the values to transform
#' @param value_col Column name that contains values for transforming
#' @param transform_type type of transformation to perform. If \code{transform_type =
#'   "logtransform"}, then values are log transformed  
#' @param transform_col Column name to store transformed data. If left as NA, then
#'   value_col will be overwritten.
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
transformData <- function(data
  , value_col
  , transform_type = "logtransform"
  , transform_col = NA) {
  
  # ----< Error trap >----
  {
    # value_col: exist and numeric field; valid transform_type
    stopifnot(
      value_col %in% names(data)
      , is.numeric(pull(data[ , value_col]))
      , is.na(transform_type) || transform_type %in% c("logtransform", "exptransform", "logbase10"
        , "antilog10", "standnorm", "percent")
    )
    
    # neg value test
    numNegValues <- sum(data[[value_col]] < 0) 
    if (numNegValues > 0 && transform_type %in% c("logtransform", "logbase10", "percent")) {
      warning(paste(numNegValues, "values found in data --", transform_type, "not valid option.\n"
        , "Transformation not performed."))
      return(data) 
    }
    
  } # end ~ error trap
  
  # Do transformation
  data_mean <- mean(data[[value_col]] , na.rm = TRUE)
  data_sd   <- sd(data[[value_col]], na.rm = TRUE)  
  
  if (transform_type == "logtransform") {
    data[[transform_col]] <- log(data[[value_col]])
  } else if (transform_type == "exptransform") {
    data[[transform_col]] <- exp(data[[value_col]])
  } else if (transform_type == "logbase10") {
    data[[transform_col]] <- log10(data[[value_col]])
  } else if (transform_type == "antilog10") {
    data[[transform_col]] <- 10 ^ (data[[value_col]])
  } else if (transform_type == "standnorm") {
    data[[transform_col]] <- (data[[value_col]] - data_mean) / data_sd
  } else if (transform_type == "percent") {
    data[[transform_col]] <- 100 * (data[[value_col]] / data_mean)
  } else {
    stop("Transformation error")
  }
  
  return(data)
  
} # end ~ function: transformData

  