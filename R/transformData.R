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
#' }
#' 
#' Negative values in \code{value_col} are not permitted for "logtransform" and "logbase10" options.
#' 
#' @param data Data table to analyze. Must have one column: \code{value_col}
#'   which contains the values to transform
#' @param value_col Column name that contains values for transforming
#' @param transform_type type of transformation to perform. If \code{transform_type =
#'   "logtransform"}, then values are log transformed. If \code{transform_type =
#'   NA}, then an unchanged data table is returned.
#' @param transform_col Column name to store transformed data. If left as NA, then
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
#' @export
#' 
transformData <- function(data
  , value_col
  , transform_type = NA
  , transform_col = NA) {

  # ----< Do nothing if transform_type is NA >----  
  if (is.na(transform_type)) {
    return(data)
  }
  
  # ----< Error trap >----
  {
    # value_col: exist and numeric field; valid transform_type
    stopifnot(
      value_col %in% names(data)
      , is.numeric(pull(data[ , value_col]))
      , is.na(transform_type) || transform_type %in% c("logtransform", "exptransform", "logbase10"
        , "antilog10")
    )
    
    # no negative values for log* transforms 
    numNegValues <- sum(data[[value_col]] < 0) 
    if (numNegValues > 0 && transform_type %in% c("logtransform", "logbase10")) {
      warning(paste(numNegValues, "negative values found in data --", transform_type, "not valid option.\n"
        , "Transformation not performed."))
      return(data) 
    }
    
    # no big positive values for 10^ or exp transforms 
    numBigValues <- sum(data[[value_col]] >50) 
    if (numBigValues > 0 && transform_type %in% c("exptransform", "antilog10")) {
      warning(paste(numBigValues, "values > 50 found in data --", transform_type, "not valid option.\n"
        , "Transformation not performed."))
      return(data) 
    }
    
  } # end ~ error trap
  
  # Figure out where to store transformed values ####
  if (is.na(transform_col)) {
    transform_col = value_col
  }
  
  # Do transformation ####
  if (transform_type == "logtransform") {
    data[[transform_col]] <- log(data[[value_col]])
  } else if (transform_type == "exptransform") {
    data[[transform_col]] <- exp(data[[value_col]])
  } else if (transform_type == "logbase10") {
    data[[transform_col]] <- log10(data[[value_col]])
  } else if (transform_type == "antilog10") {
    data[[transform_col]] <- 10 ^ (data[[value_col]])
  } else {
    stop("Transformation error")
  }
  
  return(data)
  
} # end ~ function: transformData

  