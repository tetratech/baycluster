# ####
#' @title Find named color close to hexCol
#' 
#' @description Find named color close to hexCol
#'   
#' @details ...
#' 
#' 
#' @param hexCol hex color formatted character, i.e., "#80FF00"
#' 
#' @examples 
#' \dontrun{
#' 
#' hexColor2Name("#80FF00")
#' 
#' grpCnt <- 5
#' 
#' grpCol <- rev(rainbow(grpCnt))
#' 
#' grpDF  <- tibble(lab = paste("Group",1:grpCnt)) %>%
#'   mutate(., grpCol = scales::hue_pal()(grpCnt)) %>%
#'   mutate(., grpCol = apply(as.data.frame(grpCol), 1, hexColor2Name)) 
#' with(grpDF, barplot(1:grpCnt, col=grpCol, names.arg=grpCol))
#' }
#' 
#' @return named color
#' @keywords internal
#' 
#' @seealso \code{\link{calcQuanClass}}
#' 
#' @importFrom grDevices col2rgb colours
#' 
#' @export
#' 
hexColor2Name <- function(hexCol) { 
  
  # finds color in colors() that closely matches a hex 
  #color in "#80FF00" format # hexCol <- grpCol[1]
  
  rgbCol <- data.frame(t(col2rgb(hexCol)))
  
  colorMap = data.frame(colorName = colours(),t(col2rgb(colours())))
  
  colorMap$dist2 <- (colorMap$red - rgbCol$red)^2 + (colorMap$green -
      rgbCol$green)^2 + (colorMap$blue - rgbCol$blue)^2
  
  bestMatch <- colorMap[colorMap$dist2 == min(colorMap$dist2),'colorName']
  
  hexColor2Name.return <- bestMatch[1]
  
}


# ####
#' @title Create a list of all arguments passed to function
#' 
#' @description Create a list of all arguments passed to function
#'   
#' @details Derived from https://stackoverflow.com/questions/66329835/using-r-how-to-get-all-parameters-passed-into-a-function-with-their-values
#' 
#' 
#' @examples 
#' \dontrun{
#' 
#' adebo.deepSearch = function(z, pi_0 = 0.3, families=list(), ... ) {
#'   args = grabFunctionParameters();
#'   names(args)
#'   return(args )
#' }
#' 
#' X <- adebo.deepSearch(z=4, a=345, families = list(a=34, b=545), myList = list(a=34, b=545))
#' }
#' 
#' @return list of arguments
#' 
#' @keywords internal
#' 
#' @seealso \code{\link{calcQuanClass}}
#' 
#' @export
#' 
grabFunctionParameters <- function() {
  pf <- parent.frame()    
  args_names <- ls(envir = pf, all.names = TRUE, sorted = FALSE)
  if("..." %in% args_names) {
    dots <- eval(quote(list(...)), envir = pf)
  }  else {
    dots = list()
  }
  args_names <- sapply(setdiff(args_names, "..."), as.name)
  if(length(args_names)) {
    not_dots <- lapply(args_names, eval, envir = pf) 
  } else {
    not_dots <- list()
  }
  out <- c(not_dots, dots)
  out[names(out) != ""]                                 
}   

# ####
#' @title Convert vector to a single character
#' 
#' @description Convert vector to a single character
#'   
#' @details ...
#' 
#' @param x vector to be strung together
#' @param sep separator between elements of vector
#' 
#' @examples 
#' \dontrun{
#' M <- 1:10
#' vec.strg(M)
#' 
#' }
#' 
#' @return character
#' 
#' @keywords internal
#' 
#' @seealso \code{\link{calcQuanClass}}
#' 
#' @export
#' 
# converts a vector to single string character
vec.strg <- function(x, sep=", ") {
  vec.strg <- paste0(x, "", collapse = sep)   
  return(vec.strg)
} # end of vec.strg


# ####
#' @title pry components from list or table to parent environment
#' 
#' @description pry components from list or table to parent environment
#'   
#' @details ...
#' 
#' @param listvar name of target list or table
#' @param v vector of variables to pry from list or table
#' 
#' @examples 
#' \dontrun{
#' # TBD
#' 
#' }
#' 
#' @keywords internal
#' 
#' @seealso \code{\link{calcQuanClass}}
#' 
#' @export
#' 
pry <- function(listvar, v=NA) {

  if (is.na(v)) {
    v <- names(v)
  }
    
  for(nam in v) {eval(parse(text=paste0(nam," <- listvar$",nam)))}  
  rm(nam, listvar, v)
  argList <- grabFunctionParameters()   # create list of function arguments  
  invisible( list2env(argList, parent.frame() ) )
  
} # end ~ function: pry



