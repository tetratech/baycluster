# #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#- ----
#' @title Return named color close to hexCol
#' 
#' @description Return named color close to hexCol
#'   
#' @param hexCol hex color formatted character, i.e., "#80FF00"
#' 
#' @examples 
#' 
#' k <- 5
#' 
#' grp_col <- rev(rainbow(k))
#' grp_col <- apply(as.data.frame(grp_col), 1, hexColor2Name)
#' barplot(1:k, col=grp_col, names.arg=grp_col)
#' 
#' # baycluster color call statement
#' k <- 5
#' grp_col <- getColors(k)
#' barplot(1:k, col=grp_col, names.arg=grp_col)
#' 
#' # baycluster color call statement for exogenous covariate
#' k <- 4
#' grp_col <- getColors2(k)
#' barplot(1:k, col=grp_col, names.arg=grp_col)
#' 
#' @return named color
#' 
#' @keywords internal
#' 
#' @seealso \code{\link{getColors}} \code{\link{getColors2}} \code{\link[scales]{hue_pal}}
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
  
} # end ~ function: hexColor2Name


# #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#- ----
#' @title Return vector of named colors
#' 
#' @description Return vector of named colors 
#'   
#' @param k number of colors
#' 
#' @examples 
#' 
#' k <- 5
#' 
#' grp_col <- rev(rainbow(k))
#' grp_col <- apply(as.data.frame(grp_col), 1, hexColor2Name)
#' barplot(1:k, col=grp_col, names.arg=grp_col)
#' 
#' # baycluster color call statement
#' k <- 5
#' grp_col <- getColors(k)
#' barplot(1:k, col=grp_col, names.arg=grp_col)
#' 
#' # baycluster color call statement for exogenous covariate
#' k <- 4
#' grp_col <- getColors2(k)
#' barplot(1:k, col=grp_col, names.arg=grp_col)
#' 
#' @return named vector
#' 
#' @keywords internal
#' 
#' @seealso \code{\link{hexColor2Name}} \code{\link{getColors2}} \code{\link[scales]{hue_pal}}
#' 
#' @importFrom scales hue_pal 
#' 
#' @export
#' 
getColors <- function(k) {
  
  x<-apply(as.data.frame(scales::hue_pal()(k)), 1, hexColor2Name)
  return(x)
  
} # end ~ function: getColors 



# #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#- ----
#' @title Return vector of named colors ... red to blue
#' 
#' @description Return vector of named colors  ... red to blue
#'   
#' @param k number of colors
#' 
#' @examples 
#' 
#' k <- 5
#' 
#' grp_col <- rev(rainbow(k))
#' grp_col <- apply(as.data.frame(grp_col), 1, hexColor2Name)
#' barplot(1:k, col=grp_col, names.arg=grp_col)
#' 
#' # baycluster color call statement
#' k <- 5
#' grp_col <- getColors(k)
#' barplot(1:k, col=grp_col, names.arg=grp_col)
#' 
#' # baycluster color call statement for exogenous covariate
#' k <- 4
#' grp_col <- getColors2(k)
#' barplot(1:k, col=grp_col, names.arg=grp_col)
#' 
#' @return named vector
#' 
#' @keywords internal
#' 
#' @seealso \code{\link{hexColor2Name}} \code{\link{getColors}} \code{\link[scales]{hue_pal}}
#' 
#' @importFrom scales hue_pal 
#' 
#' @export
#' 
getColors2 <- function(k) {
  
  ex_cov_col_fct <- scales::col_numeric(
    palette = c("red","lightblue","blue")
    , na.color = NA
    , domain = c(1,k)) 
  
  x<-apply(as.data.frame(ex_cov_col_fct(1:k)), 1, hexColor2Name)
  
  return(x)
  
} # end ~ function: getColors2



# #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#- ----
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
#' @seealso \code{\link{simpleCap}} \code{\link{firstCap}} \code{\link{firstCapAll}}
#' 
#' @export
#' 
# converts a vector to single string character
vec.strg <- function(x, sep=", ") {
  
  vec.strg <- paste0(x, "", collapse = sep)   
  return(vec.strg)
  
} # end ~ function: vec.strg


# #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#- ----
#' @title capitalize first letter of words separated by blanks in a character string
#' 
#' @description capitalize first letter of words separated by blanks in a character string
#'   
#' @details ...
#' 
#' @param x character string
#' 
#' @examples 
#' \dontrun{
#' # TBD
#' 
#' }
#' 
#' @return character
#' 
#' @keywords internal
#' 
#' @seealso \code{\link{vec.strg}} \code{\link{firstCap}} \code{\link{firstCapAll}}
#' 
#' @export
#' 
simpleCap <- function(x) { 
  
  s <- strsplit(x, " ")[[1]]
  s <- paste(toupper(substring(s, 1, 1)), substring(s, 2),
    sep = "", collapse = " ")
  
  return(s)
  
} # end ~ function: simpleCap

# #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#- ----
#' @title capitalize first letter of character string
#' 
#' @description capitalize first letter of character string
#'   
#' @details ...
#' 
#' @param cs character string
#' 
#' @examples 
#' \dontrun{
#' # TBD
#' 
#' }
#' 
#' @return character
#' 
#' @keywords internal
#' 
#' @seealso \code{\link{simpleCap}} \code{\link{vec.strg}} \code{\link{firstCapAll}}
#' 
#' @export
#' 
firstCap <- function(cs) {  
  
  cs <- tolower(cs)
  cs <- paste0(toupper(substr(cs,1,1)),substring(cs,2))
  return(cs)
  
} # end ~ function: firstCap


# #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#- ----
#' @title capitalize first letter of words separated by blanks in a character string
#' 
#' @description capitalize first letter of words separated by blanks in a
#'   character string and checks to make sure other letters are lower case
#'   
#' @details ...
#' 
#' @param cs character string
#' 
#' @examples 
#' \dontrun{
#' # TBD
#' 
#' }
#' 
#' @return character
#' 
#' @keywords internal
#' 
#' @seealso \code{\link{simpleCap}} \code{\link{firstCap}} \code{\link{vec.strg}}
#' 
#' @export
#' 
firstCapAll <- function(cs) {
  
  cs.return <- cs
  if (length(cs)==1) {
    cs.split <- unlist(strsplit(cs,' '))
    cs.split <- firstCap(cs.split)
    cs.return <- vec.strg(cs.split, " ")
  } else {
    if(length(cs)> 1) {
      cs.split <- strsplit(cs,' ')
      for (i in 1:length(cs)) { 
        cs.return[i] <- vec.strg(firstCap(cs.split[[i]]), " ")
      }
    }
  }
  
  return(cs.return)
  
} # end ~ function: firstCapAll

# #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#- ----
#' @title clear viewer pane
#' 
#' @description clear viewer pane
#' 
#' @export
#' 
cv <- function() {
  
  dir <- tempfile()
  dir.create(dir)
  TextFile <- file.path(dir, "blank.html")
  writeLines("", con = TextFile)
  rstudioapi::viewer(TextFile) 
  
} # end ~ function: cv



# #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#- ----
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
#' my_function = function(z, pi_0 = 0.3, families=list(), ... ) {
#'   args = grabFunctionArguments()
#'   names(args)
#'   return(args)
#' }
#' 
#' X <- my_function(z=4, a=345, families = list(a=34, b=545), myList = list(a=34, b=545))
#' }
#' 
#' @return list of arguments
#' 
#' @keywords internal
#' 
#' @export
#' 
grabFunctionArguments <- function() {
  
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
  
} # end ~ function: grabFunctionArguments



# #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#- ----
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
#' 
#' my_list <- list(a = 4, b = 5, c = "charlie")
#' pry(my_list, c("a", "c"))
#' 
#' d = "delta"
#' my_list <- stow(my_list, v = "d")
#' my_list
#' 
#' @keywords internal
#' 
#' @seealso \code{\link{stow}}
#' 
#' @export
#' 
pry <- function(listvar, v=NA) {
  
  if (any(is.na(v))) {
    v <- names(v)
  }
  
  for(nam in v) {eval(parse(text=paste0(nam," <- listvar$",nam)))}  
  rm(nam, listvar, v)
  argList <- grabFunctionArguments()   # create list of function arguments  
  invisible( list2env(argList, parent.frame() ) )
  
} # end ~ function: pry



# #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#- ----
#' @title append variables to a list 
#' 
#' @description append variables to a list 
#' 
#' @details ...
#' 
#' @param listvar name of target list 
#' @param v vector of variables to stow into list
#' 
#' @examples 
#' 
#' my_list <- list(a = 4, b = 5, c = "charlie")
#' pry(my_list, c("a", "c"))
#' 
#' d = "delta"
#' my_list <- stow(my_list, v = "d")
#' my_list
#' 
#' @keywords internal
#' 
#' @seealso \code{\link{pry}}
#' 
#' @export
#' 
stow <- function(listvar=list(), v=NA) {

  for (var in v) {
    listvar[[var]] <- eval(parse(text=var))
  }  
  
  return(listvar)
  
} # end ~ function: stow