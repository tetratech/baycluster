# ####
#' @title Return named color close to hexCol
#' 
#' @description Return named color close to hexCol
#'   
#' @param hexCol hex color formatted character, i.e., "#80FF00"
#' 
#' @examples 
#' 
#' print(hexColor2Name("#80FF00"))
#' 
#' grp_cnt <- 5
#' 
#' grp_col <- rev(rainbow(grp_cnt)) 
#' grp_col <- apply(as.data.frame(grp_col), 1, hexColor2Name)
#' barplot(1:grp_cnt, col=grp_col, names.arg=grp_col)
#' 
#' # baycluster colors
#' grp_col = scales::hue_pal()(grp_cnt)
#' grp_col = apply(as.data.frame(grp_col), 1, hexColor2Name)
#' barplot(1:grp_cnt, col=grp_col, names.arg=grp_col)
#' 
#' # simpler baycluster color call statement
#' grp_cnt <- 4
#' grp_col <- getColors(grp_cnt)
#' barplot(1:grp_cnt, col=grp_col, names.arg=grp_col)
#' 
#' 
#' @return named color
#' 
#' @keywords internal
#' 
#' @seealso \code{\link{getColors}} \code{\link[scales]{hue_pal}}
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
#' @title Return vector of named colors
#' 
#' @description Return vector of named colors
#'   
#' @param k number of colors
#' 
#' @examples 
#' 
#' print(hexColor2Name("#80FF00"))
#' 
#' grp_cnt <- 5
#' 
#' grp_col <- rev(rainbow(grp_cnt)) 
#' grp_col <- apply(as.data.frame(grp_col), 1, hexColor2Name)
#' barplot(1:grp_cnt, col=grp_col, names.arg=grp_col)
#' 
#' # baycluster colors
#' grp_col = scales::hue_pal()(grp_cnt)
#' grp_col = apply(as.data.frame(grp_col), 1, hexColor2Name)
#' barplot(1:grp_cnt, col=grp_col, names.arg=grp_col)
#' 
#' # simpler baycluster color call statement
#' grp_cnt <- 4
#' grp_col <- getColors(grp_cnt)
#' barplot(1:grp_cnt, col=grp_col, names.arg=grp_col)
#' 
#' @return named vector
#' 
#' @keywords internal
#' 
#' @seealso \code{\link{hexColor2Name}} \code{\link[scales]{hue_pal}}
#' 
#' @importFrom scales hue_pal 
#' 
#' @export
#' 
getColors <- function(grp_cnt) {
  x<-apply(as.data.frame(scales::hue_pal()(grp_cnt)), 1, hexColor2Name)
  return(x)
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
#'   args = grabFunctionArguments();
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

  if (any(is.na(v))) {
    v <- names(v)
  }
    
  for(nam in v) {eval(parse(text=paste0(nam," <- listvar$",nam)))}  
  rm(nam, listvar, v)
  argList <- grabFunctionArguments()   # create list of function arguments  
  invisible( list2env(argList, parent.frame() ) )
  
} # end ~ function: pry



# ####
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
#' @seealso \code{\link{calcQuanClass}}
#' 
#' @export
#' 
simpleCap <- function(x) { 
  s <- strsplit(x, " ")[[1]]
  s <- paste(toupper(substring(s, 1, 1)), substring(s, 2),
    sep = "", collapse = " ")
  return(s)
}

# ####
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
#' @seealso \code{\link{calcQuanClass}}
#' 
#' @export
#' 
firstCap <- function(cs) {  
  cs <- tolower(cs)
  cs <- paste0(toupper(substr(cs,1,1)),substring(cs,2))
  return(cs)
}


# ####
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
#' @seealso \code{\link{calcQuanClass}}
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
}

# ####
#' @title clear viewer pane
#' 
#' @description clear viewer pane
#'   
#' @details ...
#' 
#' @examples 
#' \dontrun{
#' cv()
#' 
#' }
#' 
#' @seealso \code{\link{calcQuanClass}}
#' 
#' @export
#' 
cv <- function() {
  dir <- tempfile()
  dir.create(dir)
  TextFile <- file.path(dir, "blank.html")
  writeLines("", con = TextFile)
  rstudioapi::viewer(TextFile) 
}



# ####
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
#' \dontrun{
#' 
#' a = c(1, 3, 5)
#' b = c("abc", "def", "ghi", "jkl")
#' my_list <- stow(my_list, c("a", "b") )
#' 
#' }
#' 
#' @keywords internal
#' 
#' @seealso \code{\link{calcQuanClass}}
#' 
#' @export
#' 
stow <- function(listvar=list(), v=NA) {

  for (var in v) {
    listvar[[var]] <- eval(parse(text=var))
  }  
  
  return(listvar)
  
}