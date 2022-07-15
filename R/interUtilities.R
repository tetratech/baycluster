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
#' grpCol <- tapply(grpCol, grpCol, hexColor2Name)
#' barplot(1:grpCnt, col=grpCol, names.arg=grpCol)
#' }
#' 
#' @return named color
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