# ####
#' @title Add supporting variables to c.spec
#' 
#' @description Add supporting variables to c.spec
#'   
#' @details 
#' ... 
#' 
#' @param c.spec list of cluster analysis specifications
#'  
#' @examples 
#' # TBD
#' 
#' @return n/a
#' 
#' @keywords internal
#' 
#' @seealso \code{\link{readTextFile}}
#' 
#' 
#' @importFrom tibble tibble as_tibble 
#' @importFrom scales col_numeric
#' 
#' @export
#' 
setSpecBuildout <- function(c.spec) {
  
  varsNeeded <- c("statVec", "startYear", "endYear", "monthGrid", "dayGrid"
    , "grpCnt", "wqParm", "wqLayer", "idVar", "profVar", "monthAdj"
    , "analysisTitle", "analysisDate", "filename", "dataOut", "exCovClass")
  for(nam in varsNeeded) {eval(parse(text=paste0(nam," <- c.spec$",nam)))}
  
  # Station setup: labels and order based on statVec ####
  statDF <- tibble(ord = 1:length(statVec)
    , statVec
    , lab = statVec)
  
  # Year setup: labels and order based on startYear and endYear ####
  yearVec = startYear:endYear
  yearDF <- tibble(ord = 1:length(yearVec)
    , yearVec
    , lab =  paste(yearVec))
  
  # Month setup: vector, labels and order based on monthGrid and monthAdj ####
  monthVec <- monthGrid
  if (exists("monthAdj") && !(any(is.na(monthAdj)) | is.null(monthAdj))) {
    if (monthAdj[1] > 0) {
      monthOrd <- c(monthVec[monthVec %in% monthAdj], monthVec[!(monthVec %in% monthAdj)])
    } else {
      monthOrd <- c(monthVec[!(monthVec %in% abs(monthAdj))], monthVec[(monthVec %in% abs(monthAdj))])
    }
  } else {
    monthOrd <- NA
  }
  monthDF <- tibble(ord = 1:length(monthOrd)
    , monthVec = monthOrd
    , lab = month.abb[monthOrd])
  
  # Plot Variable ####
  pltVar <- paste(wqParm,"pred",sep=".") 
  
  # Cluster group and colors ####
  grpCol <- rev(rainbow(grpCnt))
  grpCol <- tapply(grpCol, grpCol, hexColor2Name)
  attr(grpCol, "dimnames") <- NULL
  grpDF  <- tibble(lab = paste("Group",1:grpCnt)
    ,  col = grpCol)

  # Cluster group and colors ####
  exCovColFct <- scales::col_numeric(
    palette = c("red","lightblue","blue")
    , na.color = NA
    , domain = c(1,exCovClass))
  exCovCol <- exCovColFct(1:exCovClass)
  exCovCol <- tapply(exCovCol, exCovCol, hexColor2Name)
  attr(exCovCol, "dimnames") <- NULL
  exCovDF <- tibble(lab = paste("Class",1:exCovClass)
    , col = exCovCol)
  
  # ID variable label ####
  if (length(idVar)==2) {
    idVarLab <- vec.strg(idVar,sep='&')
  } else {
      idVarLab <- paste(idVar)
  }
  
  # output file names ####
  if (is.null(filename)) {
    filename <- paste("Cluster",analysisTitle,wqLayer,wqParm,"of"
      , idVarLab,"by",profVar,startYear,endYear,sep="_")
  }
  
  if (is.null(dataOut)) {
    dataOut <- paste("ClusterGroup",analysisTitle,wqLayer,wqParm,"of"
      , idVarLab,"by",profVar,startYear,endYear,sep="_")
  }
  
  # build base prediction data set ####
  
  basePred <- createBasePred(
    startYear = startYear,
    endYear = endYear,
    monthGrid = monthGrid,
    dayGrid = dayGrid,
    monthAdj = monthAdj
  )
  
  # append variables to list for return ####
  vars2append <- c("pltVar", "idVarLab", "filename", "dataOut"
    , "statDF", "yearDF", "monthDF",  "grpDF", "exCovDF", "basePred")

  for (var in vars2append) {
    c.spec[[var]] <- eval(parse(text=var))
  }  
  
  return(c.spec)
  
}





