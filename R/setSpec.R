# ####
#' @title Organize cluster analysis specifications into a list
#' 
#' @description Organize user-provided cluster analysis specifications into a
#'   list. And where specifications are not provided then make some default
#'   selections.
#'   
#' @details TBD
#' 
#' 
#' @param projRoot root folder for analysis
#' @param gamRoot folder where output from baytrends is stored. There should be
#'   subfolders in this folder with names corresponding to the various parameters
#'   of interest. For example, if \code{wqParm} is set to \code{"chla"}, then there should 
#'   be a subdirectory under the \code{gamRoot} folder called \code{chla}. 
#' @param wqParm parameter to be analyzed
#' @param waterBody waterBody
#' @param slayer slayer
#' @param gamNumbr gamNumbr
#' @param startYear First year of data to be used in cluster analysis
#' @param endYear Last year of data to be used in cluster analysis
#' @param idVar Variable that identifies the rows of the cluster matrix, i.e. items that are clustered 
#' @param profVar Variable that becomes columns of the cluster matrix, i.e. attribute to cluster
#' @param rowAdj rowAdj
#' @param scale  scale
#' @param grpCnt grpCnt
#' @param grpCol grpCol
#' @param statVec statVec
#' @param statLab statLab
#' @param monVec monVec
#' @param monLab monLab
#' @param filename filename=NA
#' @param monOrd monOrd=NA,
#' @param yrOrd yrOrd=NA,
#' @param dataOut dataOut=NA,
#' @param subGrps subGrps=FALSE,
#' @param addFlow addFlow,
#' @param flowFile flowFile,
#' @param flowLab flowLab='Flow',
#' @param datSource datSource
#' @param datFile datFile,
#' @param lineOne lineOne,
#' @param grpLab grpLab=NA,
#' @param yearType yearType = 'calendar'
#' 
#' @examples 
#' # TBD
#' 
#' @return list with cluster specifications
#' 
#' @seealso \code{\link{calcQuanClass}}
#' 
#' @export
#' 
setSpec <- function(projRoot, gamRoot, wqParm, waterBody, slayer
  , gamNumbr, startYear, endYear, idVar, profVar, rowAdj, scale
  , grpCnt, grpCol, statVec, statLab, monVec, monLab
  , filename=NA, monOrd=NA, yrOrd=NA, dataOut=NA, subGrps=FALSE, addFlow
  , flowFile, flowLab='Flow', datSource, datFile
  , lineOne, grpLab=NA, yearType = 'calendar')
{
  #  set up specifications based on user supplied data, compile these in a list to pass to functions
  c.spec <- list()  # initialize list for cluster specifications.
  c.spec$projRoot <- projRoot
  c.spec$datSource <- datSource
  c.spec$datFile <- datFile
  c.spec$lineOne <- lineOne
  c.spec$gamRoot <- gamRoot
  c.spec$wqParm <- wqParm
  c.spec$waterBody <- waterBody
  c.spec$slayer <- slayer
  c.spec$slayer.strg <- vec.strg(slayer,sep='_')
  c.spec$gamNumbr <- gamNumbr
  c.spec$startYear <- startYear
  c.spec$endYear <- endYear
  c.spec$yearType <- yearType
  c.spec$idVar <- idVar
  c.spec$profVar <- profVar
  c.spec$rowAdj <- rowAdj
  c.spec$scale <- scale
  c.spec$grpCnt <- grpCnt
  c.spec$grpCol <- grpCol
  if(is.na(grpLab)){c.spec$grpLab <- paste("Group",1:grpCnt)}
  
  c.spec$statVec <- statVec
  c.spec$statLab <- statLab
  c.spec$monVec <- monVec
  c.spec$monLab <- monLab
  c.spec$yrVec  <- yrVec <-  startYear:endYear # determines order of plotting years
  c.spec$yrLab  <- yrLab <- paste(yrVec)
  c.spec$pltVar <- pltVar <- paste(wqParm,"pred",sep=".")
  
  if (length(idVar)==2) {
    idVarLab <- vec.strg(idVar,sep='&')
  } else {
    idVarLab <- paste(idVar)
  }
  
  c.spec$idVarLab <- idVarLab
  
  if (is.na(filename)) {
    filename <- paste("Cluster",waterBody,c.spec$slayer.strg,wqParm,"of",idVarLab,"by",profVar,startYear,endYear,sep="_")
  }
  c.spec$filename <- filename
  
  if (is.na(dataOut)) {
    dataOut <- paste("ClusterGroup",waterBody,slayer.strg,wqParm,"of",idVarLab,"by",profVar,startYear,endYear,sep="_")
  }
  
  c.spec$dataOut <- dataOut
  c.spec$subGrps <- subGrps
  c.spec$addFlow <- addFlow
  c.spec$flowFile <- flowFile
  c.spec$flowLab <- flowLab
  
  if (!is.na(flowFile)) {
    c.spec$flowIndex <- prepFlow(flowFile,startYear,endYear,yearType)
  } else {
      c.spec$flowIndex <- NA
  }
  
  ord <- list()  # create a list of order vectors for month, year, and station
  if (is.na(monOrd)) ord$month <- monVec
  if (is.na(yrOrd)) ord$year  <- (startYear:endYear)-startYear+1
  ord$station <- statVec
  c.spec$ord <- ord
  
  # creating these lists makes it easy to switch between clustering by years, months, or stations.
  idLev <- list()
  idLev$year <- yrVec
  idLev$month <- monVec
  idLev$station <- statVec
  c.spec$idLev <- idLev
  
  idLab <- list()  
  idLab$year <- yrLab
  idLab$month <- monLab
  idLab$station <- statLab
  c.spec$idLab <- idLab
  
  c.spec$idVarLab <- idVarLab <- vec.strg(idVar,sep='-')
  c.spec$idVarFac <- idVarFac <- paste0(idVar,"Fac")
  c.spec$profVarFac <- profVarFac <- paste0(profVar,"Fac")
  
  setSpec.return <- c.spec
  
} # end ~ function: setSpec