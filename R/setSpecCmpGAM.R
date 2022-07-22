# ####
#' @title Complete c.spec with GAM-specific supporting variables
#' 
#' @description Complete c.spec with GAM-specific supporting variables
#'   
#' @details 
#' ... 
#' 
#' @param c.spec list of cluster analysis specifications
#'  
#' @examples 
#' # TBD
#' 
#' @return list
#' 
#' @keywords internal
#' 
#' @seealso \code{\link{readTextFile}}
#' 
#' 
#' @importFrom tibble tibble as_tibble 
#' @importFrom scales col_numeric hue_pal
#' @importFrom assertthat not_empty see_if
#' @importFrom dplyr %>% mutate select filter bind_rows case_when rename group_by
#' 
#' @export
#' 
setSpecCmpGAM <- function(c.spec) {
  
  # ----< extract needed variables >----
  varsNeeded <- c("statVec", "startYear", "endYear", "monthGrid", "dayGrid"
    , "grpCnt", "wqParm", "wqLayer", "idVar", "profVar", "monthAdj"
    , "analysisTitle", "analysisDate", "filename", "dataOut", "exCovClass")
  extract(c.spec, varsNeeded)
  
  # ----< Plot Variable >----
  pltVar <- paste(wqParm,"pred",sep=".") 
  
  # ----< Cluster group and colors >----
  grpDF  <- tibble(lab = paste("Group",1:grpCnt)) %>%
    mutate(., grpCol = scales::hue_pal()(grpCnt)) %>%
    mutate(., grpCol = apply(as.data.frame(grpCol), 1, hexColor2Name)) 

  # ----< Cluster group and colors >----
  exCovColFct <- scales::col_numeric(
    palette = c("red","lightblue","blue")
    , na.color = NA
    , domain = c(1,exCovClass))
  exCovCol <- exCovColFct(1:exCovClass)
  exCovCol <- tapply(exCovCol, exCovCol, hexColor2Name)
  attr(exCovCol, "dimnames") <- NULL
  exCovDF <- tibble(lab = paste("Class",1:exCovClass)
    , col = exCovCol)
  
  # ----< ID variable label >----
  if (length(idVar)==2) {
    idVarLab <- vec.strg(idVar,sep='&')
  } else {
      idVarLab <- paste(idVar)
  }
  
  # ----< output file names >----
  if (is.null(filename)) {
    filename <- paste("Cluster",analysisTitle,wqLayer,wqParm,"of"
      , idVarLab,"by",profVar,startYear,endYear,sep="_")
  }
  
  if (is.null(dataOut)) {
    dataOut <- paste("ClusterGroup",analysisTitle,wqLayer,wqParm,"of"
      , idVarLab,"by",profVar,startYear,endYear,sep="_")
  }
  
  # ----< build base prediction data set >----
  
  basePred <- createBasePred(
    startYear = startYear,
    endYear = endYear,
    monthGrid = monthGrid,
    dayGrid = dayGrid,
    monthAdj = monthAdj
  ) %>%
    structure( out.attrs = NULL
      , dateCreated = NULL
      , startYear = NULL
      , endYear = NULL
      , monthGrid = NULL
      , dayGrid = NULL
      , monthAdj = NULL
      , firstDay = NULL
      , lastDay = NULL
      , month.order = NULL
    )
  
  
  # ----< append variables to list for return >----
  vars2append <- c("pltVar", "idVarLab", "filename", "dataOut"
    , "grpDF", "exCovDF", "basePred")

  for (var in vars2append) {
    c.spec[[var]] <- eval(parse(text=var))
  }  
  
  return(c.spec)
  
} # end ~ function: setSpecCmpGAM





