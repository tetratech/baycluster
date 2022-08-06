# ####
#' @title calculate euclidean distances between groups
#' 
#' @description calculate euclidean distances between groups
#'   
#' @param c.spec list that stores specifications for cluster analysis 
#' @param data table of data to cluster. Rows are the items to be clustered and
#'   columns represent the different variables. 
#' @param grp1 cluster results table   
#' 
#' @examples 
#' \dontrun{
#' # TBD
#' 
#' }
#' 
#' @seealso \code{\link{calcQuanClass}}
#' 
#' @return table of results
#' 
#' @export
#'
grpCentrDist <- function(c.spec,data,grp1) { 
  
  clus_var <- names(data[, -1])
  names(data)[1]    <- "id_row"
  
  tbluk <- unique(grp1[,c("prim_grp","prim_lbl")])
  
  data.grp <- left_join(data, grp1, by = "id_row" )
  
  data.grp.mn <- aggregate(data.grp[,clus_var],list(prim_grp=data.grp[,"prim_grp"]),mean,na.rm = TRUE)
  
  grpCentrDist.return <- dist(data.grp.mn[,clus_var], method = "euclidean",upper=TRUE,diag=TRUE) # distance matrix
  
  grpCentrDist.return <- as.data.frame(as.matrix(grpCentrDist.return))
  
  grpCentrDist.return$prim_grp <- as.numeric(rownames(grpCentrDist.return) )
  
  grpCentrDist.return <- left_join(grpCentrDist.return, tbluk, by = "prim_grp") %>%
    relocate(., prim_lbl, prim_grp, .before = NULL)
  
  names(grpCentrDist.return) <-  c("prim_lbl","prim_grp", grpCentrDist.return[ , 1])
  
  return(grpCentrDist.return)
  
} # end ~ function: grpCentrDist