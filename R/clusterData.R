# #################################################################################
# ####
#' @title Perform basic hierarchical cluster analysis 
#' 
#' @description Perform basic hierarchical cluster analysis and return an
#'   object which describes the produced tree.
#'   
#' @details This function calculates the distance matrix between the rows of a
#'   data matrix. This distance matrix is then used to perform the hierarchical
#'   cluster analysis. Optionally, the function will output a basic plot.
#'   
#' The input data should only have columns used in the cluster analysis (it is
#' ok to have labeled rows.)
#' 
#' @param data table of data to cluster. Rows are the items to be clustered and
#'   columns represent the different variables. 
#' @param dist.Method distance matrix method to be used in coordination with
#'   stats::dist(). This must be one of "euclidean", "maximum", "manhattan",
#'   "canberra", "binary" or "minkowski".
#' @param aggl.Method dissimilarities agglomeration method to be used in
#'   coordination with stats::hclust(). This should be one of "ward.D",
#'   "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA),
#'   "median" (= WPGMC) or "centroid" (= UPGMC)
#' @param dendo.Title title of plot.
#' @param output.Plot TRUE to output plot.
#' 
#' @examples 
#' \dontrun{
#' p.dend <-clusterData(iris[, -5], output.Plot = FALSE)
#' p.dend <-clusterData(iris[, -5], dendo.Title = "Iris Data")
#' plot(p.dend, main="Iris Data", xlab = "Cluster Groups", sub=NA)
#' }
#' 
#' @return An object which describes the produced tree.
#' 
#' @seealso \code{\link{calcQuanClass}}
#' 
#' @importFrom stats dist hclust 
#' 
#' @export
#
clusterData <- function(data, dist.Method = "euclidean", aggl.Method = "ward.D"
  , dendo.Title = "", output.Plot = TRUE)  
{ 
  
  data.dist    <- dist(data, method = dist.Method)
  data.hclust  <- hclust(data.dist, method = aggl.Method)
  
  if (output.Plot) {
    plot(data.hclust
      , main = dendo.Title
      , xlab = "Cluster Groups"
      , sub = paste("dist.:", dist.Method, "|", "aggl.:",aggl.Method)
    )
  }
  
  # return
  return(data.hclust)  
  
} # end function: clusterData