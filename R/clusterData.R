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
#' The input data should only have columns used in the cluster analysis. (It is
#' advantageous to have labeled rows for plotting purposes.)
#' 
#' @param data table of data to cluster. Rows are the items to be clustered and
#'   columns represent the different variables. 
#' @param dist_method distance matrix method to be used in coordination with
#'   stats::dist(). This must be one of "euclidean", "maximum", "manhattan",
#'   "canberra", "binary" or "minkowski".
#' @param aggl_method dissimilarities agglomeration method to be used in
#'   coordination with stats::hclust(). This should be one of "ward.D",
#'   "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA),
#'   "median" (= WPGMC) or "centroid" (= UPGMC)
#' @param dendo_title title of plot.
#' @param output_plot TRUE to output plot.
#' 
#' @examples 
#' \dontrun{
#' p.dend <-clusterData(iris[, -5], output_plot = FALSE)
#' p.dend <-clusterData(iris[, -5], dendo_title = "Iris Data")
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
clusterData <- function(data, dist_method = "euclidean", aggl_method = "ward.D"
  , dendo_title = "", output_plot = TRUE)  
{ 
  
  data_dist    <- dist(data, method = dist_method)
  data_hclust  <- hclust(data_dist, method = aggl_method)
  
  if (output_plot) {
    plot(data_hclust
      , main = dendo_title
      , xlab = "Cluster Groups"
      , sub = paste("dist.:", dist_method, "|", "aggl.:",aggl_method)
    )
  }
  
  # return
  return(data_hclust)  
  
} # end function: clusterData