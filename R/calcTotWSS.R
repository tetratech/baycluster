# ####
#' @title Calculate total within sum of squares 
#' 
#' @description Calculate total within sum of squares to inform the number of
#'   clusters to use. Create and return "knee-of-the-curve" plot
#'   
#' @details The "knee-of-the-curve" method can be used to inform the number of
#'   clusters to use when performing cluster analyses. The input data should
#'   only have columns used in the cluster analysis.
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
#' 
#' @examples 
#' \dontrun{
#' calTotWSS(iris[ , -5]) 
#' }
#' 
#' @return "knee of the curve" plot
#' 
#' @seealso \code{\link{calcQuanClass}}
#' 
#' @importFrom tibble tibble  
#' @importFrom stats dist hclust cutree
#' @importFrom ggplot2 ggplot geom_line geom_point theme_bw theme
#' @importFrom ggplot2 element_text element_line scale_y_continuous
#' @importFrom ggplot2 scale_x_continuous labs
#' 
#' @export
#'
calcTotWSS <- function(data, dist_method = "euclidean", aggl_method="ward.D") {
  
  # calculate distance matrix and agglomeration clusters
  
  data_hclust <- clusterData(data
    , dist_method = dist_method
    , aggl_method = aggl_method
    , dendo_title = ""
    , output_plot = FALSE)  
  
  # calculate number of clusters to evaluate
  size <- dim(data)
  k_num <- min(30,size[1])
  
  # calculate total sum of squares (i.e., same as when k=1)
  tss  <- sum(colSums(scale(data, scale=FALSE)^2))
  
  # set up table to receive within total sum of square errors
  tot_wss <- tibble(k = 1:k_num, wss = NA_real_)
  
  # calculate total sum of square errors
  for (k1 in tot_wss$k) {
    data_k1     <- cutree(data_hclust, k=k1)
    groups      <- split(data, data_k1)
    wss         <- sapply(groups, function(x) sum(colSums(scale(x, scale=FALSE)^2)))
    tot_wss$wss[k1] <- sum(wss)
  }
  
  # create plot of results
  p <- ggplot(tot_wss, mapping = aes(k, wss)) +
    geom_line(color  = "gray65", linetype = "solid", size=1) +
    geom_point(color = "black", size = 3) +
    theme_bw() +
    theme(axis.title = element_text(size=12)
      , axis.text  = element_text(size=11)
      , panel.grid = element_line(color = "gray85")) +
    scale_y_continuous("Total Within Sum of Square") +
    scale_x_continuous("Number of Clusters", breaks = seq(0,k_num,2)) +
    labs(caption = paste0("Total sum of squares: ", signif(tss,6)))
  
  # return
  return (p_tot_wss = p)
}



  

