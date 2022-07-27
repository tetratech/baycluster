# ####
#' @title Calculate total within sum of squares 
#' 
#' @description Calculate total within sum of squares to inform the number of
#'   clusters to use. Create and return "knee-of-the-curve" plot
#'   
#' @details The "knee-of-the-curve" method can be used to inform the number of
#' clusters to use when performing cluster analyses. The input data should only 
#' have columns used in the cluster analysis (it is ok to have labeled rows.)
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
#' 
#' @examples 
#' \dontrun{
#' calcTotWSS(iris[ , -5]) 
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
calcTotWSS <- function(data, dist.Method = "euclidean", aggl.Method="ward.D") {
  
  # calculate distance matrix and agglomeration clusters
  
  data.hclust <- clusterData(data
    , dist.Method = dist.Method
    , aggl.Method = aggl.Method
    , dendo.Title = ""
    , output.Plot = FALSE)  
  
  # calculate number of clusters to evaluate
  size <- dim(data)
  kNum <- min(30,size[1])
  
  # calculate total sum of squares (i.e., same as when k=1)
  TSS  <- sum(colSums(scale(data, scale=FALSE)^2))
  
  # set up table to receive within total sum of square errors
  TotWSS <- tibble(k = 1:kNum, wss = NA_real_)
  
  # calculate total sum of square errors
  for (k1 in TotWSS$k) {
    data.k1     <- cutree(data.hclust, k=k1)
    groups      <- split(data, data.k1)
    wss         <- sapply(groups, function(x) sum(colSums(scale(x, scale=FALSE)^2)))
    TotWSS$wss[k1] <- sum(wss)
  }
  
  # create plot of results
  p <- ggplot(TotWSS, mapping = aes(k, wss)) +
    geom_line(color  = "gray65", linetype = "solid", size=1) +
    geom_point(color = "black", size = 3) +
    theme_bw() +
    theme(axis.title = element_text(size=12)
      , axis.text  = element_text(size=11)
      , panel.grid = element_line(color = "gray85")) +
    scale_y_continuous("Total Within Sum of Square") +
    scale_x_continuous("Number of Clusters", breaks = seq(0,kNum,2)) +
    labs(caption = paste0("Total sum of squares: ", signif(TSS,6)))
  
  # return
  return (pTotWSS = p)
}



  

