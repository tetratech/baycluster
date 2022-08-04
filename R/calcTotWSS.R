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
#' The input \code{data} must be organized to have the first column as an
#' identifier for the rows. The remainder of the columns have data to be used in
#' the analysis. It is strongly advised and advantageous to have labeled rows
#' for plotting purposes. The output from \code{crossTabulate} is properly 
#' formatted for this purpose.
#' 
#' @param data table of data to cluster. Rows are the items to be clustered and
#'   columns represent the different variables. See Details for more information.
#'   
#' @param dist_method distance matrix method to be used in coordination with
#'   stats::dist(). This must be one of "euclidean", "maximum", "manhattan",
#'   "canberra", "binary" or "minkowski".
#'   
#' @param aggl_method dissimilarities agglomeration method to be used in
#'   coordination with stats::hclust(). This should be one of "ward.D",
#'   "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA),
#'   "median" (= WPGMC) or "centroid" (= UPGMC)
#' 
#' @examples 
#' \dontrun{
#' # create data set from well known iris data set
#' iris_df <- data.frame(id_row = paste(substr(iris[ ,5],1,3), rownames(iris), sep="."), iris[ , -5])
#' rownames(iris_df) <- iris_df[, 1]
#' 
#' baycluster::calcTotWSS(iris_df)
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
  
  # ----< run hierarchical cluster >----
  dend <- data[ , -1] %>%
    dist(., method = dist_method) %>%
    hclust(., method = aggl_method) %>%
    as.dendrogram()  
  
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



  

