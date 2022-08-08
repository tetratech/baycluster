#' @title Perform basic hierarchical cluster analysis 
#' 
#' @description Perform basic hierarchical cluster analysis and return an
#'   object which describes the produced tree.
#'   
#' @details This function calculates the distance matrix between the rows of a
#'   data matrix. This distance matrix is then used to perform the hierarchical
#'   cluster analysis. 
#'   
#' The input \code{data} must be organized to have the first column as an
#' identifier for the rows. The remainder of the columns have data to be used in
#' the analysis. It is strongly advised and advantageous to have labeled rows
#' for plotting purposes. The output from \code{crossTabulate} is properly 
#' formatted as input.
#' 
#' The variables actively used from \code{c.spec} include the following:
#' 
#' \code{dist_method} distance matrix method to be used in coordination with
#'   stats::dist(). This must be one of "euclidean", "maximum", "manhattan",
#'   "canberra", "binary" or "minkowski".
#'   
#' \code{aggl_method} dissimilarities agglomeration method to be used in
#'   coordination with stats::hclust(). This should be one of "ward.D",
#'   "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA),
#'   "median" (= WPGMC) or "centroid" (= UPGMC)
#' 
#' \code{wq_parm} 
#' \code{grp_cnt}
#' \code{prof_var}
#' \code{id_var}
#' 
#' 
#' @param c.spec list that stores specifications for cluster analysis 
#' @param data table of data to cluster. Rows are the items to be clustered and
#'   columns represent the different variables. See Details for more information.
#' @param man_dend_grp_lbl Labels to manually override automatic group labeling 
#' 
#' @examples 
#' \dontrun{
#' #TBD
#' 
#' }
#' 
#' @return table of results
#' 
#' @seealso \code{\link{crossTabulate}} \code{\link[stats]{dist}} \code{\link[stats]{hclust}}
#' 
#' @importFrom stats dist hclust as.dendrogram
#' 
#' @export
#
clusterData <- function(c.spec, data, man_dend_grp_lbl=NA) { 
  
  # ----< preliminary set up >----
  
  # unpack needed variables ####
    pry(c.spec, v = c("wq_parm", "grp_cnt", "prof_var", "id_var", "aggl_method", "dist_method"))

  # re-label first column of data ####
  names(data)[1] <- "id_row"  

  # extract original order of rows of data ####
  id_row_vec <- data[ , 1]  
  
  # if user passes a vector of labels as an argument, then it takes precedent on number of clusters ####
  {
    if (any(is.na(man_dend_grp_lbl))) {
      is_auto <- TRUE
    } else {
      is_auto <- FALSE
      grp_cnt <- length(man_dend_grp_lbl)
      grp_df <- data.frame(prim_lbl = man_dend_grp_lbl
        , prim_col= apply(as.data.frame(scales::hue_pal()(grp_cnt)), 1, hexColor2Name) )
    }
  }
  
  # ----< run hierarchical cluster >----
  dend <- data[ , -1] %>%
    dist(., method = dist_method) %>%
    hclust(., method = aggl_method) %>%
    as.dendrogram()  
  
  # create summary of Leaves
  leaves <- deterLeaves(dend, id_row_vec, grp_cnt)
  
  # ----< manual labeling >----
  {
    if (!is_auto) {
      # build table of labels based on manual over-ride
      dend_lbl <- tibble(prim_grp = rev(1:grp_cnt), dend_grp_ord = 1:grp_cnt, grp_df)
      
      # amend manual labels to leaves
      leaves <- left_join(leaves, dend_lbl, by = "dend_grp_ord")
      
      # create summary of groups for labeling dendrogram
      dend_lbl <- unique(leaves[, c("cutree_grp", "cutree_leaves", "prim_grp"
        , "prim_lbl", "prim_col", "dend_grp_ord")]) %>%
        as.data.frame()
    }
  }
  
  # ----< auto labeling >----
  {
    if (is_auto) {
      
      # Create cgrp data set for autogrp_lab function. ####
      # From from deterLeaves > dendextend::cutree, cutree_grp is the group id
      # and dend_grp_ord_1 is the left-to-right order of leaves
      cgrp   <- leaves %>%
        select(., id_row, cutree_grp, dend_grp_ord) %>%
        rename(., grpid = cutree_grp, prim_grp = dend_grp_ord) 

      # generate automatic labels ####
      grp_labOrd <- autogrp_lab(c.spec, data, cgrp) 
      if (c.spec$TEST) grp_labOrd
      
      # build table of labels based on automatic labeling ####
      grp_lab_ord_df <- tibble(dend_grp_ord = 1:grp_cnt
        , prim_grp = grp_labOrd[[2]]
        , prim_lbl =  grp_labOrd[[1]]) %>%
        arrange(., desc(prim_grp)) %>%
        mutate(., prim_col = apply(as.data.frame(scales::hue_pal()(grp_cnt)), 1, hexColor2Name)) 

      # amend labels to leaves ####    
      leaves <- left_join(leaves, grp_lab_ord_df, by = "dend_grp_ord" ) %>%
        arrange(., desc(prim_grp), id_row_ord)
      
      # ----< rotate dendrogram >----
      {
        # identify desired order of leaves 
        rot_vec <- leaves$id_row  
        
        # rotate dendrogram  
        dend_rot <- dend %>%
          dendextend::rotate(order = rot_vec) 
        
        # determine updated order of leaves
        leaves_rot <- deterLeaves(dend_rot, id_row_vec, grp_cnt) %>%
          select(., id_row, dend_ord, dend_grp_ord) %>%
          rename(., dend_ord_r = dend_ord, dend_grp_ord_r = dend_grp_ord)
        
        # update initial leaves to include updated autolabel order 
        leaves <- left_join(leaves, leaves_rot, by = "id_row") %>%
          mutate(., dend_ord = dend_ord_r, dend_grp_ord = dend_grp_ord_r) %>%
          arrange(., dend_ord) %>%
          select(., -dend_ord_r, -dend_grp_ord_r) 
        
        # create summary of groups for labeling dendrogram
        dend_lbl <- unique(leaves[, c("prim_lbl", "prim_col", "cutree_leaves")]) %>%
          as.data.frame()
        
      } # end ~ rotate dendrogram
      
      # overwrite initial dendrogram with "rotated" dendrogram 
      dend <- dend_rot   
      
    } # end ~ if(auto)
    
  } # end ~ auto
  
  # plot
  
  plotDendrogram(
    dend = dend,
    grp_cnt = grp_cnt,
    dend_lbl = dend_lbl,
    dend_title = "Tree 1",
    dend_xlab = "Sites",
    dend_ylab = "Distance",
    dist_method = dist_method,
    aggl_method = aggl_method
  )

  tblFT1(leaves, "Cluster Groups")
  
  if (all(id_var == "station")) {
    
    stat_df <- c.spec$stat_df %>%
      select(., stat_vec, latitude, longitude) %>% 
      rename(id_row = stat_vec)
    
    leaves <- left_join(leaves, stat_df, by = "id_row")
  }
  
  return(leaves)
  
} # end ~ function: clusterData
  
  
  
  