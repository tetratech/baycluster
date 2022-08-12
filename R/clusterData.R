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
#' @param dend_title user supplied figure title
#' @param dend_xlab user supplied x axis label
#' @param dend_ylab user supplied y axis label
#' @param ... alternative variable passing
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
clusterData <- function(c.spec = NULL, data, man_dend_grp_lbl = NA
  , dend_title = NA, dend_xlab = NA, dend_ylab = NA, ...) { 
  
  # ----< load c.spec settings if provided >----
  vars = c("grp_cnt", "aggl_method", "dist_method"
    , "wq_parm", "data_center", "prof_var"
    , "wq_layer", "analysis_title", "stat_df", "ex_cov_quan")
  if (!is.null(c.spec)) {
    pry(c.spec, v = vars)
  } else {
    pry(list(...), v = vars)
  }
  
  # ----< error trap >----
  stopifnot(
    !is.null(data)
    , !is.null(grp_cnt)
    , !is.null(aggl_method)
    , !is.null(dist_method)
    , !is.null(man_dend_grp_lbl)
  )
  
  # ----< preliminary set up >----
  {
    # store id_var; re-name first column of data to id_row
    id_var         <- names(data)[1]
    names(data)[1] <- "id_row"  
    
    # create vector of variables used in cluster analysis  
    clus_var       <- names(data)[-1]
    
    # extract original order of rows of data 
    id_row_vec <- data[ , 1]  
  }
  
  # if user passes a vector of labels as an argument, then it takes precedent on number of clusters
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
  {
    dend <- data[ , -1] %>%
      dist(., method = dist_method) %>%
      hclust(., method = aggl_method) %>%
      as.dendrogram()  
    
    # create summary of Leaves
    leaves <- deterLeaves(dend, id_row_vec, grp_cnt)
  }
  
  # ----< manual labeling >----
  if (!is_auto) {
    
    # build table of labels based on manual over-ride
    grp_lbl <- tibble(prim_grp = rev(1:grp_cnt), dend_grp_ord = 1:grp_cnt, grp_df)
    
    # amend manual labels to leaves
    leaves <- left_join(leaves, grp_lbl, by = "dend_grp_ord")
    
    # create summary of groups for labeling dendrogram
    grp_lbl <- unique(leaves[, c("cutree_grp", "cutree_leaves", "prim_grp"
      , "prim_lbl", "prim_col", "dend_grp_ord")]) %>%
      as.data.frame()
    
  } # end ~ if(!is_auto)
  
  # ----< auto labeling >----
  if (is_auto) {
    
    # Create cgrp data set for autogrp_lab function. ####
    # From deterLeaves > dendextend::cutree, cutree_grp is the group id
    # and dend_grp_ord is the left-to-right order of leaves
    cgrp   <- leaves %>%
      select(., id_row, cutree_grp, dend_grp_ord) %>%
      rename(., grpid = cutree_grp, prim_grp = dend_grp_ord) 
    
    # error trap for additional requirements if running auto-labeling
    stopifnot(
      !is.null(wq_parm)  # used
      , !is.null(data_center) 
      , !is.null(prof_var) 
      , !is.null(id_var)
    )
    
    # generate automatic labels ####
    grp_labOrd <- autogrp_lab(data, cgrp, wq_parm, prof_var, id_var, data_center, ex_cov_quan) 
    
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
      grp_lbl <- unique(leaves[, c("prim_lbl", "prim_col", "cutree_leaves")]) %>%
        as.data.frame()
      
    } # end ~ rotate dendrogram
    
    # overwrite initial dendrogram with "rotated" dendrogram 
    dend <- dend_rot   
    
  } # end ~ if(is_auto)
  
  # plot dendrogram
  {
    usr_title <- ifelse(exists("dend_title")
      , dend_title
      , paste("Dendrogram: ", analysis_title, toupper(wq_parm), toupper(wq_layer)))
    
    usr_xlab <- ifelse(exists("dend_xlab")
      , dend_xlab
      , simpleCap(id_var))
    
    usr_ylab <- ifelse(exists("dend_ylab")
      , dend_ylab
      , "Distance")
    
    p <- plotDendrogram(
      dend = dend
      , grp_cnt     = grp_cnt
      , grp_lbl     = grp_lbl
      , dend_title  = usr_title
      , dend_xlab   = usr_xlab
      , dend_ylab   = usr_ylab
      , dist_method = dist_method
      , aggl_method = aggl_method
    )
  }
  
  # output results
  tblFT1(leaves, "Cluster Groups")
  
  # append lat/lng if available ####
  {
    # if clustering over stations, warn user to make stat_df file
    if (all(id_var == "station")) {
      if (is.null(stat_df)) {
        warning(simpleWarning("Variable 'stat_df' not found ... lat/lng cannot be appended to results"))
      }
    }
    
    if (all(id_var == "station") & !is.null(stat_df) ) {
      
      stat_df <- stat_df %>%
        select(., stat_vec, latitude, longitude) %>% 
        rename(id_row = stat_vec)
      
      leaves <- left_join(leaves, stat_df, by = "id_row")
    }
  }
  
  return(leaves)
  
} # end ~ function: clusterData



