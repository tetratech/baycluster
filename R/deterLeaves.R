# ####
#' @title Summarize cluster analysis results into table
#' 
#' @description Summarize cluster analysis results into table
#' 
#' @details ...
#' 
#' @param dend dendrogram object from dendextend
#' @param id_row_vec id_row elements (typically representing the rows of data
#'   used in cluster analysis)
#' @param grp_cnt number of clusters
#' 
#' @examples 
#' \dontrun{
#' # TBD
#' 
#' }
#' 
#' @seealso \code{\link{calcQuanClass}}
#' 
#' @importFrom dplyr %>% mutate select filter bind_rows case_when rename group_by
#' @importFrom dplyr distinct relocate left_join arrange between pull summarise ungroup
#' @importFrom tibble tibble as_tibble 
#' 
#' @export
#' 
deterLeaves <- function(dend, id_row_vec, grp_cnt) {
  tmp_id_ord <- tibble(id_row = id_row_vec, id_row_ord = 1:length(id_row_vec),)
  
  tmp_dend_ord <- tibble(id_row = labels(dend), dend_ord = 1:dendextend::nleaves(dend))
  
  tmp_cutree <- tibble(id_row = names(dendextend::cutree(dend, k = grp_cnt))
    , cutree_grp = dendextend::cutree(dend, k = grp_cnt)) 
  
  tmp_summary <- count(tmp_cutree, cutree_grp, name = "cutree_leaves")
  
  leaves <- left_join(tmp_id_ord, tmp_dend_ord, by ="id_row") %>%
    left_join(., tmp_cutree, by = "id_row") %>%
    left_join(., tmp_summary, by = "cutree_grp") %>% 
    relocate(., dend_ord,  .after = last_col())  %>%
    arrange(., dend_ord)
  
  tmp_dend_grp_ord <- tibble(dend_grp_ord = 1:NROW(tmp_summary)
    , cutree_grp = unique(leaves$cutree_grp))
  
  leaves <- left_join(leaves, tmp_dend_grp_ord, by = "cutree_grp")
  
  return(leaves)
  
} # end ~ function: leaves