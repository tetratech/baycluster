# ####
#' @title Summarize cluster analysis results into table
#' 
#' @description Summarize cluster analysis results into table
#' 
#' @details ...
#' 
#' @param dend dendrogram object from dendextend
#' @param id_row_vec id_row_vec
#' @param grp_cnt grp_cnt
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
}

# ####
#' @title Plot dendrogram
#' 
#' @description Plot dendrogram
#' 
#' @details ...
#' 
#' @param dend dend
#' @param grp_cnt grp_cnt 
#' @param dend_lbl dend_lbl 
#' @param dend_title dend_title 
#' @param dend_xlab dend_xlab 
#' @param dend_ylab dend_ylab 
#' @param dist_method dist_method 
#' @param aggl_method aggl_method 
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
plotDendrogram <- function(dend, grp_cnt = NA, dend_lbl = NA, dend_title = NA
  , dend_xlab = NA , dend_ylab = "Distance"
  , dist_method = NA, aggl_method = NA) {
  
  # set up labels
  {
    if (any(is.na(dist_method), is.na(aggl_method))) {
      dend_sub <- ""
    } else {
      dend_sub <- paste("dist.:", dist_method, "|", "aggl.:",aggl_method)
    }
    
    if (is.na(dend_title)) dend_title <- ""
    if (is.na(dend_xlab))  dend_xlab  <- ""
  }
  
  # create  dendrogram to plot
  
  # reduce cex based on increasing number of characters in group labels
  # g (max cex, min cex,  lower cutoff for max cex, upper cutoff for min cex)
  g1 <- c(1, 0.7, 20, 50)
  m1 <- (g1[2]-g1[1])/(g1[4]-g1[3])
  n1 <- max(nchar(dend_lbl$prim_lbl))
  mtext_cex <- as.numeric(n1 >= c(0, g1[3:4])) %*% c(g1[1], m1*(n1-g1[3]), -m1*(n1-g1[4]))
  
  # reduce label size as number of leaves gets bigger
  g2 <- c(1, 0.1, 40, 200)
  m2 <- (g2[2]-g2[1])/(g2[4]-g2[3])
  n2 <- sum(dend_lbl$cutree_leaves)
  label_size <- as.numeric(n2 >= c(0, g2[3:4])) %*% c(g2[1], m2*(n2-g2[3]), -m2*(n2-g2[4]))
  
  # set larger bottom margin
  par(mar = c(8.1, 4.1, 4.1, 2.1))  
  
  dend %>% 
    dendextend::set("labels_cex", label_size) %>%
    plot(main = dend_title)
  
  if (!is.na(grp_cnt)) {
    # add group boxes
    dend %>% dendextend::rect.dendrogram(k=grp_cnt
      , lwd = 2
      , border = dend_lbl$prim_col 
    ) 
    
    # helper variable for plotting group labels
    pos <- c(0,cumsum(dend_lbl$cutree_leaves))
    
    # plot group labels
    for (k1 in  1:(length(pos)-1)) {
      mtext(dend_lbl[k1,"prim_lbl"], side = 1, font = 2, line = 3.5
        , col = dend_lbl[k1,"prim_col"]
        , cex = mtext_cex
        , at = (pos[k1]+1 + pos[k1+1])/2 )
    }
  }
  # reset margins
  par(mar = c(5.1, 4.1, 4.1, 2.1))
  
  # add x- and y- axis labels
  title(xlab = dend_xlab
    , ylab = dend_ylab
    , sub  = dend_sub
  )
  
}
