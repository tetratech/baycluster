# ####
#' @title compute mean group profiles and assign group labels
#' 
#' @description compute mean group profiles and assign group labels
#'   
#' @param allMat table of data to cluster. Rows are the items to be clustered and
#'   columns represent the different variables. 
#' @param grp1 cluster results table   
#' @param wq_parm wq_parm
#' @param prof_var prof_var
#' @param id_var id_var
#' @param data_center data_center
#' @param ex_cov_quan ex_cov_quan
#' 
#' @keywords internal
#' 
#' @seealso \code{\link{calcQuanClass}}
#' 
#' @return table of results
#' 
#' @export
#'
autogrp_lab <- function(allMat, grp1, wq_parm, prof_var, id_var, data_center, ex_cov_quan=NA) {
  # compute mean group profiles and assign group labels

  #  grp1 <- cgrp;  allMat <- data

  grpVar <- "prim_grp"
  clus_var <- names(allMat[,-1])
  
  TEST_jh <- FALSE
  
  allMat.grp <- merge(allMat, grp1, by = "id_row")
  if (TEST_jh) print("autolabeler >  autogrp_lab 1")
  
  # compute group means for clus_var
  # JBH: ~ row mean by "prim_grp"
  allMat.mn <- aggregate(allMat.grp[,clus_var],list(grp=allMat.grp[,grpVar]),mean,na.rm=TRUE)
  if (TEST_jh) print("autolabeler >  autogrp_lab 2")
  
  # BRANCH 1: call different group labeling functions based on type of cluster ####
  if(id_var[1]=="station" & prof_var== "year" & is.na(c.spec$data_center)) {
    if (TEST_jh) print("autolabeler >  BRANCH 1")
    grp_lab <- rnkRowMnLab(allMat.mn, wq_parm, clus_var)
    # grp_lab <- rnkRowMnLab(c.spec,allMat.mn, wq_parm, clus_var)
    if (TEST_jh) print("autolabeler >  BRANCH 1 ~ end")
  }
  
  # BRANCH 2: #### 
  if (id_var[1]=="station" & prof_var== "year" & !is.na(c.spec$data_center))  {
    if (TEST_jh) print("autolabeler >  BRANCH 2")
    grp_labOrd <- rnkTrndLab(allMat.mn, wq_parm, clus_var)
    if (any(!is.na(ex_cov_quan))) {
      if (TEST_jh) print("autolabeler >  call rnkFlowCorLab")
      flowLab <- rnkFlowCorLab(allMat.mn, wq_parm, clus_var, ex_cov_quan)
      grp_lab <- list(paste(grp_labOrd[[1]], flowLab[[1]]),grp_labOrd[[2]])
    }
    if (TEST_jh) print("autolabeler >  BRANCH 2 ~ end")
  }
  
  # BRANCH 3: ####
  if(prof_var == "month" & is.na(c.spec$data_center)) {
    if (TEST_jh) print("autolabeler >  BRANCH 3") 
    grp_lab <- rnkRowMnLab(allMat.mn, wq_parm, clus_var)
    if (TEST_jh) print("autolabeler >  BRANCH 3 ~ end")
  }
  
  # BRANCH 4: ####
  if(prof_var == "month" & !is.na(c.spec$data_center)) {
    if (TEST_jh) print("autolabeler >  BRANCH 4 ~ end")
    grp_lab <- rnkSeasLab(allMat.mn, wq_parm, clus_var)
    if (TEST_jh) print("autolabeler >  BRANCH 4 ~ end")
  }
  
  # BRANCH 5: ####
  if(id_var[1]=="year" & prof_var == "station" & is.na(c.spec$data_center)) {
    if (TEST_jh) print("autolabeler >  BRANCH 5")
    grp_lab1 <- rnkRowMnLab(allMat.mn, wq_parm, clus_var)
    grp_lab2 <- yeargrp_lab(allMat.grp, wq_parm, clus_var)
    grp_lab <- paste(grp_lab1[[1]], grp_lab2[[1]])
    grpOrd <- grp_lab1[[2]]
    grp_lab <- list(grp_lab, grpOrd)
    if (TEST_jh) print("autolabeler >  BRANCH 5 ~ end")
  }
  
  # BRANCH ELSE: ####
  # if fall through above without changing grp_lab, make grp_lab default and record error
  if(grp_lab[1] == 'auto') {
    if (TEST_jh) print("autolabeler >  BRANCH ELSE ~ end")
    grp_lab <- paste0('Grp', 1:grp_cnt)
    if (TEST_jh) print("autolabeler >  BRANCH ELSE ~ end")
  }
  
  
  
  autogrp_lab.return <- grp_lab
  
  
} # end ~ function: autogrp_lab 


# ####
#' @title compute group labels based on mean
#' 
#' @description compute group labels based on mean
#'   
#' @param allMat.mn table of profile means
#' @param wq_parm wq_parm
#' @param clus_var column names associated with prof_var
#' 
#' @keywords internal
#' 
#' @seealso \code{\link{calcQuanClass}}
#' 
#' @return table of results
#' 
#' @export
#' 
  rnkRowMnLab <- function(allMat.mn, wq_parm, clus_var)
{  # create labels by mean row rank
  # overall row mean (i.e,. one mean per cluster)
  grp.mn <- rowMeans(allMat.mn[,clus_var])
  
  grpOrd <- rank(grp.mn) # rank row means (low number ... lowest mean)
  
  grp_lab <- paste(toupper(wq_parm),grpOrd)  # concatenate parameter and rank
  
  # JBH: e.g, TN 1, TN 2, ...
  rnkRowMnLab.return <- list(grp_lab,grpOrd)  # return labels and ranks in list
}  # end ~ function: rnkRowMnLab

# ####
#' @title compute group labels based on comparison of beginning and ending 3-yr time period
#' 
#' @description compute group labels based on comparison of beginning and ending 3-yr time period
#'   
#' @param allMat.mn table of profile means
#' @param wq_parm wq_parm
#' @param clus_var column names associated with prof_var
#' 
#' @keywords internal
#' 
#' @seealso \code{\link{calcQuanClass}}
#' 
#' @return table of results
#' 
#' @export
#'
rnkTrndLab <- function(allMat.mn, wq_parm, clus_var)
{  # create labels by mean row trend
  #estimate trend (mean last 3 years) - (mean first 3 years)
  
  # number of columns of data
  nCol <- length(clus_var)
  
  grp.mn.beg <- rowMeans(allMat.mn[,clus_var[1:3]])
  grp.mn.end <- rowMeans(allMat.mn[,clus_var[(nCol-2):(nCol)]])
  
  # df of grp, mn.beg, mn.end
  rnkTrndMat <- data.frame(grp = allMat.mn[,'grp'], mn.beg = grp.mn.beg, mn.end = grp.mn.end)
  
  # add diff and rank
  rnkTrndMat$diff <- rnkTrndMat$mn.end - rnkTrndMat$mn.beg 
  rnkTrndMat$grpOrd <- rank(rnkTrndMat$diff) # set grpOrd from smallest to largest
  
  # rank within trend direction for labels
  rnkTrndMat[rnkTrndMat$diff >=0,'dir']  <- 'Inc'
  rnkTrndMat[rnkTrndMat$diff < 0,'dir']  <- 'Dec'
  
  # absolute value of diff
  rnkTrndMat$aDiff <- abs(rnkTrndMat$diff)
  
  # sort from smallest to largest absolute differ
  rnkTrndMat <- rnkTrndMat[order(rnkTrndMat$dir,abs(rnkTrndMat$diff)),]
  rnkTrndMat$rnk[1] <- 1
  for(i in 2:nrow(rnkTrndMat)) {
    if(rnkTrndMat$dir[i] == rnkTrndMat$dir[i-1]) {
      rnkTrndMat$rnk[i] <- rnkTrndMat$rnk[i-1]+1
    } else {
      rnkTrndMat$rnk[i] <- 1
    }
  }
  # put back in group order
  rnkTrndMat <- rnkTrndMat[order(rnkTrndMat$grp),]
  
  rnkTrndMat$grp_lab <- paste0(rnkTrndMat$dir,rnkTrndMat$rnk)
  rnkTrndLab.return <- list(rnkTrndMat$grp_lab, rnkTrndMat$grpOrd)
} # end ~ function: rnkTrndLab

# ####
#' @title compute group labels based on seasonality
#' 
#' @description compute group labels based on seasonality
#'   
#' @param allMat.mn table of profile means
#' @param wq_parm wq_parm
#' @param clus_var column names associated with prof_var
#' 
#' @keywords internal
#' 
#' @seealso \code{\link{calcQuanClass}}
#' 
#' @return table of results
#' 
#' @export
#'
rnkSeasLab <- function(allMat.mn, wq_parm, clus_var)  {  
  # create labels by seasonality
  
  grpRng <- data.frame(minVal = apply(allMat.mn[,clus_var],1,min))  
  grpRng$maxVal <- apply(allMat.mn[,clus_var],1,max)  
  grpRng$minMon <- apply(allMat.mn[,clus_var],1,which.min)  
  grpRng$diff <- grpRng$maxVal - grpRng$minVal
  grpRng$grpOrd <- rank(grpRng$diff)
  grpRng$maxMon <- apply(allMat.mn[,clus_var],1,which.max)  
  grpRng$minMon <- apply(allMat.mn[,clus_var],1,which.min)  
  grpRng$grp_lab <- paste0("S",grpRng$grpOrd,"_H",grpRng$maxMon,"L",grpRng$minMon)
  
  rnkSeasLab.return <- list(grpRng$grp_lab, grpRng$grpOrd)
} # end ~ function: rnkSeasLab

# ####
#' @title compute group labels based on correlation with flow
#' 
#' @description compute group labels based on correlation with flow
#'   
#' @param c.spec list that stores specifications for cluster analysis 
#' @param allMat.mn table of profile means
#' @param wq_parm wq_parm
#' @param clus_var column names associated with prof_var
#' @param flow_index ex_cov_quan
#' 
#' @keywords internal
#' 
#' @seealso \code{\link{calcQuanClass}}
#' 
#' @return table of results
#' 
#' @importFrom stats aggregate cor 
#' 
#' @export
#'
rnkFlowCorLab <- function(allMat.mn, wq_parm, clus_var, flow_index) {  
  # create labels by correlation with flow

  
  grpCor <- data.frame(grp=numeric(), flowCor=numeric())
  for(i in 1:nrow(allMat.mn)) {
    grpProf <- unlist(allMat.mn[i,clus_var])
    grpCor[i,"grp"] <- allMat.mn[i,'grp']
    flowCor <- cor(cbind(grpProf,flow_index["year_avg"]), method = "spearman")[1,2]
    grpCor[i,"flowCor"] <- flowCor
  } #end of grp loop
  grpCor[,'grpOrd'] <- rank(grpCor$flowCor)
  grpCor[grpCor$flowCor<0,'flowDir'] <- '-F'
  grpCor[grpCor$flowCor>0,'flowDir'] <- '+F'
  grpCor$aCorr <- abs(grpCor$flowCor)
  # rank aCorr within correlation direction
  grpCor <- grpCor[order(grpCor$flowDir,grpCor$aCorr),]
  grpCor$rnk[1] <- 1
  for(i in 2:nrow(grpCor))  {
    if(grpCor$flowDir[i] == grpCor$flowDir[i-1]) {
      grpCor$rnk[i] <- grpCor$rnk[i-1]+1
    } else {
      grpCor$rnk[i] <- 1
    }
  }
  # put back in group order
  grpCor <- grpCor[order(grpCor$grp),]
  grpCor$grp_lab <- paste0(grpCor$flowDir,grpCor$rnk)
  rnkFlowCorLab.return <- list(grpCor$grp_lab, grpCor$grpOrd)
} # end ~ function: rnkFlowCorLab


# ####
#' @title compute group labels based on years within group
#' 
#' @description compute group labels based on years within group
#'   
#' @param allMat.mn table of profile means
#' @param wq_parm wq_parm
#' @param clus_var column names associated with prof_var
#' 
#' @keywords internal
#' 
#' @seealso \code{\link{calcQuanClass}}
#' 
#' @return table of results
#' 
#' @export
#'
yeargrp_lab <- function(allMat.grp, wq_parm, clus_var) {  
  # create labels by identifying years within group

  allMat.grp$year <- as.numeric(allMat.grp$id_row)
  allMat.grp <- allMat.grp[order(allMat.grp$prim_grp,allMat.grp$year),] # order by groups and years
  
  # set grpOrd according the mean value of years within group
  mnYears <- aggregate(allMat.grp$year,list(prim_grp = allMat.grp$prim_grp),mean,na.rm = TRUE)
  grpOrd <- rank(mnYears$x)
  
  # create groups of seqential years within cluster group
  seqGrp <- 1
  allMat.grp[1,'seqGrp'] <- seqGrp 
  for(i in 2:nrow(allMat.grp)) {
    #print(paste(allMat.grp[i,'year'], allMat.grp[i-1,'year'],allMat.grp[i,'prim_grp'],allMat.grp[i-1,'prim_grp'],seqGrp))
    if((allMat.grp[i,'year'] - allMat.grp[i-1,'year']) > 1 | allMat.grp[i,'prim_grp'] != allMat.grp[i-1,'prim_grp'] ) {
      seqGrp <- seqGrp + 1
    }
    allMat.grp[i,"seqGrp"] <- seqGrp 
  } # end of i-loop
  #allMat.grp[,c('prim_grp','year',"seqGrp")]
  # find min and max year within seqGrp within prim_grp
  rngYr <- aggregate(allMat.grp[,'year'],list(prim_grp = allMat.grp[,'prim_grp'], seqGrp = allMat.grp[,'seqGrp']),max)  
  names(rngYr)[3] <- 'maxYr'
  rngYr$minYr <- aggregate(allMat.grp[,'year'],list(prim_grp = allMat.grp[,'prim_grp'], seqGrp = allMat.grp[,'seqGrp']),min)[,3]  
  #create labels by sequential group
  rngYr[(rngYr$maxYr==rngYr$minYr),'yrLab'] <-  paste0(rngYr[rngYr$maxYr==rngYr$minYr,'minYr'])
  rngYr[(rngYr$maxYr!=rngYr$minYr),'yrLab'] <-  paste(rngYr[rngYr$maxYr!=rngYr$minYr,'minYr'],rngYr[rngYr$maxYr!=rngYr$minYr,'maxYr'],sep='-')
  # paste labels together by prim_grp
  grp_lab <- aggregate(rngYr[,'yrLab'],list(prim_grp = rngYr[,'prim_grp']),paste,simplify=TRUE,collapse=",")  
  yeargrp_lab.return <-list(unlist(grp_lab$x),grpOrd)
} # end ~ function: yeargrp_lab