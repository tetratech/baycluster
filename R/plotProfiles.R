#' @title Plot profiles - one plot
#' 
#' @description Plot profiles - one plot
#'   
#' @param c.spec list that stores specifications for cluster analysis 
#' @param data table of data to plot. Rows are the items to plot and
#'   columns represent the profile data.
#' @param grp1 cluster results table  
#' @param legendPos legend position
#' 
#' 
#' @examples 
#' \dontrun{
#' #TBD
#' 
#' }
#' 
#' @return table of plotting data
#' 
#' @seealso \code{\link{calcQuanClass}}
#' 
#' @importFrom graphics axis layout legend lines  mtext par points title 
#' @importFrom stats aggregate cor 
#' 
#' 
#' @export 
#'
plotClusProfMat <- function(c.spec, data, grp1, legendPos="topleft") 
{
  # plot profiles in data by group superimposed on one plot
  #  mnOnly=FALSE; grp.lab <- c.spec$grp_lab; grpVar <- "prim_grp"; legendPos <- "topright"  
  # unpack c.spec
  c.spec.names <- names(c.spec)
  for(nam in c.spec.names){ eval(parse(text=paste0(nam," <- c.spec$",nam)))}
  
  #### jh2ep interfacing
  {
    mnOnly <- FALSE
    grpVar<-"prim_grp"
    names(data)[1]    <- "id_row"
    yVarLab             <- c.spec$axis_label
    c.spec$clus_var     <- clus_var <- names(data[,-1])
    flow_index   <- c.spec$ex_cov_quan
    grp_cnt <- c.spec$grp_cnt
    id_lev <- c.spec$id_lev
    id_lab <- c.spec$id_lab
    prof_var <- c.spec$prof_var
    analysis_title <- c.spec$analysis_title
    wq_layer <- c.spec$wq_layer
    wq_parm <- c.spec$wq_parm
  }
  
  # create group summary 
  dend_lbl <- unique(grp1[, c("cutree_grp", "cutree_leaves", "prim_grp"
    , "prim_lbl", "prim_col", "dend_grp_ord")]) %>%
    arrange(., desc(prim_grp)) %>%
    as.data.frame()
  #### jh2ep interfacing ~ end
  
  xtics <- id_lev[[prof_var]]
  xtics <- 1:length(xtics)
  
  xrng <- range(xtics)
  yrng <- range(data[,clus_var])
  yrng[2] <- yrng[2] + 0.1*(yrng[2]-yrng[1])
  data.grp <- merge(data, grp1, by = "id_row") 
  # plot the group profiles by color
  plot(0,0,type="n",main=paste("Group Plot by", prof_var,"for",analysis_title,wq_layer,wq_parm),
    xaxt='n',xlab=prof_var,ylab=yVarLab,xlim=xrng,ylim=yrng)  #
  xlabs <- id_lab[[prof_var]]
  axis(side = 1,at=xtics,labels=xlabs,las=2)
  
  
  legend(legendPos, dend_lbl$prim_lbl,lty=rep(1,grp_cnt),col=dend_lbl$prim_col)
  # JBH legend(legendPos,grp.lab,lty=rep(1,grp_cnt),col=grp_col)
  if(!mnOnly) {  
    for(i in 1:nrow(data.grp)) {
      lines(xtics,as.vector(data.grp[i,clus_var]),col=data.grp$prim_col[i] ,lty=3)
      # JBH lines(xtics,as.vector(data.grp[i,clus_var]),col=grp_col[data.grp[i,'prim_grp']],lty=3)
    }
  }
  for(i in 1:grp_cnt)  {
    oneGrp <- data.grp[data.grp[,grpVar]==i,clus_var]
    oneGrpMn <- colMeans(oneGrp)
    line_col <- dend_lbl[dend_lbl$prim_grp == i, "prim_col"]
    lines(xtics,oneGrpMn,col=line_col,lty=1,lwd=2)
  }
  # add flow legend to x-axis if ex_cov_incl requested
  if (c.spec$TEST) print("  pltclusProf 1")
  if(ex_cov_incl) {      # if(ex_cov_incl) {
    if(prof_var == 'year') {
      # flow_col <- c("red","orange","lightblue","blue")   #JBH
      flow_col <- c.spec$ex_cov_df$ex_cov_col
      if (c.spec$TEST) print("  pltclusProf 2")
      if(is.na(flow_index[1,1])) {
        warning("pltClustProfMat error:  flow_index object not available")
      } else {
        for( i in 1:nrow(flow_index)) {
          # points(i,yrng[1],col = flow_col[flow_index[i,"flowCat"]],pch=17,cex=1.5)  
          colIndex <- unlist(flow_index[i,"year_cat"])
          points(i,yrng[1],col = flow_col[colIndex],pch=17,cex=1.5)   #JBH
        }  # end of i loop
      }
    } # end of ex_cov_incl true condition
  } else {   
    errorTrap <- warning("pltClustProfMat error: ex_cov_incl requested but prof_var not equal 'year'")
  }
  pltClusProfiles.return <- data.grp
} # end ~ function: plotClusProfMat


#' @title Plot profiles - panels
#' 
#' @description Plot profiles - panels
#'   
#' @param c.spec list that stores specifications for cluster analysis 
#' @param data table of data to plot. Rows are the items to plot and
#'   columns represent the profile data.
#' @param grp1 cluster results table  
#' @param legendPos legend position
#' @param num_col number of columns
#' 
#' @examples 
#' \dontrun{
#' #TBD
#' 
#' }
#' 
#' @return table of plotting data
#' 
#' @seealso \code{\link{calcQuanClass}}
#' 
#' @importFrom graphics axis layout legend lines  mtext par points title 
#' @importFrom stats aggregate cor 
#' 
#' @export 
#'
plotClusProfMatPan <- function(c.spec,data,grp1, legendPos="topleft", num_col = 3) {
  # plot profiles in data by group in panels
  #  grp_lab <- paste("Group",1:grp_cnt); grpVar<-"prim_grp"; num_col <- 3; legendPos="topleft"; data <- data
  # unpack c.spec
  c.spec.names <- names(c.spec)
  for(nam in c.spec.names){ eval(parse(text=paste0(nam," <- c.spec$",nam)))}
  
  #### jh2ep interfacing
  {
    grpVar<-"prim_grp"
    names(data)[1]    <- "id_row"
    yVarLab             <- c.spec$axis_label
    c.spec$clus_var     <- clus_var <- names(data[,-1])
    c.spec$flow_index   <- c.spec$ex_cov_quan
    grp_cnt <- c.spec$grp_cnt
    id_lev <- c.spec$id_lev
    id_lab <- c.spec$id_lab
    prof_var <- c.spec$prof_var
  }
  
  
  # create group summary 
  dend_lbl <- unique(grp1[, c("cutree_grp", "cutree_leaves", "prim_grp"
    , "prim_lbl", "prim_col", "dend_grp_ord")]) %>%
    arrange(., desc(prim_grp)) %>%
    as.data.frame()
  
  #### jh2ep interfacing ~ end 
  
  
  if(is.na(num_col)) num_col <- ceiling((grp_cnt+1)/4)
  par.orig <-  par(no.readonly = TRUE) # the whole list of settable par's.
  numCell <- ceiling((grp_cnt+1)/num_col) * num_col
  layout(matrix(1:numCell, ncol = num_col, byrow=TRUE))# sets up for multiple plots per page
  
  xtics <- id_lev[[prof_var]]
  xtics <- 1:length(xtics)
  
  xrng <- range(xtics)
  yrng <- range(data[,clus_var])
  yrng[2] <- yrng[2] + 0.1*(yrng[2]-yrng[1])
  data.grp <- merge(data, grp1, by = "id_row") 
  
  # plot the group profiles by color
  for(i in rev(1:grp_cnt)) {
    oneGrp <- data.grp[data.grp[,grpVar]==i,]
    plot(0,0,type="n",
      xaxt='n',xlab=prof_var,ylab=yVarLab,xlim=xrng,ylim=yrng)  #
    xlabs <- id_lab[[prof_var]]
    axis(side = 1,at=xtics,labels=xlabs,las=2)
    
    legend("topleft",oneGrp[1,"prim_lbl"],lty=1,col=oneGrp[1,"prim_col"])
    # JBH: legend("topleft",grp_lab[i],lty=1,col=grp_col[i])
    
    for(j in 1:nrow(oneGrp)) {
      lines(xtics,as.vector(oneGrp[j,clus_var]),col=oneGrp[j,"prim_col"],lty=3)
      # JBH: lines(xtics,as.vector(oneGrp[j,clus_var]),col=grp_col[oneGrp[j,'prim_grp']],lty=3)
    }
  }
  plot(0,0,type="n",
    xaxt='n',xlab=prof_var,ylab=yVarLab,xlim=xrng,ylim=yrng)  #
  xlabs <- id_lab[[prof_var]]
  axis(side = 1,at=xtics,labels=xlabs,las=2)
  legend(legendPos, dend_lbl$prim_lbl,lty=rep(1,grp_cnt),col=dend_lbl$prim_col)
  # JBH legend(legendPos,  grp_lab,lty=rep(1,grp_cnt),col=grp_col)
  
  for(i in 1:grp_cnt) {
    oneGrp   <- data.grp[data.grp[,grpVar]==i,clus_var]
    oneGrpMn <- colMeans(oneGrp)
    line_col <- dend_lbl[dend_lbl$prim_grp == i, "prim_col"]
    lines(xtics,oneGrpMn,col=line_col,lty=1,lwd=2)
  }
  par(par.orig)
  pltClusProfMatPan.return <- data.grp
}# end of plotClusProfMatPan



