#' @title Add a text annotation to map from clusterMap 
#' 
#' @description Add a text annotation to map from clusterMap
#'   
#' @details ...
#' 
#' @param cmap map object from clusterMap
#' @param lat vertical location of annotation expressed as latitude
#' @param lon horizontal location of annotation expressed as longitude
#' @param lab_txt text of label. Can be a vector
#' @param x_space change in horizontal location between lines
#' @param y_space change in vertical location between lines
#' @param lab_col label color
#' @param lab_size label size
#' @param h_just horizontal justification parameter
#' 
#' @examples 
#' \dontrun{
#' m <- clusterMap(...)
#' m2 <-clusterMapLabel(cmap = m
#'   , lat = 39.5, lon = -77.00
#'   , lab_txt = c("Station Grouping", "by", "TN Level")
#'   , x_space = 0, y_space = 0.1, lab_col <- "black", lab_size = 8)
#' 
#' }
#' 
#' 
#' @return map object 
#' 
#' @seealso \code{\link{clusterMap}}
#' 
#' @export
#' 
clusterMapLabel <- function(cmap, lat, lon, lab_txt, x_space=0, y_space=0
  , lab_col="black", lab_size=8, h_just=0)
{
  x1 <- lon
  y1 <- lat
  for(itxt in 1:length(lab_txt))
  {
    cmap <- cmap +
      ggplot2::geom_text() +
      ggplot2::annotate("text", label = lab_txt[itxt], x = x1, y = y1
        , size = lab_size, colour = lab_col, hjust = h_just)
    x1 <- x1 + x_space; y1 <- y1 - y_space
  }
  
  return(cmap)
} # end ~ function clusterMapLabel



#' @title Wrapper function for mapPoints() function 
#' 
#' @description Wrapper function for mapPoints() function 
#'   
#' @details ...
#'
#' @param grp_data cluster analysis results table
#' @param grp_lab group labels
#' @param grp_col group color
#' @param leg_title legend title
#' @param col_lat column name with latitude data
#' @param col_lon column name with longitude data
#' @param col_grp column with group 
#' @param leg_pos legend position
#' @param leg_labs legend labels
#' @param leg_cols legend colors 
#' @param file_layer file with base layer
#' @param zoom_buffer zoom buffer
#' @param map_coord_ratio map coordinate ratio
#' @param boo_tidal tidal indicator
#'
#' @examples 
#' \dontrun{
#' m <- clusterMap(...)
#' m2 <-clusterMapLabel(cmap = m
#'   , lat = 39.5, lon = -77.00
#'   , lab_txt = c("Station Grouping", "by", "TN Level")
#'   , x_space = 0, y_space = 0.1, lab_col <- "black", lab_size = 8)
#' 
#' }
#' 
#' 
#' @return map object 
#' 
#' @seealso \code{\link{clusterMap}}
#' 
#' @export
#' 
clusterMap <- function(grp_data, grp_lab,grp_col, leg_title = "Station Grouping",
  col_lat = "latitude", col_lon = "longitude", col_grp = "grpColor",
  leg_pos = "left", leg_labs = grp_lab, leg_cols = grp_col,
  file_layer = NA, 
  zoom_buffer = NA, map_coord_ratio = 1.3, boo_tidal = TRUE) {
  m <- mapPoints(pts = grp_data
    , col_lat = col_lat
    , col_lon = col_lon
    , col_grp = col_grp
    , leg_pos = leg_pos
    , leg_title = leg_title
    , leg_labs = grp_lab
    , leg_cols = grp_col
    , file_layer = file_layer
    , zoom_buffer = zoom_buffer
    , map_coord_ratio = map_coord_ratio
    , boo_tidal = boo_tidal
  )
  return(m)
} # end ~ function: clusterMap

#' @title Basic map plotting function 
#' 
#' @description Basic map plotting function 
#'   
#' @details ...
#'
#' @param pts table of data to plot on a map
#' @param col_lat column name with latitude data
#' @param col_lon column name with longitude data
#' @param col_grp column name with group colors
#' @param leg_pos legend position
#' @param leg_title legend title
#' @param leg_labs legend labels
#' @param leg_cols legend colors
#' @param file_layer directory with base layer  (note: I’ll probably switch to the file.path at a later time)
#' @param zoom_buffer zoom layer
#' @param map_coord_ratio map coordinate ratio
#' @param boo_tidal flag to indicate tidal
#'
#' @examples 
#' \dontrun{
#' m <- clusterMap(...)
#' m2 <-clusterMapLabel(cmap = m
#'   , lat = 39.5, lon = -77.00
#'   , lab_txt = c("Station Grouping", "by", "TN Level")
#'   , x_space = 0, y_space = 0.1, lab_col <- "black", lab_size = 8)
#' 
#' }
#' 
#' 
#' @return map object 
#' 
#' @seealso \code{\link{clusterMap}}
#' 
#' @export
#' 
mapPoints <- function(pts
  , col_lat = "latitude"
  , col_lon = "longitude"
  , col_grp = NA
  , leg_pos = "bottom"
  , leg_title = NULL
  , leg_labs = NULL
  , leg_cols = NULL
  , file_layer = file_layer 
  , zoom_buffer = NA
  , map_coord_ratio = 1.3
  , boo_tidal = TRUE) {
  
  # QC----
  qc_cols <- sum(c(col_lat, col_lon, col_grp) %in% names(pts))
  if(qc_cols != 3){
    stop("Columns missing in data frame.")
  }## IF ~ qc_cols != 3 ~ END
  
  # Munge----
  ## Add Factor Levels to Group Fill
  if(is.null(leg_cols)) {
    pts[, col_grp] <- factor(pts[, col_grp]
      , levels = unique(pts[, col_grp]))
  } else {
    pts[, col_grp] <- factor(pts[, col_grp]
      , levels = leg_cols)
  }## IF ~ is.null(fl_grp_fill) ~ END
  ## Prepare data for plot
  pts <- ggplot2::fortify(pts)
  
  # Load map layer----
  if (is.na(file_layer)) {
    base_layer <- baycluster::shape_cbseg
  } else {
    loadRData <- function(fileName) {
      load(fileName)
      get(ls()[ls() != "fileName"])
    }
    base_layer <- loadRData(file_layer)
  }

  # if (isTRUE(boo_tidal)) {
  #   load(file.path(file_layer, "map_layers.RDA"))
  # } else {
  #   msg <- "Function not (yet) set up for non-tidal map."
  #   stop(msg)
  # }## IF ~ isTRUE(boo_tidal) ~ END
  
  # Map, Base----
  p <- ggplot2::ggplot() + 
    ggplot2::geom_polygon(data = shp_cbseg
      , ggplot2::aes(long, lat, group=group, fill=hole)
      , colour = "grey59"
      , fill = "lightskyblue") +
    ggplot2::theme_void()
  
  # Map, Points----
  p <- p + 
    ggplot2::geom_point(data = pts
      , ggplot2::aes_string(x = col_lon
        , y = col_lat
        , group = col_grp
        , fill = col_grp
      )
      , size = 4
      , pch = 21
      , na.rm = TRUE)
  # if include coord_fixed here get warning when use again later
  ## Coordinate system already present. Adding new coordinate system
  ## , which will replace the existing one.
  
  # Map, Legend----
  if(is.null(leg_title)) {
    p <- p + 
      ggplot2::scale_fill_manual(name = ggplot2::element_blank()
        , labels = leg_labs
        , values = leg_cols) + 
      #ggplot2::theme(legend.title = element_blank()) + 
      ggplot2::theme(legend.position = leg_pos)
  } else {
    p <- p + 
      ggplot2::scale_fill_manual(name = leg_title
        , labels = leg_labs
        , values = leg_cols) +
      #ggplot2::labs(fill = leg_title) + 
      ggplot2::theme(legend.position = leg_pos)
  }## IF ~ is.na(leg_title) ~ END
  
  # Map, Zoom----
  ## coord_fixed
  if(!anyNA(zoom_buffer)) {
    # Ensure positive
    zoom_buffer <- abs(zoom_buffer)
    # Check length
    if(length(zoom_buffer) == 1){
      zoom_buffer <- c(zoom_buffer[1], zoom_buffer[1])
    }## IF ~ length(zoom_buffer) ~ END
    # Create map extent
    x_min <- min(pts[, col_lon]) - zoom_buffer[1]
    x_max <- max(pts[, col_lon]) + zoom_buffer[1]
    y_min <- min(pts[, col_lat]) - zoom_buffer[2]
    y_max <- max(pts[, col_lat]) + zoom_buffer[2]
    # Apply to map
    p <- p +
      ggplot2::coord_fixed(ratio = map_coord_ratio
        , xlim = c(x_min, x_max)
        , ylim = c(y_min, y_max))
  } else {
    p <- p +
      ggplot2::coord_fixed(map_coord_ratio) 
  }## IF ~ !is.na(zoom_buffer) ~ END
  
  # Return----
  return(p)
  
} # FUNCTION ~ END
