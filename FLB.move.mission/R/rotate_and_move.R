#' Rotates geo-formatted data points by given degrees
#'
#' @param geo_formatted_coords Formatted geo-coordinates.
#' @param degrees_rot Degrees rotation
#' @param waypoint_center The waypoint which the rotation should pivot around (1-indexed)
#' @return rotated Same format as before function, to ensure order of rotation and movement is irrelevant.
#' @export
rotate_geo_formatted = function(geo_formatted_coords, degrees_rot, waypoint_center = 1){
  rotated = maptools::elide(geo_formatted_coords, rotate=degrees_rot,
  						center = sp::coordinates(geo_formatted_coords)[waypoint_center,])
  return(rotated)
}

#' Moves geo-formatted data points to given new location (lat/long)
#'
#' @param coordinates Formatted geo-coordinates.
#' @param waypoint The waypoint for which the coordinates are given, project moves along to that.
#' @param new_lat (~y), new lat for the waypoint to be moved to, if not given, unchanged
#' @param new_lon (~x), new long for the waypoint to be moved to, if not given, unchanged
#' @return new_coord_df Same format as before function, to ensure order of rotation and movement is irrelevant.
#' @export
move_to_coordinates = function(coordinates, waypoint = 1, new_lat=NULL, new_lon=NULL){
    original = sp::coordinates(coordinates)[waypoint,]
    lat_diff = if(is.null(new_lat)) 0 else (original[[1]] - new_lat)
    lon_diff = if(is.null(new_lon)) 0 else (original[[2]] - new_lon)
    new_coord_vector = apply(coordinates(coordinates), MARGIN = 1, function(x){
      x_1 = x[[1]] - lat_diff
      x_2 = x[[2]] - lon_diff
      return(c(x_1, x_2))
    })
    new_coord_df = data.frame(lat = new_coord_vector[1,], lon = new_coord_vector[2,])
    sp::coordinates(new_coord_df) <- c('lat', 'lon')
    new_coord_df$alt = coordinates$alt
    return(new_coord_df)
}
