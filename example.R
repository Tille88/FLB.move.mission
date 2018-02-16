setwd("~/Desktop/mission_move/")

#Read in files
library(readr)
library(stringr)


preprocess_number_line = function(line){
  removed_comma = str_replace_all(line, ",", "")
  trimmed = str_trim(removed_comma)
  numeric = as.numeric(trimmed)
  return(numeric)
}


read_in_mission_file = function(path){
  file_extension = str_extract(path, "mission$|plan$")
  file = read_lines(file = path)
  coordinate_list = list()
  match_string = if(file_extension == "mission") "coordinate" else "params" 
  for(line in seq_along(file)){
    if(str_detect(file[[line]], match_string)){
      if(file_extension == "mission"){
        coordinate_triple = c(file[[line+1]],
                              file[[line+2]],
                              file[[line+3]])
      } else if(file_extension=="plan"){
        coordinate_triple = c(file[[line+5]],
                              file[[line+6]],
                              file[[line+7]])
      }
      coordinate_triple_double = sapply(coordinate_triple, preprocess_number_line)
      #test_triple = coordinate_triple
      coordinate_list[[length(coordinate_list)+1]] = coordinate_triple_double
    }
  }
  #Process corrdinate list
  geo_coords = coord_list_to_geo_format(coordinate_list)
  return((list(geo_coords, file, file_extension)))
}

library(maptools)

coord_list_to_geo_format = function(coord_list){
  coordinates = data.frame(matrix(unlist(coord_list), ncol= 3, byrow=T))
  names(coordinates) = c('lat', 'lon', 'alt')
  coordinates(coordinates) <- c('lat', 'lon')  
  return(coordinates)
}

rotate_geo_formatted = function(geo_formatted_coords, degrees_rot, waypoint_center = 1){
  rotated = elide(geo_formatted_coords, rotate=degrees_rot, center=coordinates(geo_formatted_coords)[waypoint_center,])  
  return(rotated)
}

move_to_coordinates = function(coordinates, waypoint = 1, new_lat=NULL, new_lon=NULL){
    original = coordinates(coordinates)[waypoint,]
    lat_diff = if(is.null(new_lat)) 0 else (original[[1]] - new_lat)
    lon_diff = if(is.null(new_lon)) 0 else (original[[2]] - new_lon)
    new_coord_vector = apply(coordinates(coordinates), MARGIN = 1, function(x){
      x_1 = x[[1]] - lat_diff
      x_2 = x[[2]] - lon_diff
      return(c(x_1, x_2))
    })
    new_coord_df = data.frame(lat = new_coord_vector[1,], lon = new_coord_vector[2,])
    print(new_coord_df)
    coordinates(new_coord_df) <- c('lat', 'lon')
    new_coord_df$alt = coordinates$alt
    return(new_coord_df)
}



write_mission_file = function(new_coordinates, filepath, prev_file_lines, file_extension){
  match_string = if(file_extension == "mission") "coordinate" else "params" 
  new_file = prev_file_lines
  coordinate_counter = 1
  pattern = "[[:digit:]\\.-]+"
  for(line in seq_along(new_file)){
    if(str_detect(new_file[[line]], match_string)){
      if(file_extension == "mission"){
        coord_row = new_coordinates[coordinate_counter,]
        new_file[[line+1]] = str_replace(new_file[[line+1]], 
                                         pattern = pattern, 
                                         as.character(coordinates(coord_row)[[1]]))
        new_file[[line+2]] = str_replace(new_file[[line+2]], 
                                         pattern = pattern, 
                                         as.character(coordinates(coord_row)[[2]]))
        new_file[[line+3]] = str_replace(new_file[[line+3]], 
                                         pattern = pattern, 
                                         as.character(coord_row[[1]]))
        coordinate_counter = coordinate_counter + 1
      } else if(file_extension=="plan"){
        stop("TODO: Not done for this filetype...")
      }
    }
  }
  write_lines(new_file, filepath)
}



###############
# EXAMPLE
library(FLB.move.mission)


coordinates_and_file = read_in_mission_file("./01_First_mission.mission")
#coordinates_and_file = read_in_mission_file("./missions-1-round-flight.plan")
coordinates = coordinates_and_file[[1]]

rotated = rotate_geo_formatted(coordinates, 90, 2)

plot(rotated)
plot(rbind(coordinates, rotated))
text(coordinates[,'alt'],  row.names(coordinates),
     cex=0.65, pos=3,col="red") 



#(LAT, LON) maps to (Y, X)
moved = move_to_coordinates(rotated, 2, new_lon = coordinates(rotated)[1,2]+0.005)
#default plot does have axes mixed up
plot(rbind(coordinates, rotated, moved))

rotated2 = rotate_geo_formatted(moved, 90, 2)
plot(rbind(coordinates, rotated, moved, rotated2))
text(rbind(coordinates, rotated, moved, rotated2)[,'alt'],  row.names(coordinates),
     cex=0.65, pos=3,col="red") 


write_mission_file(rotated2, 
                   filepath = "./01_mission_altered.mission",
                   coordinates_and_file[[2]], 
                   coordinates_and_file[[3]])




# Alter to package