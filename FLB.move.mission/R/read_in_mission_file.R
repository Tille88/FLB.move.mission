preprocess_number_line = function(line){
  removed_comma = stringr::str_replace_all(line, ",", "")
  trimmed = stringr::str_trim(removed_comma)
  numeric = as.numeric(trimmed)
  return(numeric)
}

coord_list_to_geo_format = function(coord_list){
  coordinates = data.frame(matrix(unlist(coord_list), ncol= 3, byrow=T))
  names(coordinates) = c('lat', 'lon', 'alt')
  sp::coordinates(coordinates) <- c('lat', 'lon')
  return(coordinates)
}

#' Reads in file and returns it, together with  in
#'
#' @param path Path to .mission or .plan file
#' @return list containing coordinates, file-lines and extension info
#' @export
read_in_mission_file = function(path){
  file_extension = stringr::str_extract(path, "mission$|plan$")
  file = readr::read_lines(file = path)
  coordinate_list = list()
  match_string = if(file_extension == "mission") "coordinate" else "params"
  for(line in seq_along(file)){
    if(stringr::str_detect(file[[line]], match_string)){
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
      coordinate_list[[length(coordinate_list)+1]] = coordinate_triple_double
    }
  }
  #Process corrdinate list
  geo_coords = coord_list_to_geo_format(coordinate_list)
  return((list(geo_coords, file, file_extension)))
}
