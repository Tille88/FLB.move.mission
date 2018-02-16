#' Writes a .mission file (currently no .plan files)
#'
#' @param new_coordinates Coordinates for the new plan after alterations
#' @param filepath File name/relative path+file name
#' @param prev_file_lines original files read in
#' @param file_extension  extension used for read in file
#' @export
write_mission_file = function(new_coordinates, filepath, prev_file_lines, file_extension){
  match_string = if(file_extension == "mission") "coordinate" else "params"
  new_file = prev_file_lines
  coordinate_counter = 1
  pattern = "[[:digit:]\\.-]+"
  for(line in seq_along(new_file)){
    if(stringr::str_detect(new_file[[line]], match_string)){
      if(file_extension == "mission"){
        coord_row = new_coordinates[coordinate_counter,]
        new_file[[line+1]] = stringr::str_replace(new_file[[line+1]],
                                         pattern = pattern,
                                         as.character(sp::coordinates(coord_row)[[1]]))
        new_file[[line+2]] = stringr::str_replace(new_file[[line+2]],
                                         pattern = pattern,
                                         as.character(sp::coordinates(coord_row)[[2]]))
        new_file[[line+3]] = stringr::str_replace(new_file[[line+3]],
                                         pattern = pattern,
                                         as.character(coord_row[[1]]))
        coordinate_counter = coordinate_counter + 1
      } else if(file_extension=="plan"){
        stop("TODO: Not done for this filetype...")
      }
    }
  }
  readr::write_lines(new_file, filepath)
}
