#' Fills a template with correct values FOR .PLAN FILE
#'
gen_filled_template = function(list_of_filled_vals, empty_template){
  template_filled = empty_template
  for(i in seq_along(empty_template)){
    matched_list = stringr::str_match(empty_template[[i]], names(list_of_filled_vals))
    if(!all(is.na(matched_list))){
      value_for_fill = list_of_filled_vals[[which(!is.na(matched_list))]]
      to_be_replaced = matched_list[[which(!is.na(matched_list))]]
      template_filled[[i]] = stringr::str_replace(empty_template[[i]], to_be_replaced, value_for_fill)
    }
  }
  return(template_filled)
}


#' Extract used values into list for input into .mission file FOR .PATH FILE
#' FOR .PLAN FILE
#'
file_values_extracted = function(plan_file, new_coordinates){

  # Find row where coordinates and such start
  for(i in seq_along(plan_file)){
    if(!is.na(stringr::str_match(plan_file[i], "items"))){
      cur_row = i + 1
      break("BREAK")
    }
  }

  empty_list_of_values = list(
    COMMAND = NULL,
    LAT = NULL,
    LON = NULL,
    REL_ALT = NULL,
    DO_JMP_ID = NULL,
    FRAME = NULL,
    PARAM_1 = NULL,
    PARAM_2 = NULL,
    PARAM_3 = NULL,
    PARAM_4 = NULL
  )

	fill_values = empty_list_of_values
  	values_list = list()
  	numeric_ptrn_with_period = "[0-9\\.]+"
	  # With current row, update have cur_row
  	while(paste0(stringr::str_trim(plan_file[cur_row]), stringr::str_trim(plan_file[cur_row+1])) != "}],"){
	    cur_str = stringr::str_trim(plan_file[cur_row])
	    if(!is.na(stringr::str_match(cur_str, "command"))){
	      fill_values$COMMAND = stringr::str_match(plan_file[cur_row], numeric_ptrn_with_period)
	      cur_row = cur_row + 1
	      cur_str = stringr::str_trim(plan_file[cur_row])
	    }
	    if(!is.na(stringr::str_match(cur_str, "doJumpId"))){
	      fill_values$DO_JMP_ID = stringr::str_match(plan_file[cur_row], numeric_ptrn_with_period)
	      cur_row = cur_row + 1
	      cur_str = stringr::str_trim(plan_file[cur_row])
	    }
	    if(!is.na(stringr::str_match(cur_str, "frame"))){
	      fill_values$FRAME = stringr::str_match(plan_file[cur_row], numeric_ptrn_with_period)
	      cur_row = cur_row + 1
	      cur_str = stringr::str_trim(plan_file[cur_row])
	    }
	    if(!is.na(stringr::str_match(cur_str, "params"))){
	      fill_values$PARAM_1 = stringr::str_match(plan_file[cur_row + 1], numeric_ptrn_with_period)
	      fill_values$PARAM_2 = stringr::str_match(plan_file[cur_row + 2], numeric_ptrn_with_period)
	      fill_values$PARAM_3 = stringr::str_match(plan_file[cur_row + 3], numeric_ptrn_with_period)
	      fill_values$PARAM_4 = stringr::str_match(plan_file[cur_row + 4], numeric_ptrn_with_period)
	      fill_values$LAT = as.character(sp::coordinates(new_coordinates)[[length(values_list) + 1, 1]])
	      fill_values$LON = as.character(sp::coordinates(new_coordinates)[[length(values_list) + 1, 2]])
	      fill_values$REL_ALT = as.character(coordinates_and_file[[1]][length(values_list) + 1,]$alt)
	      values_list[[length(values_list) + 1]] = fill_values
	      fill_values = empty_list_of_values
	      cur_row = cur_row + 8
	    }
	    cur_row = cur_row + 1
	  }
	  return(values_list)
}





#' Writes a .mission file (currently no .plan files)
#'
#' @param new_coordinates Coordinates for the new plan after alterations
#' @param filepath File name/relative path+file name
#' @param prev_file_lines original files read in
#' @param file_extension  extension used for read in file
#' @export
write_mission_file = function(new_coordinates,
                              filepath,
                              prev_file_lines,
                              file_extension) {
  if (file_extension == "mission") {
  	#Unused, only for .mission these days...
    match_string = if (file_extension == "mission")
      "coordinate"
    else
      "params"
    new_file = prev_file_lines
    coordinate_counter = 1
    pattern = "[[:digit:]\\.-]+"
    for (line in seq_along(new_file)) {
      if (stringr::str_detect(new_file[[line]], match_string)) {
        coord_row = new_coordinates[coordinate_counter, ]
        new_file[[line + 1]] = stringr::str_replace(new_file[[line + 1]],
                                                    pattern = pattern,
                                                    as.character(sp::coordinates(coord_row)[[1]]))
        new_file[[line + 2]] = stringr::str_replace(new_file[[line + 2]],
                                                    pattern = pattern,
                                                    as.character(sp::coordinates(coord_row)[[2]]))
        new_file[[line + 3]] = stringr::str_replace(new_file[[line + 3]],
                                                    pattern = pattern,
                                                    as.character(coord_row[[1]]))
        coordinate_counter = coordinate_counter + 1
      }
    }
  } else if (file_extension == "plan") {
    # Get list of values to replace old lines with
    file_values_list = file_values_extracted(prev_file_lines, new_coordinates)

    # Get template section for .mission style file
    #template_file = readr::read_lines("./mission_template.mission")
    template_file = readr::read_lines(
      system.file("extdata", "mission_template.mission", package = "FLB.move.mission")
    )
    start_file_rows = template_file[1:4]
    end_file_rows = template_file[6:length(template_file)]

    # Get parameter section
    #parameter_section = readr::read_lines("./template_mission_rows_to_add.mission")
    parameter_section = readr::read_lines(
      system.file(
        "extdata",
        "template_mission_rows_to_add.mission",
        package = "FLB.move.mission"
      )
    )


    # Fill values into template sections
    list_of_filled_templates = lapply(file_values_list, function(x)
      gen_filled_template(x, parameter_section))
    vector_form = unlist(list_of_filled_templates)
    # Last row needs no "," for formatting reasons
    vector_form[length(vector_form)] = stringr::str_replace(vector_form[length(vector_form)], ",", "")
    new_file = c(start_file_rows, vector_form, end_file_rows)
  }
  readr::write_lines(new_file, filepath)
}
