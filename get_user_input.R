get_user_input <- function() {
  user_input_surgery <- readline(paste0("Please enter the Practice ID",
  " of the surgery you require: "))
  clean_input_surgery <- trimws(toupper(user_input_surgery))
  
  return(clean_input_surgery)
}

