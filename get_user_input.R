get_user_input <- function() {
  cat("Please enter the Practice ID of the surgery you require:\n")
  user_input_surgery <- readline("Input: ")
  clean_input_surgery <- trimws(toupper(user_input_surgery))
  
  return(clean_input_surgery)
}

