get_user_input <- function() {
  cat("Please enter the Practice ID of the surgery you require:\n")
  user_input_surgery <- readline("Input: ")
  clean_input_surgery <- trimws(toupper(user_input_surgery))
  
  return(clean_input_surgery)
}

user_select_option <- function(user_choice){
  while (!(user_choice %in% c("1", "2", "3", "4", "5"))) {
    # if (!(user_choice %in% c("1", "2", "3", "4", "5")))
    cat("You have not entered a correct option, try again. \n \n")  
    cat('\nPlease select one of the following options by typing the number of the option you require followed by enter:\n \n')
    display_option <- create_menu_dataframe()
    user_choice <- readline("Input: ")
  }
    user_choice <- strtoi(user_choice)
    return(user_choice)
  
}


user_go_again <- function() {
  go_again <- cat("\nWould you like see the menu options again?\n \nType 'Y' to see the menu or 'exit' to leave the program\n")
  go_again_input <- readline("Input: ")
  go_again_input <- tolower(go_again_input)
  if (go_again_input  == "y" || go_again_input == "'y'") {
    result <- 1
    return(result)
  } else if (go_again_input == "exit" || go_again_input == "'exit'"){
    result <- 2
    return(result)
    exit()
  } else {
    cat("You have not entered a correct option, try again. \n \n")
    user_go_again()
}
}

