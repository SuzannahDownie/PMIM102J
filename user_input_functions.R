get_user_input <- function() {
  cat("Please enter the Practice ID of the surgery you require:\n")
  user_input_surgery <- readline("Input: ")
  clean_input_surgery <- trimws(toupper(user_input_surgery))
  
  return(clean_input_surgery)
}

user_select_option <- function(){
  cat('\nPlease select one of the following options by typing the number of the option you require followed by enter:\n \n')
  display_option <- create_menu_dataframe()
  user_choice <- readline("Input: ")
  user_choice <- strtoi(user_choice)
  if (user_choice == 1 || user_choice == 2 || user_choice == 3 || user_choice == 4 || user_choice == 5) {
    return(user_choice)
  } else {
    cat("You have not entered a correct option, try again. \n \n")  
    user_select_option()
  }
}

user_go_again <- function() {
  go_again <- cat("\nWould you like see the menu options again?\n \nType 'Y' to see the menu or 'exit' to leave the program\n")
  go_again_input <- readline("Input: ")
  go_again_input <- tolower(go_again_input)
  if (go_again_input  == "y" || go_again_input == "'y'") {
    user_select_option()
  } else if (go_again_input == "exit" || go_again_input == "'exit'"){
    exit()
  } else {
    cat("You have not entered a correct option, try again. \n \n")
    user_go_again()
}
}

