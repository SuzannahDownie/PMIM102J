user_select_option <- function(){
  cat('\nPlease select one of the following options by typing the number of the option you require followed by enter:\n \n')
  display_option <- create_menu_dataframe()
  user_choice <- readline("Input: ")
  user_choice <- strtoi(user_choice)
  if (user_choice == 1 || user_choice == 2 || user_choice == 3 || user_choice == 4 || user_choice == 5) {
    cat("create this soon")
  } else {
    cat("You have not entered a correct option, try again. \n \n")  
    user_select_option()
  }
  return(user_choice)
}

