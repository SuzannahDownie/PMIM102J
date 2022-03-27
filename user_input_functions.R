### FUNCTION TO GET USER INPUT OF PRACTICE ID
get_user_practice_input <- function() {
    cat("Please enter the Practice ID of the surgery you require:\n")
    user_input_surgery <- readline("Input: ")
    clean_input_surgery <- trimws(toupper(user_input_surgery))
    return(clean_input_surgery)
}

### THIS FUNCTION VALIDATES THE PRACTICE FOUND BY ASKING THE USER IF IT IS CORRECT
user_validate_practice <- function(practice){
    cat("\n \nIs the above practice correct? Type 'Y' or 'N' and press enter: ")
    user_confirm <- readline("Input: ")
    user_confirm<- toupper(user_confirm)
    if (user_confirm  == "N" || user_confirm == "'N'") {
    cat("Please feel free to enter another practice code.\n")
    return(main())
    } else if (user_confirm == "Y" || user_confirm == "'Y'"){
    cat("Thank you. ") 
    cat("We are just checking if we have medicine and QOF information\n")
    cat("for this practice.\n \n")
    cat("This may take a moment...")
    user_confirm_flag <- TRUE
    return(user_confirm_flag)
    } else {
    cat("ERROR: You have not entered a valid choice. Please try again. 'Y' or 'N")
    user_validate_practice()
  }
}

### FUNCTION TO ASK THE USER TO SELECT AN AVAILABLE MENU OPTION
user_select_option <- function(){
    user_choice <- readline("Input: ")
    while (!(user_choice %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", 
                                "10", "11"))) {
    cat("You have not entered a correct option, try again. \n \n")  
    display_option <- create_menu_dataframe()
    user_select_option()
    } 
    if (user_choice %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", 
                           "11")) {
    return(user_choice)
}
}

### FUNCTION TO ASK USER IF THEY WANT TO TRY ANOTHER MENU OPTION OR QUIT PROGRAM
user_go_again <- function() {
    cat("\nWhat would you like to do:\n \n")
    cat("Type 'Y' to see the menu again.\n")
    cat("Type 'done' to leave the program. \n")
    cat("Type 'new' to start again with a new practice ID. \n")
    go_again_input <- readline("Input: ")
    go_again_input <- tolower(go_again_input)
    if (go_again_input  == "y") {
    result <- 1
    return(result)
    } else if (go_again_input == "done"){
    result <- 2
    exit()
    return(result)
    } else if (go_again_input == "new"){
    result <- 3
    return(result)
    } else {
    cat("You have not entered a correct option, try again. \n \n")
    user_go_again()
}
}

