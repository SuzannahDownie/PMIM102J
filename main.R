library(GetoptLong)
library(tidyverse)
library(huxtable)
library(ggpubr)
library(reshape)
library(plotly)
source("user_input_functions.R")
source("user_display_functions.R")
source("business_logic_functions.R")
source("database_query_functions.R")


db <- gp_data_connection()

main <- function()  {
  user_input <- get_user_input()
  
  practice <- check_practice(user_input, db)
  
  if (nrow(practice) == 0 || nrow(practice) > 1) {
    cat("ERROR: That practice code is incorrect, please try again.\n \n")
    return (main())
  } 
  else { 
    print(practice)
    }
  user_confirm_flag <- FALSE
  while (user_confirm_flag == FALSE){
    cat("Is the above practice correct? Type 'Y' or 'N' and press enter: ")
    user_confirm <- readline("Input: ")
    user_confirm<- toupper(user_confirm)
    if (user_confirm  == "N" || user_confirm == "'N'") {
      cat("Please feel free to enter another practice code.\n")
      return(main())
    } else if (user_confirm == "Y" || user_confirm == "'Y'"){
      practice_id <- practice$practiceid
      practice_name <- practice$street
      practice_postcode <- practice$postcode
      cat("Thank you.") 
      cat("We are just checking if we have medicine and QOF information\n")
      cat("for this practice...\n \n")
      user_confirm_flag <- TRUE
    }
    else {
      cat("ERROR: You have not entered a valid choice. Please try again. 'Y' or 'N")
    }
  practice_viable <- check_practice_viable(practice_id, db)
  } 
  patient_number <- get_patient_num(practice_id, db, practice_name)
  cat("The number of patients at this practice is detailed below:\n \n")
  print(patient_number)
  exit_flag = FALSE
  while (exit_flag == FALSE) {
    cat("\nPlease select one of the following options by typing the number of\n")
    cat("the option you require followed by enter:\n \n")
    display_option <- create_menu_dataframe()
    user_choice <- readline("Input: ")
    user_select <- user_select_option(user_choice)
    user_select_output <- get_user_select_output(db, practice_id, practice_name, 
                                                 practice_postcode, user_select)
    exit_or_stay <- user_go_again()
    if (exit_or_stay == 1) {
      exit_flag <- FALSE
    }
    else if (exit_or_stay == 2) {
      exit_flag <- TRUE
    } 
  }
}

main()
  
  
  


 
 
 