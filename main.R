library(GetoptLong)
library(tidyverse)
library(huxtable)
source("gp_data_connection.R")
source("get_user_input.R")
source("check_practice.R")
source("get_data.R")
source("check_med_avail.R")
source("check_qof_avail.R")
source("check_practice_viable.R")
source("get_patient_num.R")
source("create_menu_dataframe.R")
source("user_select_option.R")

db <- gp_data_connection()

main <- function()  {
  user_input <- get_user_input()
  
  practice <- check_practice(user_input, db)
  
  if (nrow(practice) == 0 || nrow(practice) > 1) {
    cat("ERROR: That practice code is incorrect, please try again.\n")
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
      cat("Thank you. We are just checking if we have medicine and QOF information for this practice...\n \n")
      user_confirm_flag <- TRUE
    }
    else {
      cat("ERROR: You have not entered a valid choice. Please try again. 'Y' or 'N")
    }
  practice_viable <- check_practice_viable(practice_id, db)
  if (practice_viable == FALSE){
    cat("I'm afraid we have insufficient data about this practice to proceed. Please enter another practice")
    return (main())
    }
  } # user confirm loop
  patient_number <- get_patient_num(practice_id, db, practice_name)
  cat("The number of patients at this practice is detailed below:\n \n")
  print(patient_number)
  user_select <- user_select_option()
  
} #main

main()
  
  
  


 
 
 