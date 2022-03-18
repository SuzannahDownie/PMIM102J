library(GetoptLong)
library(tidyverse)
source("gp_data_connection.R")
source("get_user_input.R")
source("check_practice.R")
source("get_data.R")
source("check_med_avail.R")
source("check_qof_avail.R")
source("check_practice_viable.R")
source("get_patient_num.R")

db <- gp_data_connection()

main <- function()  {
  user_input <- get_user_input()
  
  practice <- check_practice(user_input, db)
  
  if (nrow(practice) == 0 || nrow(practice) > 1) {
    print("ERROR: That practice code is incorrect, please try again.")
    return (main())
  } 
  else { 
    print(practice)
    }
  user_confirm_flag <- FALSE
  while (user_confirm_flag == FALSE){
    user_confirm <- readline("Is the above practice correct? Type 'Y' or 'N' and press enter: ")
    user_confirm<- toupper(user_confirm)
    if (user_confirm  == "N" || user_confirm == "'N'") {
      print("Please feel free to enter another practice code.")
      return(main())
    } 
    if (user_confirm == "Y" || user_confirm == "'Y'"){
      user_confirm_flag <- TRUE
      practice_id <- practice$practiceid
      print("Thank you. We are just checking if we have medicine and QOF information for this practice")
    }
    else {
      print("ERROR: You have not entered a valid choice. Please try again. 'Y' or 'N")
    }
  practice_viable <- check_practice_viable(practice_id, db)
  if (practice_viable == FALSE){
    print("I'm afraid we have insufficient data about this practice to proceed. Please enter another practice")
    return (main())
    }

  patient_number <- get_patient_num(practice_id, db)
  paste("The number of patients at this surgery is: ", patient_number)
  }
}

main()
  
  
  


 
 
 