###  NECESSARY LIBRARY IMPORTS AND FILE SOURCES ###
library(GetoptLong)
library(tidyverse)
library(huxtable)
library(ggpubr)
library(reshape)
library(plotly)
library(stringr)
source("user_input_functions.R")
source("user_display_functions.R")
source("business_logic_functions.R")
source("database_query_functions.R")

### CONNECT TO DATABASE ###
db <- gp_data_connection()


### MAIN PROGRAM ###
main <- function()  {
  user_input <- get_user_input()
  
  practice <- check_practice(user_input, db)
  
  user_confirm_flag <- FALSE
  while (user_confirm_flag == FALSE){
    user_confirm_flag <- user_validate_practice(practice)
  practice_id <- practice$practiceid
  practice_name <- practice$street
  practice_postcode <- practice$postcode
  practice_viable <- check_practice_viable(practice_id, db)
  } 
  patient_number <- get_patient_num(practice_id, db, practice_name)
  exit_flag = FALSE
  while (exit_flag == FALSE) {
    create_menu_dataframe()
    user_select <- user_select_option()
    user_select <- strtoi(user_select)
    user_select_output <- get_user_select_output(db, practice_id, practice_name, 
                                                 practice_postcode, user_select)
    exit_or_stay <- user_go_again()
    if (exit_or_stay == 1) {
      exit_flag <- FALSE
    }
    else if (exit_or_stay == 2) {
      exit_flag <- TRUE
    } else if (exit_or_stay == 3) {
      main()
    }
  }
}

### MAIN PROGRAM CALL ###
main()
  
  
  


 
 
 