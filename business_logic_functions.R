### ALLOW USER TO EXIT THE FUNCTION
exit <- function() { 
  cat("Exiting the program.\nGoodbye")
  invokeRestart("abort")
  }    

### OUTPUT THE RESULT OF THE USER'S MENU SELECTION -
### THIS WILL SHOW THE DESIRED OUTPUT OF PART 1 OF THE ASSIGNMENT AND EITHER 
### ASK USER IF THEY WANT TO GO AGAIN OR QUIT
get_user_select_output <- function(db, id, name, selection) {
  if (selection == 1) {
    user_select_output <- get_av_spend(id, db)
    result <- cat("The average monthly spend on medication at", name, "is:\n \n £", user_select_output, "\n")
    user_go_again()
  } else if (selection == 2) {
    user_select_output <- get_av_spend_area(db, id)
  }
  return(result)
  }


### THIS FUNCTION CALLS TWO DATABASE QUERY FUNCTIONS:
###         - CHECK IF MEDICINE DATA AVAILABLE FOR CHOSEN GP PRACTICE
###         - CHECK IF QOF DATA AVAILABLE FOR CHOSEN GP PRACTICE
### CHECKS IF BOTH ARE AVAILABLE AND LETS THE USER KNOW IF THEY CAN PROCEED.
check_practice_viable <- function(input, db){
  med_avail <- check_med_avail(input, db)
  
  qof_avail <- check_qof_avail(input, db)
  
  if (med_avail == TRUE && qof_avail == TRUE){
    cat("WE HAVE SUFFIECIENT DATA TO PROCEED!\n \n")
    practice_viable <- TRUE
  } else{
    practice_viable <- FALSE
    cat("I'm afraid we have insufficient data about this practice to proceed. Please enter another practice. \n")
    return (main())
  }
  return(practice_viable) 
}  
  
