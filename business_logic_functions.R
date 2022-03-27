### ALLOW USER TO EXIT THE FUNCTION
exit <- function() { 
  cat("Exiting the program.\nGoodbye")
  invokeRestart("abort")
  }    

### OUTPUT THE RESULT OF THE USER'S MENU SELECTION -
### THIS WILL SHOW THE DESIRED OUTPUT OF PART 1 OF THE ASSIGNMENT
### COULD BE REFACTORED TO BE FAR LESS CUMBERSOME
get_user_select_output <- function(db, id, name, postcode, selection) {
  if (selection == 1) {
    get_total_spend(id, name)
  } else if (selection == 2) {
    get_med_spend(postcode)
  } else if (selection == 3){
    get_diabetes_rate(id)
  } else if (selection == 4){
    get_stat_diabetes_insulin()
    } else if (selection == 5) {
    get_stat_diabetes_metformin()
    } else if (selection == 6) {
      get_wales_spending()
    } else if (selection == 7) {
      compare_insulin_metformin()
    } else if (selection == 8) {
      get_top_medicines(id)
    }
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
    cat("\n \nI'm afraid we have insufficient data about this practice to proceed.\n") 
    cat ("\n \nPlease enter another practice.\n")
    return (main())
  }
  return(practice_viable)
} 

### FUNCTION TO OUTPUT RESULT OF USER SELECTED OPTION 1 - TOTAL SPEND
get_total_spend <- function(id, name){
  user_select_output <- get_av_spend(id, db)
  result <- cat("The average monthly spend on medication at", name, "is:\n \n £", 
                user_select_output, "\n")
  return(result)
}

### FUNCTION TO OUTPUT RESULT OF USER SELECTED OPTION 2 - MED SPEND
get_med_spend <- function(postcode){
  outcode <- get_outcode(db, postcode)
  av_spend_df <- get_av_spend_area(outcode, db, postcode)
  View(av_spend_df)
  plot_spend <- visualise_opt_2(av_spend_df)
}

### FUNCTION TO OUTPUT RESULT OF USER SELECTED OPTION 3 - PRACTICE/WALES DIABETES
get_diabetes_rate <- function(id) {
  diabetes_rate_practice <- get_diabetes_rate_practice(db, id)
  diabetes_rate_wales <- get_diabetes_rate_wales(db, id)
  diabetes_df <- data.frame(diabetes_rate_practice, diabetes_rate_wales)
  diabetes_df <- melt(diabetes_df, measure.vars = c("diabetes_rate_practice", 
                                                    "diabetes_rate_wales"))
  colnames(diabetes_df) <- c("cat_name", "diabetes_rate")
  View(diabetes_df)
  plot_diabetes_rate <- visualise_opt_3(diabetes_df)
}

### FUNCTION TO OUTPUT RESULT OF USER SELECTED OPTION 4 - STATS DIABETES/INSULIN
get_stat_diabetes_insulin <- function(){
  diabetes_insulin_rate <- get_diabetes_and_insulin(db)
  plot_diabetes_insulin <- visualise_opt_4(diabetes_insulin_rate)
  t_test_ins <- t.test(diabetes_insulin_rate$total_insulin, 
                       diabetes_insulin_rate$total_with_diabetes)
  cat("The t test p-value for diabetes and Insulin is:\n \n", 
      format(t_test_ins$p.value, scientific = FALSE), "\n \n")
  if (t_test_ins$p.value < 0.5){
    cat("This is statistically significant. It allows us to reject the null hypothesis\n")
    cat("These results are not the result of chance.\n \n")
    return(t_test_ins)
  }
}

### FUNCTION TO OUTPUT RESULT OF USER SELECTED OPTION 4 - STATS DIABETES/METFORMIN
get_stat_diabetes_metformin <- function() {
  diabetes_metformin_rate <- get_diabetes_and_metformin(db)
  plot_diabetes_metformin <- visualise_opt_5(diabetes_metformin_rate)
  t_test_met <- t.test(diabetes_metformin_rate$total_metformin, 
                   diabetes_metformin_rate$total_with_diabetes)
  cat("The t test p-value for diabetes and Metformin is:\n \n", 
      format(t_test_met$p.value, scientific = FALSE), "\n \n")
  if (t_test_met$p.value < 0.5){
    cat("This is statistically significant. It allows us to reject the null hypothesis\n")
    cat("These results are not the result of chance.\n \n")
    return(t_test_met)
}
}

### FUNCTION TO COMPARE STATISTICAL OUTPUT OF DIABETES AND INSULIN/METFORMIN
compare_insulin_metformin <- function(){
stats_diabetes_insulin <- get_stat_diabetes_insulin()
stats_diabetes_metformin <- get_stat_diabetes_metformin()
if (stats_diabetes_insulin$p.value < stats_diabetes_metformin$p.value){
  cat("Insulin has a stronger statistical relationship with diabetes.\n")
} else {
  cat("Metformin has a stronger statistical relationship with diabetes.\n")
}
}

### FUNCTION TO GET ALL WALES SPENDING AND VISUALISE THIS FOR 2014 AND 2015
get_wales_spending <- function(){
  spend <- get_all_wales_spending()
  View(spend)
  visualise_opt_6(spend)
}

### FUNCTION TO GET TOP MEDICINES, BNF CODES AND VISUALISE THIS
get_top_medicines <- function(id){
  top_meds <- get_top_prescription(id)
  View(top_meds)
  visualise_top_meds(top_meds)
}
