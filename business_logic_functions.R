### ALLOW USER TO EXIT THE FUNCTION
exit <- function() { 
    cat("Exiting the program.\nGoodbye")
    invokeRestart("abort")
  }    

### OUTPUT THE RESULT OF THE USER'S MENU SELECTION -
### THIS WILL SHOW THE DESIRED OUTPUT OF PART 1 OF THE ASSIGNMENT
### COULD BE REFACTORED TO BE FAR LESS CUMBERSOME
get_user_select_output <- function(id, name, postcode, selection) {
    if (selection == 1) {
    practice_med_spend(id, name)
    } else if (selection == 2) {
    practice_postcode_med_spend(db = db, postcode)
    } else if (selection == 3){
    practice_diabetes_rate(id)
    } else if (selection == 4){
    practice_top_meds(id)
    } else if (selection == 5) {
    wales_diabetes_rate_ins()
    } else if (selection == 6) {
    wales_diabetes_rate_met()
    } else if (selection == 7) {
    comp_wales_diabetes_insmet()
    } else if (selection == 8) {
    wales_monthly_spend()
    } else if (selection == 9) {
    wales_smoking_cancer_diagnosis()
    } else if (selection == 10) {
    wales_smoking_COPD_diagnosis()
    } else if (selection == 11) {
    comp_wales_smoking_cancer_COPD()
    }
}


### THIS FUNCTION CALLS TWO DATABASE QUERY FUNCTIONS:
###         - CHECK IF MEDICINE DATA AVAILABLE FOR CHOSEN GP PRACTICE
###         - CHECK IF QOF DATA AVAILABLE FOR CHOSEN GP PRACTICE
### CHECKS IF BOTH ARE AVAILABLE AND LETS THE USER KNOW IF THEY CAN PROCEED.
check_practice_viable <- function(input){
    med_avail <- check_med_avail(input)
    
    qof_avail <- check_qof_avail(input)
    
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

### FUNCTION TO OUTPUT RESULT OF USER SELECTED OPTION - TOTAL PRACTICE SPEND
practice_med_spend <- function(id, name){
    user_select_output <- get_practice_med_spend(id)
    result <- cat("The average monthly spend on medication at", name, 
                  "is:\n \n £", user_select_output, "\n")
    return(result)
}

### FUNCTION TO OUTPUT RESULT OF USER SELECTED OPTION - MED SPEND
practice_postcode_med_spend <- function(db = db, postcode){
    outcode <- get_outcode(db, postcode)
    av_spend_df <- get_practice_postcode_med_spend(outcode, postcode)
    View(av_spend_df)
    plot_spend <- vis_practice_postcode_med_spend(av_spend_df)
}

### FUNCTION TO OUTPUT RESULT OF USER SELECTED OPTION - PRACTICE/WALES DIABETES
practice_diabetes_rate <- function(id) {
    diabetes_rate_practice <- get_practice_diabetes_rate(id)
    diabetes_rate_wales <- get_wales_diabetes_rate(id)
    diabetes_df <- data.frame(diabetes_rate_practice, diabetes_rate_wales)
    diabetes_df <- melt(diabetes_df, measure.vars = c("diabetes_rate_practice", 
                                                      "diabetes_rate_wales"))
    colnames(diabetes_df) <- c("cat_name", "diabetes_rate")
    View(diabetes_df)
    plot_diabetes_rate <- vis_practice_diabetes_rate(diabetes_df)
}

### FUNCTION TO OUTPUT RESULT OF USER SELECTED OPTION - STATS DIABETES/INSULIN
wales_diabetes_rate_ins <- function(){
    diabetes_insulin_rate <- get_wales_diabetes_rate_ins()
    plot_diabetes_insulin <- vis_wales_diabetes_rate_ins(diabetes_insulin_rate)
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

### FUNCTION TO OUTPUT RESULT OF USER SELECTED  - STATS DIABETES/METFORMIN
wales_diabetes_rate_met <- function() {
    diabetes_metformin_rate <- get_wales_diabetes_rate_met()
    plot_diabetes_metformin <- vis_wales_diabetes_rate_met(diabetes_metformin_rate)
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
comp_wales_diabetes_insmet <- function(){
    stats_diabetes_insulin <- wales_diabetes_rate_ins()
    stats_diabetes_metformin <- wales_diabetes_rate_met()
    if (stats_diabetes_insulin$p.value < stats_diabetes_metformin$p.value){
      cat("Insulin has a stronger statistical relationship with diabetes.\n")
    } else {
      cat("Metformin has a stronger statistical relationship with diabetes.\n")
}
}

### FUNCTION TO GET ALL WALES SPENDING AND VISUALISE THIS FOR 2014 AND 2015
wales_monthly_spend <- function(){
    spend <- get_wales_monthly_spend()
    View(spend)
    vis_wales_monthly_spend(spend)
}

### FUNCTION TO GET TOP MEDICINES, BNF CODES AND VISUALISE THIS
practice_top_meds <- function(id){
    top_meds <- get_practice_top_meds(id)
    View(top_meds)
    vis_practice_top_meds(top_meds)
}

### FUNCTION TO VISUALISE RELATIONSHIP BETWEEN SMOKING AND CANCER DIAGNOSIS 
### INCLUDING T-TEST
wales_smoking_cancer_diagnosis <- function() {
    wales_cancer_rate <- get_wales_cancer_diagnosis()
    wales_smoking_rate <- get_wales_smoking()
    combined_df <- merge(wales_cancer_rate, wales_smoking_rate, 
                      by = c("practiceid", "street"))
    combined_df <- combined_df %>% select(-indicator.x, -indicator.y, 
                                          -field4, -county.y)
    vis_wales_smoking_cancer_diagnosis(combined_df)
    t_test_smoking_cancer <- t.test(combined_df$total_cancer, 
                                    combined_df$total_smokers)
    print(t_test_smoking_cancer)
    cat("The t test p-value for smoking and cancer diagnosis is:\n \n", 
        format(t_test_smoking_cancer$p.value, scientific = FALSE), "\n \n")
    if (t_test_smoking_cancer$p.value < 0.5){
      cat("This is statistically significant. It allows us to reject the null hypothesis\n")
      cat("These results are not the result of chance.\n \n")
    return(t_test_smoking_cancer)
    }
}

### FUNCTION TO VISUALISE RELATIONSHIP BETWEEN SMOKING AND COPD DIAGNOSIS 
### INCLUDING T-TEST
wales_smoking_COPD_diagnosis <- function() {
    wales_COPD_rate <- get_wales_COPD_diagnosis()
    wales_smoking_rate <- get_wales_smoking()
    combined_df <- merge(wales_COPD_rate, wales_smoking_rate, 
                         by = c("practiceid", "street"))
    combined_df <- combined_df %>% select(-indicator.x, -indicator.y, 
                                          -field4, -county.y)
    vis_wales_smoking_COPD_diagnosis(combined_df)
    t_test_smoking_COPD <- t.test(combined_df$total_copd, 
                                    combined_df$total_smokers)
    print(t_test_smoking_COPD)
    cat("The t test p-value for smoking and cancer diagnosis is:\n \n", 
        format(t_test_smoking_COPD$p.value, scientific = FALSE), "\n \n")
    if (t_test_smoking_COPD$p.value < 0.5){
      cat("This is statistically significant. It allows us to reject the null hypothesis\n")
      cat("These results are not the result of chance.\n \n")
    return(t_test_smoking_COPD)
    }
}

### FUNCTION TO COMPARE STATISTICAL RELATIONSHIP BETWEEN SMOKING AND COPD/CANCER
comp_wales_smoking_cancer_COPD <- function() {
    stats_smoking_COPD <- wales_smoking_COPD_diagnosis()
    stats_smoking_cancer <- wales_smoking_cancer_diagnosis()
    if (stats_smoking_cancer$p.value < stats_smoking_COPD$p.value){
      cat("Cancer has a stronger statistical relationship with smoking.\n")
    } else {
      cat("COPD has a stronger statistical relationship with smoking.\n")
      }
}