check_practice_viable <- function(input, db_object){
  med_avail <- check_med_avail(input, db_object)
  
  qof_avail <- check_qof_avail(input, db_object)
  
  if (med_avail == TRUE && qof_avail == TRUE){
    print("We have sufficient data for this practice!")
    practice_viable <- TRUE
  } else{
    practice_viable <- FALSE
  }
  return(practice_viable) 
}  
