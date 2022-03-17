get_patient_num <- function(input, db_object){
  query <- qq(paste0("SELECT MAX(k.field4) FROM qof_achievement k WHERE orgcode = '@input'"))
  num_patients <- get_data(db_object, query)
  return(num_patients)
}