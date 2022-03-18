get_patient_num <- function(id, db_object, name){
  query <- qq("SELECT MAX(k.field4) FROM public.qof_achievement k WHERE orgcode = '@{id}'")
  num_patients <- get_data(db_object, query)
  colnames(num_patients)[which(names(num_patients) == "max")] <- "Number of Patients"
  return(num_patients)
}

