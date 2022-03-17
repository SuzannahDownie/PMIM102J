check_qof_avail <- function(input, db){
  practice_query <- qq(paste0("SELECT COUNT(DISTINCT t.indicator)  FROM qof_achievement t WHERE orgcode = '@{practice_id}'"))
  qof_data <- get_data(db, practice_query)
  
  if (qof_data > 0){
    data_avail <- TRUE
  } else {data_avail <- FALSE}
  return(data_avail)
}
