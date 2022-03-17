check_med_avail <- function(input, db){
  practice_query <- qq(paste0("SELECT COUNT(DISTINCT g.bnfcode) FROM gp_data_up_to_2015 g WHERE practiceid = '@{practice_id}'"))
  med_data <- get_data(db, practice_query)

  if (med_data > 0){
    data_avail <- TRUE
  } else {data_avail <- FALSE}
   return(data_avail)
}
