get_data <- function(db_object, query_string){
  result <- dbGetQuery(db_object, query_string)
  return(result)
}