check_practice <- function(input, db){
  practice_query <- qq(paste0("select practiceid, street, postcode from ",
                             "address a where practiceid = '@{input}'"))
  return(get_data(db, practice_query))
}
