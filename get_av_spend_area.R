get_av_spend_area <- function() {


}

get_outward_code <- function(id){
  query <- qq("SELECT LEFT(a.postcode, strpos(a.postcode, ' ') - 1) as outcode
  FROM address a
  WHERE(a.practiceid) like '@{id}'")
  
}
