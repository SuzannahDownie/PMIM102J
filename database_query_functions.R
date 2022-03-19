### INITIALISE DATABASE CONNECTION
gp_data_connection <- function() {
  cat("GP DATA CONNECTION SUCCESSFUL!\n")
  require("RPostgreSQL")
  
  drv = dbDriver("PostgreSQL");
  
  db <- dbConnect(drv, dbname = "gp_practice_data", 
                  host = "localhost", port = 5432,
                  user = "postgres", password = rstudioapi::askForPassword())
}


### FUNCTION TO RUN A QUERY ON THE DATABASE AND RETURN DATA
get_data <- function(db, query){
  result <- dbGetQuery(db, query)
  return(result)
}


### FUNCTION TO CHECK GP PRACTICE ID RETURNS VALUE
check_practice <- function(input, db){
  query <- qq(paste0("select practiceid, street, postcode from ",
                              "address a where practiceid = '@{input}'"))
  return(get_data(db, query))
}


### CHECK THAT MEDCINE DATA IS AVAILABLE FOR THE CHOSEN GP PRACTICE
check_med_avail <- function(id, db){
  query <- qq(paste0("SELECT COUNT(DISTINCT g.bnfcode) FROM gp_data_up_to_2015 g WHERE practiceid = '@{id}'"))
  med_data <- get_data(db, query)
  
  if (med_data > 0){
    data_avail <- TRUE
  } else {data_avail <- FALSE}
  return(data_avail)
}


### CHECK THAT QOF DATA IS AVAILABLE FOR THE CHOSEN GP PRACTICE
check_qof_avail <- function(id, db){
  practice_query <- qq(paste0("SELECT COUNT(DISTINCT t.indicator)  FROM qof_achievement t WHERE orgcode = '@{id}'"))
  qof_data <- get_data(db, practice_query)
  
  if (qof_data > 0){
    data_avail <- TRUE
  } else {data_avail <- FALSE}
  return(data_avail)
}


### GET THE TOTAL PATIENT NUMBER FOR THE CHOSEN GP PRACTICE
get_patient_num <- function(id, db, name){
  query <- qq("SELECT MAX(k.field4) FROM public.qof_achievement k WHERE orgcode = '@{id}'")
  num_patients <- get_data(db, query)
  colnames(num_patients)[which(names(num_patients) == "max")] <- "Number of Patients"
  return(num_patients)
}


### GET THE AVERAGE SPEND PER MONTH FOR THE CHOSEN GP PRACTICE
get_av_spend <- function(id, db) {
  query <- qq("select period, practiceid,
  sum(items) as total_items,
  sum(nic) as total_cost
  from gp_data_up_to_2015
  where practiceid = '@{id}'
  group by period, practiceid")
  av_spend_monthly_totals <- get_data(db, query)
  av_spend_total_sum <- sum(av_spend_monthly_totals$total_cost)
  av_spend_total_periods <- n_distinct(av_spend_monthly_totals$period)
  av_monthly_spend <- av_spend_total_sum/av_spend_total_periods
  return(av_monthly_spend)
}


### FUNCTION TO GET FIRST PART OF POSTCODE (OUTCODE)
get_outcode <- function(db, id){
  query <- qq("SELECT LEFT(address.postcode, strpos(address.postcode, ' ') - 1) as outcode
  FROM address
  WHERE(address.practiceid) = '@{id}'")
  result <- get_data(db, query)
  return(result)
}


get_av_spend_area <- function(outcode, db) {
  query <- qq("select gp_data_up_to_2015.period, address.practiceid,
                            address.street, address.postcode,
                            sum(gp_data_up_to_2015.items) as total_items,
                            sum(gp_data_up_to_2015.nic) as total_cost
                            from gp_data_up_to_2015
                            inner join address
                            on gp_data_up_to_2015.practiceid = address.practiceid
                            where postcode like '@{outcode}%'
                            group by gp_data_up_to_2015.period, address.practiceid,
                            address.street, address.postcode")
  av_spend_area <- get_data(db, query)
  print(av_spend_area)
  return(av_spend_area)
}


# get_av_spend_area(db, query)


# 