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
  query <- qq(paste0("SELECT practiceid, street, postcode FROM ",
                              "address a WHERE practiceid = '@{input}'"))
  return(get_data(db, query))
}


### CHECK THAT MEDCINE DATA IS AVAILABLE FOR THE CHOSEN GP PRACTICE
check_med_avail <- function(id, db){
  query <- qq(paste0("SELECT COUNT(DISTINCT g.bnfcode) 
                     FROM gp_data_up_to_2015 g 
                     WHERE practiceid = '@{id}'"))
  med_data <- get_data(db, query)
  
  if (med_data > 0){
    data_avail <- TRUE
  } else {data_avail <- FALSE}
  return(data_avail)
}


### CHECK THAT QOF DATA IS AVAILABLE FOR THE CHOSEN GP PRACTICE
check_qof_avail <- function(id, db){
  practice_query <- qq(paste0("SELECT COUNT(DISTINCT t.indicator)  
                              FROM qof_achievement t 
                              WHERE orgcode = '@{id}'"))
  qof_data <- get_data(db, practice_query)
  
  if (qof_data > 0){
    data_avail <- TRUE
  } else {data_avail <- FALSE}
  return(data_avail)
}


### GET THE TOTAL PATIENT NUMBER FOR THE CHOSEN GP PRACTICE
get_patient_num <- function(id, db, name){
  query <- qq("SELECT MAX(k.field4) 
              FROM public.qof_achievement k 
              WHERE orgcode = '@{id}'")
  num_patients <- get_data(db, query)
  colnames(num_patients)[which(names(num_patients) == "max")] <- "Number of Patients"
  return(num_patients)
}


### GET THE AVERAGE SPEND PER MONTH FOR THE CHOSEN GP PRACTICE
get_av_spend <- function(id, db) {
  query <- qq("SELECT a.practiceid, a.street, ROUND(SUM(g.actcost::decimal), 2) 
  AS total_cost,
  COUNT(DISTINCT g.period) AS total_periods
  FROM gp_data_up_to_2015 g
  INNER JOIN address a 
  ON  g.practiceid = a.practiceid
  WHERE a.practiceid = 'W92021' and g.period >= 201401 and g.period < 201501
  GROUP BY a.practiceid, a.street")
  av_spend_total <- get_data(db, query)
  av_spend_monthly_total <- av_spend_total$total_cost/av_spend_total$total_periods 
  return(av_spend_monthly_total)
}

### GET POSTCODE OUTCODE (BEGINNING)
get_outcode <- function(db, postcode){
  query <- qq("WITH address (postcode) AS (VALUES ('@{postcode}'))
              SELECT SUBSTRING (postcode FROM '[^0-9]{2}[0-9]+')
              FROM address")
  result <- get_data(db, query)
  result <- unlist(result)
  return(result)
}


### GET THE AVERAGE SPEND PER HEAD IN POSTCODE AREA
get_av_spend_area <- function (outcode, db, postcode) {
  outcode <- get_outcode(db, postcode)
  query_total_sum <- qq("SELECT a.practiceid, a.street, SUM(g.actcost) AS total_cost
            FROM gp_data_up_to_2015 g
            INNER JOIN address a
            ON g.practiceid = a.practiceid
            WHERE g.period >= 201401 AND g.period < 201501 
            AND a.postcode LIKE '@{outcode}%'
            GROUP BY a.practiceid, a.street")
  area_sum <- get_data(db, query_total_sum)
  query_area_patient_no <- qq("  
            SELECT a.practiceid, a.street, MAX(k.field4)
            FROM address a
            INNER JOIN qof_achievement k
            ON a.practiceid = k.orgcode
            AND a.postcode LIKE '@{outcode}%'
            GROUP BY a.practiceid, a.street")
  area_total_patients <- get_data(db, query_area_patient_no)
  combine_data <- merge(area_sum, area_total_patients, by="practiceid")
  combine_data$per_patient_spend <- combine_data$total_cost / combine_data$max
  return(combine_data)
}

### GET THE RATE OF DIABETES - CHOSEN PRACTICE
get_diabetes_rate_practice <- function(db, id){
  query <- qq("SELECT *, (ROUND((numerator::decimal/field4::decimal), 3) * 100) 
              AS percentage_with
              FROM qof_achievement 
              WHERE indicator = 'DM001' AND orgcode = '@{id}'")
  result <- get_data(db, query)
  result <- result$percentage_with
  return(result)
}

### GET THE RATE OF DIABETES -  ALL WALES
get_diabetes_rate_wales <- function(db, id){
  query <- qq("SELECT *, (ROUND((numerator::decimal/field4::decimal), 3) * 100) 
                AS percentage_with 
                FROM qof_achievement k
                WHERE indicator = 'DM001' AND k.orgcode NOT IN ('@{id}')")
  result <- get_data(db, query)
  result <- round(mean(result$percentage_with), 2)
  return(result)
}
