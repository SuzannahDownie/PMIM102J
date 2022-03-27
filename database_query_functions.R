### <<PMIM102J - Scientific Computing and Healthcare>
### <Database Project>
### <Version 1>
### <Suzannah Downie>
### <Subfile - Database Query Functions>
### <Contains the database query functions for the PMIM102J assessment>

### INITIALISE DATABASE CONNECTION
gp_data_connection <- function() {
    cat("GP DATA CONNECTION SUCCESSFUL!\n \n")
    require("RPostgreSQL")
    
    drv = dbDriver("PostgreSQL");
    
    db <- dbConnect(drv, dbname = "gp_practice_data", 
                  host = "localhost", port = 5432,
                  user = "postgres", password = rstudioapi::askForPassword())
}


### FUNCTION TO RUN ANY QUERY ON THE DATABASE AND RETURN DATA
get_data <- function(db = db, query){
    result <- dbGetQuery(db, query)
    return(result)
}


### FUNCTION TO CHECK GP PRACTICE ID RETURNS VALUE
check_practice <- function(input){
    query <- qq("SELECT practiceid, street, posttown, postcode, county 
                    FROM address a 
                    WHERE practiceid = '@{input}'")
    result <- get_data(db, query)
    if (nrow(result) == 0 || nrow(result) > 1) {
    cat("ERROR: That practice code is incorrect, please try again.\n \n")
    return (main())
    } 
    else { 
    print(result)
    return (result)
  }
}


### FUNCTION TO CHECK THAT MEDCINE DATA IS AVAILABLE FOR THE CHOSEN GP PRACTICE
check_med_avail <- function(id){
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
check_qof_avail <- function(id){
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
get_patient_num <- function(id, name){
    query <- qq("SELECT MAX(k.field4) 
                    FROM public.qof_achievement k 
                    WHERE orgcode = '@{id}'")
    num_patients <- get_data(db, query)
    colnames(num_patients)[which(names(num_patients) == "max")] <- "Number of Patients"
    cat("The number of patients at this practice is detailed below:\n \n")
    print(num_patients)
}


### GET THE AVERAGE SPEND PER MONTH FOR THE CHOSEN GP PRACTICE
get_practice_med_spend <- function(id) {
    query <- qq("SELECT a.practiceid, a.street, ROUND(SUM(g.actcost::decimal), 2) 
                    AS total_cost,
                    COUNT(DISTINCT g.period) AS total_periods
                    FROM gp_data_up_to_2015 g
                    INNER JOIN address a 
                    ON g.practiceid = a.practiceid
                    WHERE a.practiceid = '@{id}' 
                    AND g.period >= 201401 AND g.period < 201501
                    GROUP BY a.practiceid, a.street")
    av_spend_total <- get_data(db, query)
    av_spend_monthly_total <- av_spend_total$total_cost/av_spend_total$total_periods 
    return(av_spend_monthly_total)
}

### GET POSTCODE OUTCODE (BEGINNING)
get_outcode <- function(db = db, postcode){
    query <- qq("WITH address (postcode) AS (VALUES ('@{postcode}'))
                  SELECT SUBSTRING (postcode FROM '[^0-9]{2}[0-9]+')
                  FROM address")
    result <- get_data(db, query)
    result <- unlist(result)
    return(result)
}


### GET THE AVERAGE SPEND PER HEAD IN POSTCODE AREA
get_practice_postcode_med_spend <- function (outcode, postcode) {
    outcode <- get_outcode(db, postcode)
    query_total_sum <- qq("SELECT a.practiceid, a.street, 
                            SUM(g.actcost) AS total_cost
                            FROM gp_data_up_to_2015 g
                            INNER JOIN address a
                            ON g.practiceid = a.practiceid
                            WHERE g.period >= 201401 AND g.period < 201501 
                            AND a.postcode LIKE '@{outcode}%'
                            GROUP BY a.practiceid, a.street")
    area_sum <- get_data(db, query_total_sum)
    query_area_patient_no <- qq("SELECT a.practiceid, a.street, MAX(k.field4)
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
    get_practice_diabetes_rate <- function(id){
    query <- qq("SELECT *, (ROUND((numerator::decimal/field4::decimal), 3) * 100) 
                AS percentage_with
                FROM qof_achievement 
                WHERE indicator = 'DM001' AND orgcode = '@{id}'")
    result <- get_data(db, query)
    result <- result$percentage_with
    return(result)
}

### GET THE RATE OF DIABETES -  ALL WALES
get_wales_diabetes_rate <- function(id){
    query <- qq("SELECT *, (ROUND((numerator::decimal/field4::decimal), 3) * 100) 
                AS percentage_with 
                FROM qof_achievement k
                WHERE indicator = 'DM001' AND k.orgcode NOT IN ('@{id}')")
    result <- get_data(db, query)
    result <- round(mean(result$percentage_with), 2)
    return(result)
}

### GET DIABETES TOTAL AND INSULIN TOTAL PER PRACTICE
get_wales_diabetes_rate_ins <- function(){
    query <- qq("SELECT a.street, k.orgcode, MAX(k.numerator)
                AS total_with_diabetes, SUM(g.items) as total_insulin
                FROM qof_achievement k
                INNER JOIN address a
                ON k.orgcode = a.practiceid
                INNER JOIN gp_data_up_to_2015 g
                ON a.practiceid = g.practiceid
                WHERE k.indicator = 'DM001' AND g.bnfcode LIKE '060101%' 
                AND g.period >= 201401 AND g.period < 201501
                GROUP BY k.orgcode, a.street
                ORDER BY total_insulin")
    result <- get_data(db, query)
    return(result)
}

### GET DIABETES TOTAL AND METFORMIN TOTAL PER PRACTICE
get_wales_diabetes_rate_met <- function() {
    query <- qq("SELECT a.street, k.orgcode, MAX(k.numerator)
                AS total_with_diabetes, SUM(g.items) as total_metformin
                FROM qof_achievement k
                INNER JOIN address a
                ON k.orgcode = a.practiceid
                INNER JOIN gp_data_up_to_2015 g
                ON a.practiceid = g.practiceid
                WHERE k.indicator = 'DM001' AND g.bnfcode LIKE '0601022%' 
                AND g.period >= 201401 AND g.period < 201501
                GROUP BY k.orgcode, a.street
                ORDER BY total_metformin")
    result <- get_data(db, query)
    return(result) 
}
  
### GET TOP 500 MOST PRESCRIBED MEDICINES AT PRACTICE AND THEIR BNF CHAPTER
get_practice_top_meds <- function(id) {
    query <- qq("SELECT g.bnfcode, g.bnfname, k.chapterdesc, k.bnfsubsection,
                ROUND(SUM(g.nic::decimal), 2) AS total_cost,
                SUM(g.quantity) AS total_items
                FROM gp_data_up_to_2015 g
                INNER JOIN bnf k
                ON LEFT(k.bnfsubsection::varchar, 6) = LEFT(g.bnfcode, 6)
                WHERE g.period >= 201401 AND g.period < 201501
                AND g.bnfcode NOT LIKE '1404000H0AAAFAF%'
                AND g.practiceid = '@{id}'
                GROUP BY g.bnfcode, g.bnfname, k.chapterdesc, k.bnfsubsection
                ORDER BY total_cost DESC
                LIMIT 500")
    
    result <- get_data(db, query)
    return(result)
}

### FUNCTION TO GET MONTHLY SPENDING - ALL WALES
get_wales_monthly_spend  <- function() {
    query <- qq("SELECT g.period AS period, 
                ROUND(SUM(g.nic::decimal), 2) AS total_spend
                FROM gp_data_up_to_2015 g
                WHERE g.period >= 201401 AND g.period <= 201512
                GROUP BY g.period")
    spend <- get_data(db, query)
    spend$month <- as.numeric(str_sub(spend$period,-2,-1))
    spend$period <- strtrim(spend$period, 4)
    spend$month <- month.abb[spend$month]
    spend$month <- factor(spend$month, levels = month.abb)
    return(spend)
}

### FUNCTION TO GET RATE OF CANCER - ALL WALES
get_wales_cancer_diagnosis <- function() {
    query_cancer <- qq("SELECT a.practiceid, a.street, a.county, 
                      k.numerator AS total_cancer,
                      k.field4 AS total_patients, k.indicator
                      FROM address a
                      INNER JOIN qof_achievement k
                      ON k.orgcode = a.practiceid
                      WHERE k.indicator = 'CAN001'
                      GROUP BY a.practiceid, a.street, k.indicator,
                      k.numerator, k.field4, a.county
                      ORDER BY street")
    
    result_cancer <- get_data(db, query_cancer)
    return(result_cancer)
}

### FUNCTION TO GET RATE OF COPD - ALL WALES
get_wales_COPD_diagnosis <- function() {
    query_COPD <- qq("SELECT a.practiceid, a.street, a.county,
                      k.numerator AS total_COPD,
                      k.field4 AS total_patients, k.indicator
                      FROM address a
                      INNER JOIN qof_achievement k
                      ON k.orgcode = a.practiceid
                      WHERE k.indicator = 'COPD001'
                      GROUP BY a.practiceid, a.street, k.indicator,
                      a.county, k.numerator, k.field4
                      ORDER BY street")
  
    result_COPD <- get_data(db, query_COPD)
    return(result_COPD)
}



### FUNCTION TO GET RATE OF SMOKING - ALL WALES
get_wales_smoking <- function() {
    query_smoking <- qq("SELECT a.practiceid, a.street, a.county,
                      k.numerator AS total_smokers, 
                      k.field4, k.indicator
                      FROM address a
                      INNER JOIN qof_achievement k
                      ON k.orgcode = a.practiceid
                      WHERE k.indicator LIKE 'SMO SCR'
                      GROUP BY a.practiceid, a.street, k.indicator, k.numerator, 
                      k.field4, a.county
                      ORDER BY street")
    result_smoking <- get_data(db, query_smoking)
    return(result_smoking)
}