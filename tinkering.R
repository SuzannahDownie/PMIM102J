get_data <- function(db, query){
  result <- dbGetQuery(db, query)
  return(result)
}

postcode <- 'SA15 3AE'

get_outcode <- function(db, postcode){
  query <- qq("WITH address (postcode) AS (VALUES ('@{postcode}'))
              SELECT SUBSTRING (postcode FROM '[^0-9]{2}[0-9]+')
              FROM address")
  result <- get_data(db, query)
  result <- unlist(result)
  print(typeof(result))
  return(result)
}

get_av_spend_area <- function (db, id, postcode) {
outcode <- get_outcode(db, postcode)
query_total_sum <- qq("SELECT a.practiceid, SUM(g.actcost) AS total_cost
            FROM gp_data_up_to_2015 g
            INNER JOIN address a
            ON g.practiceid = a.practiceid
            WHERE g.period >= 201401 AND g.period < 201501 
            AND a.postcode LIKE '@{outcode}%'
            GROUP BY a.practiceid")
area_sum <- get_data(db, query_total_sum)
query_area_patient_no <- qq("  
            SELECT a.practiceid, MAX(k.field4)
            FROM address a
            INNER JOIN qof_achievement k
            ON a.practiceid = k.orgcode
            AND a.postcode LIKE '@{outcode}%'
            GROUP BY a.practiceid")
area_total_patients <- get_data(db, query_area_patient_no)
combine_data <- merge(area_sum, area_total_patients, by="practiceid")
combine_data$per_patient_spend <- combine_data$total_cost / combine_data$max
return(combine_data)
}

