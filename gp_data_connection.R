gp_data_connection <- function() {
print("GP DATA CONNECTION")
require("RPostgreSQL")
  
drv = dbDriver("PostgreSQL");
  
db <- dbConnect(drv, dbname = "gp_practice_data", 
                  host = "localhost", port = 5432,
                  user = "postgres", password = rstudioapi::askForPassword())
}