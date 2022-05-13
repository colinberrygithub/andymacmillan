#' eb_number
#'
#' This function loads the census data around class, dices by ward
#'calculates based on information provided from lucid talk for unionists
#' @return eb_predictor output for UUP
#' @export
library(dplyr)
library(magrittr)

eb_number <- function(LTABC, LTCTWO, abcweight,ctwodeweight){
  

eb_ds <- read.csv('Dataset_3004_2011_1_.csv')
  
  eb_ds <- eb_ds %>% 
    filter(eb_ds$Ward %in% c('Ballyhackamore', 'Ballymacarrett', 'Belmont', 'Bloomfield (Belfast LGD)', 'Cherryvalley', 'Island', 'Knock', 'Orangefield', 'Stormont', 'Sydenham','The Mount', 'Ballyhanwood', 'Carrowreagh', 'Cregagh', 'Downshire', 'Dundonald', 'Enler', "Gilnahirk", "Graham's Bridge", 'Lisnasharragh', 'Lower Braniel', 'Tullycarnet','Upper Braniel'))
  
  eb_ds$abcone <- eb_ds$`Approximated social grade of HRP aged 16-64 years: AB` + eb_ds$`Approximated social grade of HRP aged 16-64 years: C1`
  eb_ds$ctwode <- eb_ds$`Approximated social grade of HRP aged 16-64 years: C2` + eb_ds$`Approximated social grade of HRP aged 16-64 years: DE`
  
  #lucidtalk numbers
  LTABC = LTABC
  LTCTWO = LTCTWO
  
  eb_ds$adbc_unweighted <- eb_ds$abcone * LTABC
  eb_ds$ctwode_unweighted <- eb_ds$ctwode * LTCTWO
  #adjusted census 2011 with no exponetial growth
  Religion <- c("None", "Catholic", "Protestant", "Other")
  Number <- c(0.22, 0.41, 0.37, 0.01)
  
  demographic <- cbind(Religion, Number)
  demographic <- as.data.frame(demographic)
  #assembly info demogaphic eb with no exponetial growth
  eb_religion <- c("None", "Catholic", "Protestant", "Other")
  eb_number <- c(0.12, 0.13, 0.72,0.03)
  
  eb_demographic <- cbind(demographic, eb_number) 
  eb_demographic$Number <- as.numeric(eb_demographic$Number)
  eb_demographic$ratio <- eb_demographic$eb_number/eb_demographic$Number 
  
  eb_ds$weighted_abc <- ((eb_ds$adbc_unweighted*0.12)*0.545) + 
    ((eb_ds$adbc_unweighted*0.13)*0.317) +
    ((eb_ds$adbc_unweighted*0.72)*1.945) +
    ((eb_ds$adbc_unweighted*0.03)*3)  
  
  eb_ds$weighted_ctwode <- ((eb_ds$ctwode_unweighted*0.12)*0.545) + 
    ((eb_ds$ctwode_unweighted*0.13)*0.317) +
    ((eb_ds$ctwode_unweighted*0.72)*1.945) +
    ((eb_ds$ctwode_unweighted*0.03)*3)  
  
  eb_ds$support_by_ward_raw <- (eb_ds$weighted_abc*abcweight) + (eb_ds$weighted_ctwode*ctwodeweight)
  
  eb_ds$percentage_ward <- eb_ds$support_by_ward_raw / (eb_ds$abcone + eb_ds$ctwode)
  
  eb_ds[3] <- NULL
  
  return(eb_ds)}
