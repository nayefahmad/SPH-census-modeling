

#'--- 
#' title: "SPH LOS distribution"
#' author: "Nayef Ahmad"
#' date: "2019-06-03"
#' output: html_document
#' ---
#' 

library(odbc)
library(dbplyr)
library(magrittr) 
library(tidyverse)
library(lubridate)


# 1) set up database connection: -----------
cnx <- dbConnect(odbc::odbc(),
                 dsn = "cnx_SPDBSCSTA001")

vw_admits <- dplyr::tbl(cnx, 
                        dbplyr::in_schema("ADTCMart.ADTC", 
                                          "AdmissionDischargeView"))

df1.los <- 
  vw_admits %>% 
  filter(DischargeFacilityLongName == "St. Pauls Hospital", 
         AdjustedAdmissionDate >= '2018-01-01', 
         AdjustedDischargeDate <= '2018-12-31', 
         AccountType %in% c("Inpatient")) %>% 
  select(PatientId, 
         LOSDays) %>% 
  collect() %>% 
  count(LOSDays) 


write_csv(df1.los,
          here::here("results", 
                     "dst", 
                     "2019-06-05_sph_los-distribution-in-2018.csv"))
             

