

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
                 dsn = "cnx_denodo_spappcsta001")

vw_admits <- dplyr::tbl(cnx, 
                        dbplyr::in_schema("publish", 
                                          "admission_discharge"))

df1.los <- 
  vw_admits %>% 
  filter(facility_name == "St. Pauls Hospital", 
         admission_date_id >= '20180101', 
         admission_date_id <= '20181231', 
         encntr_type_class_grp_at_ad == "Inpatient") %>% 
  select(patient_id, 
         admit_to_disch_los_elapsed_time_days) %>% 
  collect() %>% 
  count(admit_to_disch_los_elapsed_time_days) 


write_csv(df1.los,
          here::here("results", 
                     "dst", 
                     "2019-06-03_sph_los-distribution-in-2018.csv"))
             

