


#********************************************
# IMPORT DATA FOR LGH SURGERY SCHEDULE SMOOTHING 
#********************************************
# 2018-08-23
# Nayef Ahmad 

library("tidyverse") 
library("here")
library("reshape2")
library("magrittr")
library("glue")

# rm(list = ls())

# todo: ---------------


# source functions: 
source(here("src", 
            "01_helper-functions.R"))


## Data setup for pre-intervention period: ------------------------
# > import weekly schedule for pre-intervention period: ---------
options(readr.default_locale=readr::locale(tz="America/Los_Angeles"))

df1.pre.week.schedule <- 
      read_csv(here("data", 
                    as.character(glue({input.schedule.1}))))  # input.schedule is the filename is from the master file

str(df1.pre.week.schedule)      



# > generate full schedule dataset for pre-intervention period: ----------------
# how many weeks? 
numweeks <- numweeks.param  # todo: if you haven't run the master file yet, just assign a value: numweeks.param <- 12
# how many surgery types in input data? 
num.surg.types <- unique(df1.pre.week.schedule$surgtype) %>% length



# generate schedule for pre-intervention period: 
df2.pre.full.input.schedule <- repeat.rows(df1.pre.week.schedule, 
                                       numweeks) %>% 
      mutate(day.number = lapply(1:(7*numweeks), 
                                 rep, 
                                 each = num.surg.types) %>% unlist, 
             num.sda = as.character(num.sda) %>% as.integer)

str(df2.pre.full.input.schedule)




#*******************************************************************************
## Data setup for post-intervention period: ------------------------
# > import weekly schedule for pOst-intervention period: ---------
options(readr.default_locale=readr::locale(tz="America/Los_Angeles"))

df3.post.week.schedule <- 
   read_csv(here("data", 
                 as.character(glue({input.schedule.2}))))  # input.schedule is the filename is from the master file

str(df3.post.week.schedule)      



# > generate full schedule dataset for pre-intervention period: ----------------
# how many weeks? 
numweeks <- numweeks.param  # todo: if you haven't run the master file yet, just assign a value: numweeks.param <- 12
# how many surgery types in input data? 
num.surg.types <- unique(df3.post.week.schedule$surgtype) %>% length



# generate schedule for pre-intervention period: 
df4.post.full.input.schedule <- repeat.rows(df3.post.week.schedule, 
                                            numweeks) %>% 
   mutate(day.number = lapply(1:(7*numweeks), 
                              rep, 
                              each = num.surg.types) %>% unlist, 
          num.sda = as.character(num.sda) %>% as.integer)

str(df4.post.full.input.schedule)




# join pre- and post-intervention schedules into a single dataframe: -------
df5.full.input.schedule <- 
   df2.pre.full.input.schedule %>% 
   bind_rows(df4.post.full.input.schedule)

str(df5.full.input.schedule)



# import probability distributions of LOS in surgery units: ------
df6.los.distributions.raw <- 
      read_csv(here("data", 
                    "surgery-LOS-by-surgery-type.csv")) 

# isolate los column: 
los <- df6.los.distributions.raw[, 1]
      
# set up prob distributions in long format: 
df7.los.distributions <- 
      df6.los.distributions.raw %>% 
      melt() %>% 
      filter(variable != "LOSDays") %>% 
      mutate(losdays = rep(los, num.surg.types) %>% 
                   unname %>% 
                   unlist) %>% 
      droplevels()


# df3.post.los.distributions
str(df7.los.distributions)




# generate days of week: ----------
days <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
days.vec <- factor(rep(days, numweeks.param), 
                   levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))




#******************************************************************************
# write output: ------------
write_csv(df5.full.input.schedule,
          here("results",
               "dst",
               "full-schedule-from-input-data.csv"))



