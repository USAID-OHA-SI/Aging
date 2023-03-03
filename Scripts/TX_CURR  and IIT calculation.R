library(readr)
library(tidyverse)
library(dplyr)
library(lubridate)
library(purrr)


end_fy <- as.Date("2022-09-30") 
setwd("~/Data - 03FEB2023")


# Appointments & Visits Data Import and Management ----------------------------------------------
visits_data <- read_csv("Appointments.csv")

#Renaming the variables and creating id2 based on id and facility
visits_data <- visits_data %>% rename(im=`Implementing Partner`, facility=`Facility Name`, mfl_code=`Facility MFL Code`,
                                      id=`Client Number(De-identified)`, appoint_date=`Appointment Date`, 
                                      visit_date=`Actual Visit Date`) %>% 
  mutate(id2 =  paste0(mfl_code, "_", id))

# change the dates  from character to date format

visits_data$visit_date <- as.Date(visits_data$visit_date, format = "%d/%m/%Y")
visits_data$appoint_date <- as.Date(visits_data$appoint_date, format = "%d/%m/%Y")

#Only working with clients who are in the master client list
#Limitation: We'll need to drop duplicates based on id2
# check for duplicates  based on id2 

visits_data %>% distinct(id2) #88,100 distinct clients based on 1,800810 observations in the visits_dataset
# extracting  Id2  from the visits and the master client list to match 

# visits_id <- visits_data[, "Visit_id2"]
# 
# unique_visit_id <- visits_id[!duplicated(visits_id),]# remove duplicates and create a new df_unique
# 
# 
# # extract  client ID from the client list 
# 
# masterclient_id <- master_clientlist[, "id2"] 
# 
# # match the 88100  clients from the visits list to the 97758 clients on the master list
# 
# Id_merged <- merge(unique_visit_id, masterclient_id, by = c("id2"))

#visits_data[["visit_id"]] <- row_number(visits_data)



 # create masterlist_2 with  client and visit data 
operator <- inner_join(visits_data, clientlist,by ="id2")
n_distinct(visits_data$id2)-n_distinct(operator$id2)#0  verifying id2 is a distinct identifier
# one more visits between July and September



#lost_null_visit <- filter(operator, map_lgl(visit_date, is.null)) # filtering anything with a Null visit date 

# all Treatment numbers including LTFU 
end_fya <- end_fy - years(1) # calculating Yr ago 
operator2 <- filter(operator, visit_date > !!end_fya & visit_date <= !!end_fy)  %>% # selecting only appointments in the last quarter of the FY
  filter(date_art_init <= !!end_fy) %>%# Selecting only clients with initiation dates before 2022-09-30
  filter(interval(dob, !!end_fy) / years(1) < 15) %>% 
  select(id2) %>%
  inner_join(operator, by = "id2") %>% #  Merging back with the operator dataset to get their actual visits 
  select(id2,visit_date, reg_art_current) %>%
  filter(visit_date <= !!end_fy) %>% 
  group_by(id2, reg_art_current) %>%
  filter(visit_date == max(visit_date)) %>% 
  distinct() %>% ## Steps A and B
  ungroup()


 

operator2$trt_months <- 1 #  defaulting clients based on  regimen
operator2[grepl("^\\(?3", operator2$reg_art_current), ]$trt_months <- 3
operator2$next_expected_visit <- operator2$visit_date %m+% months(operator2$trt_months)
operator2$lost_date <- operator2$next_expected_visit %m+% days(28)

# check to see if we have multiple observations for the same client
duplicates_check<-group_by(operator2, id2) %>%
  
  summarise(n = n()) %>%
  filter(n > 1) %>%
  collect() %>%
  ungroup() %>%
  inner_join(operator2, by = "id2")

# since the regimen is similar we will deduplicate  and create  operator 3 with all the clients plus LTFU

 if  (nrow(duplicates_check) > 0) {
   
   inconsistent_trt <- group_by(duplicates_check, id2) %>%
     filter(n_distinct(trt_months) > 1) %>%
     ungroup() %>%
     collect()
   operator3 <- anti_join(operator2, inconsistent_trt, by = "id2") %>%
     select(id2, trt_months, visit_date) %>%
     distinct()
 } else {
   operator3 <- select(operator2, id2, trt_months, visit_date) %>%
     distinct()
 }

  
table(operator3$ trt_months)

# operator 2 has ALL visits up till 2023
# identifying clients  LFT  based on last visit  date  and 28 days after their last appointment date 
 

# for trt with 3 month followup
cutoff_1mo <- end_fy %m-% days(28) %m-% months(1)
cutoff_3mo <- end_fy %m-% days(28) %m-% months(3)

lost_1mo_trt <- filter(operator3, trt_months == 1) %>% 
  filter(visit_date <= cutoff_1mo) %>%
  ungroup()
lost_3mo_trt <- filter(operator3, trt_months == 3) %>% 
  filter(visit_date <= cutoff_3mo) %>%
  ungroup()

# select the id2's of all the clients that have been lost 
lost_clients <- unique(rbind( select(lost_1mo_trt, id2), select(lost_3mo_trt, id2)))

active_clients <- ungroup(operator3) %>%
  select(id2) %>%
  distinct() %>%
  anti_join(lost_clients, by = "id2")

#nrow(operator2)-nrow(active_clients)

#select(operator2, id2) %>% n_distinct()


# operator 2 has ALL visits up till 2023

# for treatment with 1-3 month followup
cutoff_1mo <- end_fy %m-% days(28) %m-% months(1)
cutoff_3mo <- end_fy %m-% days(28) %m-% months(3)

# calculating  LTFU for patients on one and 3 months of treatment
lost_1mo_trt <- filter(operator3, trt_months == 1) %>% 
  filter(visit_date <= cutoff_1mo)
lost_3mo_trt <- filter(operator3, trt_months == 3) %>% 
  filter(visit_date <= cutoff_3mo)

# add the numbers of LTFU 

lost_clients <- rbind(lost_1mo_trt, lost_3mo_trt)
  
  




