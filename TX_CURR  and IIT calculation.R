library(readr)
library(tidyverse)
library(dplyr)
library(lubridate)
library(purrr)

setwd("~/Data - 03FEB2023")

visits_data <- read_csv("Appointments.csv")

#Renaming the variables and creating id2 based on id and facility
visits_data <- visits_data %>% rename(im=`Implementing Partner`, facility=`Facility Name`, mfl_code=`Facility MFL Code`,
                                      id=`Client Number(De-identified)`, appoint_date=`Appointment Date`, 
                                      visit_date=`Actual Visit Date`) %>% 
  mutate(Visit_id2 =  paste0(mfl_code, "_", id))

# change the dates  from character to date 

visits_data$visit_date <- as.Date(visits_data$visit_date, format = "%d/%m/%y")
visits_data$appoint_date <- as.Date(visits_data$appoint_date, format = "%d/%m/%y")

#Only working with clients who are in the master client list
#Limitation: We'll need to drop duplicates based on id2
# check for duplicates  based on id2 

visits_data %>% distinct(Visit_id2) #88,100 distinct clients based on 1,800810 observations in the visits_dataset
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
operator <- inner_join(visits_data, clientlist,by ="id2")
n_distinct(visits_data$id2)-n_distinct(operator$id2)#0 
# one more visits between July and September



lost_null_visit <- filter(operator, map_lgl(visit_date, is.null))

operator2 <- filter(operator, !map_lgl(visit_date, is.null)) %>%
  filter(month(visit_date) <= 9 & month(visit_date) >= 7) %>%
  filter(date_art_init <= "2022-09-30") %>%
  select(id2) %>%
  inner_join(operator, by = "id2") %>%
  select(id2,visit_date, reg_art_current) %>%
  group_by(id2, reg_art_current) %>%
  filter(visit_date == max(visit_date))

operator2$visit_date <- as.Date(operator2$visit_date, format = "%d/%m/%Y")
operator2$trt_months <- 1
operator2[grepl("^\\(?3", operator2$reg_art_current), ]$trt_months <- 3
operator2$next_expected_visit <- operator2$visit_date %m+% months(operator2$trt_months)
operator2$lost_date <- operator2$next_expected_visit %m+% days(28)


# operator 2 has ALL visits up till 2023
end_fy <- as.Date("2022-09-30")

# for trt with 3 month followup
cutoff_1mo <- end_fy %m-% days(28) %m-% months(1)
cutoff_3mo <- end_fy %m-% days(28) %m-% months(3)

lost_1mo_trt <- filter(operator2, trt_months == 1) %>% 
  filter(visit_date <= cutoff_1mo) %>%
  ungroup()
lost_3mo_trt <- filter(operator2, trt_months == 3) %>% 
  filter(visit_date <= cutoff_3mo) %>%
  ungroup()

# select the id2's of all the clients that have ven lost 
lost_clients <- unique(rbind( select(lost_1mo_trt, id2), select(lost_3mo_trt, id2), select(lost_null_visit, id2)))

active_clients <- ungroup(operator2) %>%
  select(id2) %>%
  distinct() %>%
  anti_join(lost_clients, by = "id2")

# operator 2 has ALL visits up till 2023
end_fy <- as.Date("2022-09-30")

# for trt with 3 month followup
cutoff_1mo <- end_fy %m-% days(28) %m-% months(1)
cutoff_3mo <- end_fy %m-% days(28) %m-% months(3)

lost_1mo_trt <- filter(operator2, trt_months == 1) %>% 
  filter(visit_date <= cutoff_1mo)
lost_3mo_trt <- filter(operator2, trt_months == 3) %>% 
  filter(visit_date <= cutoff_3mo)

lost_clients <- rbind(lost_1mo_trt, lost_3mo_trt)
  
  
  
group_by(operator,id2,visit_date) %>% summarise(n=n()) %>% 
  filter(n>1) %>% summarise(n=n())




