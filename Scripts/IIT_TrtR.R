library(readr)
library(tidyverse)
library(dplyr)
library(lubridate)
library(purrr)


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
operator <- right_join(visits_data, clientlist, by ="id2") %>% 
  mutate(visit_date = coalesce(visit_date, date_art_init))
n_distinct(visits_data$id2)-n_distinct(operator$id2)#9635 client visit dates and we replaced with ART_init date 
# one more visits between July and September


analyzeFiscalYear <- function(end_fy = as.Date("2022-09-30"), operator) {
  
  #lost_null_visit <- filter(operator, map_lgl(visit_date, is.null)) # filtering anything with a Null visit date 
  
  # all Treatment numbers including LTFU 
  end_fya <- end_fy - years(1) # calculating Yr ago 
  operator2 <- filter(operator, visit_date > !!end_fya & visit_date <= !!end_fy)  %>% # selecting only appointments in the last quarter of the FY
    filter(date_art_init <= !!end_fy) %>%# Selecting only clients with initiation dates before 2022-09-30
   # filter(interval(dob, !!end_fy) / years(1) < 15) %>% 
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
  
  
  # calculate the earliest date that a "last_visit" could be for each treatment
  # such that we could detect a "lost to treatment"
  cutoff_1mo <- end_fy %m-% days(28) %m-% months(1)
  cutoff_3mo <- end_fy %m-% days(28) %m-% months(3)
  
  lost_1mo_trt <- filter(operator3, trt_months == 1) %>% 
    filter(visit_date <= cutoff_1mo) %>%
    ungroup()
  
  lost_3mo_trt <- filter(operator3, trt_months == 3) %>% 
    filter(visit_date <= cutoff_3mo) %>%
    ungroup()
  
  # select the id2's of all the clients that have been lost 
  lost_clients <- unique(rbind( select(lost_1mo_trt, id2), select(lost_3mo_trt, id2))) %>%
    mutate(lost_to_treatement = TRUE, active = FALSE)
  
  active_clients <- ungroup(operator3) %>%
    select(id2) %>%
    distinct() %>%
    anti_join(lost_clients, by = "id2") %>%
    mutate(lost_to_treatement = FALSE, active = TRUE)
  
  
  result <- rbind(lost_clients, active_clients) %>%
    mutate(fiscal_year_ending = !!end_fy)
  
  return(result)
  
}


fiscal_years       <- purrr::map(0:4, ~as.Date("2022-09-30") - years(.))
all_years_analysis <- purrr::map_df(fiscal_years, analyzeFiscalYear, operator= operator)

# id2, lost_to_treatment, active, fiscal_year_ending
#   1,             FALSE    TRUE               2019
#   1,             FALSE    TRUE               2020
#   1,             FALSE    FALSE              2021
# 50K * 4 years?
n_visits_df <- group_by(visits_data, id2) %>% summarise(n_visits_tt = n()) %>% ungroup()
analysis_df <- select(master_clientlist, id2, dob, date_art_init) %>%
  
  left_join(all_years_analysis, by = "id2") %>% 
  left_join(n_visits_df, by = "id2") %>% 
  mutate(age_at_end_of_fy = interval(dob, fiscal_year_ending) / years(1)) %>%
  mutate(aged_out = age_at_end_of_fy > 15) %>%
  mutate(new_in_fy = date_art_init > (fiscal_year_ending - years(1)))
         count(analysis_df, fiscal_year_ending, active, lost_to_treatement, aged_out, new_in_fy)
         
#  To check how many  clients that do not have visits 
         analysis_df %>% filter(is.na(lost_to_treatement)) %>% count(date_art_init) %>% pluck("date_art_init") %>% summary()
         
         library("ggplot2")
         
         analysis_df$evaluated <- !is.na(analysis_df$lost_to_treatement)
         
        
         analysis_df %>% group_by(yr = year(date_art_init), under_15 = age_at_joining < 15, evaluated) %>%
           summarize(n = n()) %>% 
           View()# joined after 15yrs 
         
         analysis_df %>% filter(year(date_art_init) == 2021 & evaluated == FALSE & age_at_joining < 15) %>% head(1) %>% View()
        
          analysis_df <- analysis_df %>%
           
          mutate(age_at_joining = interval(dob, date_art_init) / years(1))
          
          analysis_df %>%
            
            filter(evaluated == FALSE, year(date_art_init) == 2021) %>%
            group_by(yr = year(date_art_init), under_15 = age_at_joining < 15, age_at_end_of_fy >= 15) %>%
            summarize(n = n()) %>% View()
          analysis_df %>%
            filter(evaluated == FALSE, date_art_init <= "2021-04-20") %>%
            group_by(yr = year(date_art_init), under_15 = age_at_joining < 15, age_at_fy_end = interval(dob, "2021-09-30") / years(1) >= 15) %>%
            summarize(n = n()) %>% View()
          analysis_df %>% filter(year(date_art_init) == 2021 & evaluated == FALSE & age_at_joining < 15 & interval(dob, "2021-09-30") / years(1) >= 15) %>% head(1) %>% View()
         
         
# id2, lost_to_treatment,  active, fiscal_year_ending, dob, aged_out?
#   1,             FALSE    TRUE               2019           FALSE
#   1,             FALSE    TRUE               2020           FALSE
#   1,             FALSE    FALSE              2021           TRUE
# 50K * 4 years?