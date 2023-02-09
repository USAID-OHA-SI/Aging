rm(list=ls())

#Topic: Interagency Ageing-up analysis
#Author: Lydia Odero
#Date: Feb 6, 2023

library(readr)
library(tidyverse)
library(dplyr)
library(lubridate)


setwd("~/Work Directory/2. HIV_TB/Studies/Aging up Analysis/Interagency work/Updated data_03FEB2023")


# Client List Data Import and Management ----------------------------------------------
#The client list
clientlist <- read_csv("ClientList.csv")
str(clientlist)
dim(clientlist)

#Renaming the variables and creating id2 based on id and facility
clientlist <- clientlist %>% rename(im=`Implementing Partner`, facility=`Facility Name`, mfl_code=`Facility MFL Code`,
                                    county=`County of Treatment`,id=`Client Number(De-identified)`, sex=Sex, orphan=Orphan, 
                                    residence=`County of Residence`,dob=DOB, date_art_init=`ART Initiation Date`, 
                                    reg_art_init=`ART Regimen at Initiation`,line_art_init=`ART  Regimen Line at Initiation`, 
                                    reg_art_current=`Current ART Regimen`,line_art_current=`Current ART Regimen Line`) %>% 
                             mutate(id2 =  paste0(mfl_code, "_", id))
               
                                      
#Creating distinct clients based on ids
clientlist %>% distinct(id) #91,937 distinct clients based on client ids only from the 242,377 in the original dataset
clientlist %>% distinct(facility, id) #97,745 distinct clients based on client id and facility 
clientlist %>% distinct(mfl_code, id) #97,745 distinct clients based on client id and facility mfl code 
ref_clientlist <- clientlist %>% distinct(facility, mfl_code, id) #97,745 distinct clients based on client id and facility mfl code
sum(duplicated(ref_clientlist))

#Creating master client list based on facility, mfl code, county, id, sex, dob, ART initiation date, ART regimen at initiation and current ART regimen
#Leaving out the implementing partner, orphan, county of residence and the ART regimen lined both at initiation and current

master_clientlist <- clientlist %>% distinct(id, sex, dob, facility, mfl_code, county,date_art_init, reg_art_init, reg_art_current, id2)

#Checking the difference between the master and the reference lists
occurrence <- data.frame(table(master_clientlist$id2)) %>% rename(id2=Var1)

master_clientlist <- master_clientlist %>% left_join(occurrence, by="id2") 

master_clientlist %>% filter(Freq>1) #25 records

#Using the master_clientlist for subsequent analysis

#Categorizing period of ART initiation

master_clientlist$date_art_init <- ymd(master_clientlist$date_art_init)
master_clientlist$dob <- ymd_hms(master_clientlist$dob)
str(master_clientlist)

master_clientlist <- master_clientlist %>% mutate(
                          art_init_period = case_when(
                            date_art_init >= "2018-10-01" & date_art_init <= "2019-09-30"  ~ "FY19",
                            date_art_init >= "2019-10-01" & date_art_init <= "2020-09-30"  ~ "FY20",
                            date_art_init >= "2020-10-01" & date_art_init <= "2021-09-30"  ~ "FY21",
                            date_art_init >= "2021-10-01" & date_art_init <= "2022-09-30"  ~ "FY22",
                            date_art_init >= "2022-10-01" & date_art_init <= "2023-09-30"  ~ "FY23", 
                            TRUE ~ "Other Years") )

#Calculating age at ART initiation
master_clientlist <- master_clientlist %>% filter(!(is.na(date_art_init)) & !(is.na(dob)))
master_clientlist <- master_clientlist %>% mutate(
                            age_at_art_init = as.integer(difftime(date_art_init, dob, unit = "weeks")/52.25),
                            age_group_art_init = if_else(age_at_art_init <15, "<15 years", "15+ years"))
                            
master_clientlist %>% group_by(art_init_period, age_group_art_init) %>% summarize(clients = n())


# Appointments & Visits Data Import and Management ----------------------------------------------
visits_data <- read_csv("Appointments.csv")

#Renaming the variables and creating id2 based on id and facility
visits_data <- visits_data %>% rename(im=`Implementing Partner`, facility=`Facility Name`, mfl_code=`Facility MFL Code`,
                                    id=`Client Number(De-identified)`, appoint_date=`Appointment Date`, 
                                    visit_date=`Actual Visit Date`) %>% 
                               mutate(id2 =  paste0(mfl_code, "_", id))

#Only working with clients who are in the master client list
#Limitation: We'll need to drop duplicates based on id2









                              