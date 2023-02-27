library(tidyverse)


# Client List Data Import and Management ----------------------------------------------
#The client list
clientlist <- read_csv("Data/AgingOut_Feb162023.csv")
str(clientlist)
dim(clientlist)

#Renaming the variables and creating id2 based on id and facility( use aging out data from palladiaum)
clientlist <- clientlist %>% rename(im=`Implementing Partner`, facility=`Facility Name`, mfl_code=`Facility MFL Code`,
                                    county=`County of Treatment`,id=`Client Number(De-identified)`, sex=Sex, orphan=Orphan, 
                                    residence=`County of Residence`,dob=DOB, date_art_init=`ART Initiation Date`, 
                                    reg_art_init=`ART Regimen at Initiation`,line_art_init=`ART  Regimen Line at Initiation`, 
                                    reg_art_current=`Current ART Regimen`,line_art_current=`Current ART Regimen Line`) %>% 
  mutate(id2 =  paste0(mfl_code, "_", id))

#conver DOB to date
clientlist <- clientlist %>% 
  mutate(dob = as_date(dob) %>% ymd)

#Creating distinct clients based on ids
clientlist %>% distinct(id) #91,927 distinct clients based on client ids only from the 242,377 in the original dataset
clientlist %>% distinct(facility, id) #97,735 distinct clients based on client id and facility 
clientlist %>% distinct(mfl_code, id) #97,735 distinct clients based on client id and facility mfl code 
ref_clientlist <- clientlist %>% distinct(facility, mfl_code, id) #97,745 distinct clients based on client id and facility mfl code
sum(duplicated(ref_clientlist))

#Creating master client list based on facility, mfl code, county, id, sex, dob, ART initiation date, ART regimen at initiation and current ART regimen
#Leaving out the implementing partner, orphan, county of residence and the ART regimen lined both at initiation and current

master_clientlist <- clientlist %>% distinct(id, sex, dob, facility, mfl_code, county,date_art_init, reg_art_current, id2)

# Checking the duplicates in 'master_clientlist' # 12 ids ( but 25  observations)in the mater clientlist are duplicates 
dupl = names(which(table(master_clientlist$id2)>1))
master_clientlist<-filter(master_clientlist, !id2 %in% dupl) 

#Using the master_clientlist for subsequent analysis

#Categorizing period of ART initiation

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
  age_group_art_init = if_else(age_at_art_init < 15, "<15 years", "15+ years"))

master_clientlist %>% group_by(art_init_period, age_group_art_init) %>% summarize(clients = n())

# Removing the people that is more than 15 yo
master_clientlist <- master_clientlist %>% 
  filter(age_group_art_init != "15+ years") %>% 
  select(-age_group_art_init) %>% 
  mutate(age_out = dob %m+% years(15),
         age_out_yr = case_when(
           age_out >= ymd("2019-10-01") & age_out <= ymd("2020-09-30")  ~ "2019",
           age_out >= ymd("2020-10-01") & age_out <= ymd("2021-09-30")  ~ "2020",
           age_out >= ymd("2021-10-01") & age_out <= ymd("2022-09-30")  ~ "2021",
           age_out >= ymd("2022-10-01") & age_out <= ymd("2023-09-30")  ~ "2022",
           age_out >= ymd("2023-10-01") & age_out <= ymd("2024-09-30")  ~ "2023",
           TRUE ~ "Other Years") )

# How many persons age-up in each year? 
master_clientlist %>% 
  group_by(age_out_yr) %>% 
  summarise(n = n())

