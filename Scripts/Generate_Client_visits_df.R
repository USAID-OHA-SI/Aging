library("tidyverse")

#read in data
visits_data <- read_csv("Data/Appointments.csv", na = c("", "NA", "NULL"))
master_clientlist <- read_csv("Dataout/master_clientlist.csv") #created from Scripts/Generate_master_clientlist.R

#Renaming the variables and creating id2 based on id and facility
visits_data <- visits_data %>% rename(im=`Implementing Partner`, facility=`Facility Name`, mfl_code=`Facility MFL Code`,
                                      id=`Client Number(De-identified)`, appoint_date=`Appointment Date`, 
                                      visit_date=`Actual Visit Date`) %>% 
  mutate(id2 =  paste0(mfl_code, "_", id)) 

#remove any visits that don't have a future appointment or an invalid date
visits_data <-  visits_data %>%  
  tidylog::filter(!is.na(appoint_date), appoint_date != "01/01/1900")

# change the dates  from character to date format
visits_data <- visits_data %>% 
  mutate(across(c(visit_date, appoint_date), \(x) dmy(x)))

#remove any observations where the future appointment data is less than the visit date
visits_data <- visits_data %>% 
  tidylog::filter(appoint_date >= visit_date)

#identify the latest visit date in dataset (filter dataset down later)
max_appt <- max(visits_data$visit_date, na.rm = TRUE)

# remove duplicate entries
clean_visits_data <- distinct(visits_data) 

#Only working with clients who are in the master client list
#Limitation: We'll need to drop duplicates based on id2
# check for duplicates  based on id2

clean_visits_data %>% distinct(id2) %>% nrow() #88,110 distinct clients based on 1,800810 observations in the visits_dataset

#add in the next visit date for calculating duration between appointments
clean_visits_data <- clean_visits_data %>% 
  arrange(id2, visit_date) %>% 
  group_by(id2) %>% 
  mutate(next_visit_date = lead(visit_date, order_by = visit_date)) %>% 
  ungroup()

#reorder columns for ease of review
clean_visits_data <- clean_visits_data %>% 
  relocate(id2, .after = id) %>% 
  relocate(visit_date, .before = appoint_date)

#calculation duration between visits (proxy = 28 days after planned gap)
clean_visits_data <- clean_visits_data %>% 
  mutate(visit_gap_planned = appoint_date - visit_date,
         visit_gap_allowed = appoint_date + days(28) - visit_date,
         visit_gap_actual = next_visit_date - visit_date) 

#ltfu status
clean_visits_data <- clean_visits_data %>% 
  mutate(ltfu = (is.na(next_visit_date) & appoint_date <= max_appt) | (visit_gap_actual > visit_gap_allowed & appoint_date <= max_appt),
         rtt = case_when(ltfu == TRUE & is.na(next_visit_date) ~ FALSE,
                         ltfu & !is.na(next_visit_date) ~ TRUE))

#create type for adding in different datesets
clean_visits_data <- clean_visits_data %>% 
  mutate(type = "Visit",
         date = visit_date)

#add in LTFU date as row (will be future period ahead of last visit)
date_ltfu <- clean_visits_data %>%
  filter(ltfu == TRUE) %>%
  mutate(date = visit_date + visit_gap_allowed + days(1),
         appoint_date = NA,
         next_visit_date = NA,
         visit_gap_planned = NA,
         visit_gap_actual = NA,
         type = ifelse(rtt == FALSE, "LTFU", "LTFU (-> RTT)")) %>% 
  filter(date <= max(clean_visits_data$visit_date))

#add ageout date as row
date_ageout <- master_clientlist %>% 
  select(id, id2, facility, mfl_code, age_out) %>% 
  filter(age_out <= max(clean_visits_data$visit_date)) %>% 
  mutate(date = age_out,
         type = "Aged Out") 

#merge on age data 
clean_visits_data <- clean_visits_data %>% 
  left_join(master_clientlist,
            by = join_by(id, id2, facility, mfl_code))

#remove patients 15+ years old
clean_visits_data <- clean_visits_data %>% 
  filter(visit_date < age_out)

#bind date of ltfu date + age out date back onto dataset
binded_data <- clean_visits_data %>% 
  bind_rows(date_ltfu, date_ageout) %>%
  arrange(id2, date)
  
#apply fiscal quarter
binded_data <- binded_data %>% 
  mutate(period = date %>% 
           quarter(with_year = TRUE, fiscal_start = 10) %>%
           str_replace("20", "FY") %>% 
           str_replace("\\.", "Q"))

#filter by the max id per period to capture the status
status_data <- binded_data %>% 
  group_by(id2, period) %>% 
  filter(date == max(date)) %>% 
  ungroup() %>% 
  mutate(status = ifelse(type == "Visit", "Active", type))

#identify new
status_new <- master_clientlist %>% 
  mutate(period = date_art_init %>% 
           quarter(with_year = TRUE, fiscal_start = 10) %>%
           str_replace("20", "FY") %>% 
           str_replace("\\.", "Q"),
         is_new = TRUE) %>%
  filter(between(date_art_init, min(clean_visits_data$visit_date, na.rm = TRUE), max(clean_visits_data$visit_date, na.rm = TRUE))) %>% 
  select(id2, period, is_new)

#merge on new initations
status_data <- status_data %>% 
  tidylog::full_join(status_new) %>% 
  mutate(is_new = ifelse(is.na(is_new), FALSE, is_new),
         status = ifelse(is_new == TRUE, "New", status))


#agg table of status by period
status_data %>% 
  count(period, status) %>% 
  pivot_wider(names_from = status,
              values_from = n) %>% 
  relocate(New, `LTFU (-> RTT)`, .after = "Active")






