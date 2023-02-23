library("tidyverse")
library("data.table")
library("purrr")
library("lubridate")
visits_data <- read_csv("Appointments.csv")

#Renaming the variables and creating id2 based on id and facility
visits_data <- visits_data %>% rename(im=`Implementing Partner`, facility=`Facility Name`, mfl_code=`Facility MFL Code`,
                                      id=`Client Number(De-identified)`, appoint_date=`Appointment Date`, 
                                      visit_date=`Actual Visit Date`) %>% 
  mutate(id2 =  paste0(mfl_code, "_", id)) %>% 
  filter(!is.na(appoint_date) & !is.na(visit_date))

setDT(visits_data)

# change the dates  from character to date format

visits_data$visit_date <- as.Date(visits_data$visit_date, format = "%d/%m/%Y")
visits_data$appoint_date <- as.Date(visits_data$appoint_date, format = "%d/%m/%Y")

# Correcting '1900-01-01'
visits_data$appoint_date[which(visits_data$appoint_date == "1900-01-01")] = NA

# remove any visit with a NA appoint_date or NA visit_date and if duplicate
# visits appear (2+), only take the first occurrence (1L)
clean_visits_data <- visits_data[
  i = !is.na(appoint_date) & !is.na(visit_date),
  j = .SD[1L],
  by = .(id2, appoint_date, visit_date)]

#clean_visits_data[, diff_date := visit_date - appoint_date]
#mutate(correct_visit_data = lead(visit_date, 1))

# Checking one person
#xx = clean_visits_data[visits_data$id == "773D4AB8B838FFD025CA2E12C1CAB91DD616402F7AB5DA57BA947206319C1B01",]
#ggplot(xx) +
#  aes(x = appoint_date, y = visit_date) +
#  geom_line()

#clean_visits_data[, diff_days := as.numeric(difftime(appoint_date, visit_date, units = "days"))]

# should we keep the  visits with no appointments because the clinets have made contact

#Only working with clients who are in the master client list
#Limitation: We'll need to drop duplicates based on id2
# check for duplicates  based on id2

clean_visits_data %>% distinct(id2) #88,110 distinct clients based on 1,800810 observations in the visits_dataset
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
operator <- merge(clean_visits_data, master_clientlist, by ="id2", all.y = TRUE)

#data table will be sorted in ascending order based on the values in the id2 column.
setkey(operator, id2)

n_distinct(visits_data$id2)-n_distinct(operator$id2)#9635 client visit dates and we replaced with ART_init date
# one more visits between July and September

analyzeFiscalYear <- function(end_fy = as.Date("2022-09-30"), operator) {
  
  #lost_null_visit <- filter(operator, map_lgl(visit_date, is.null)) # filtering anything with a Null visit date
  
  # all Treatment numbers including LTFU
  end_fya <- end_fy - years(1) # calculating Yr ago
  operator_visits <- operator[visit_date > end_fya & visit_date <= end_fy] %>% # selecting only appointments in the last quarter of the FY
    .[date_art_init <= end_fy] %>%# Selecting only clients with initiation dates before 2022-09-30
    # filter(interval(dob, !!end_fy) / years(1) < 15) %>%
    .[, .(id2)] %>%
    unique() %>% 
    .[operator, nomatch = 0L] %>% #  Merging back with the operator dataset to get their actual visits
    .[, .(id2, visit_date, reg_art_current)] %>%
    .[visit_date <= end_fy]
  
  
    operator2 <- operator_visits[, .SD[which.max(visit_date)], by = .(id2)]
 
  operator2$trt_months <- 1 #  defaulting clients based on  regimen
  operator2[grepl("^\\(?3", operator2$reg_art_current)]$trt_months <- 3
  operator2$next_expected_visit <- operator2$visit_date %m+% months(operator2$trt_months)
  operator2$lost_date <- operator2$next_expected_visit %m+% days(28)
  
  # check to see if we have multiple observations for the same client
  setkey(operator2, id2)
  duplicates_check <- operator2[, .N, by = id2][N > 1][, .(id2)][operator2, nomatch = 0L]
  
  # since the regimen is similar we will deduplicate  and create  operator 3 with all the clients plus LTFU
  
  if  (nrow(duplicates_check) > 0) {
    
    inconsistent_trt <- group_by(duplicates_check, id2) %>%
      filter(n_distinct(trt_months) > 1) %>%
      ungroup() %>%
      collect()
    operator3 <- anti_join(operator2, inconsistent_trt, by = "id2") %>%
      select(id2, trt_months, visit_date) %>%
      distinct()
    
    setDT(operator3 )
  } else {
    operator3 <- operator2[, .(id2, trt_months, visit_date)] %>%
      unique()
  }
  
  
  # calculate the earliest date that a "last_visit" could be for each treatment
  # such that we could detect a "lost to treatment"
  cutoff_1mo <- end_fy %m-% days(28) %m-% months(1)
  cutoff_3mo <- end_fy %m-% days(28) %m-% months(3)
  
  lost_1mo_trt <- operator3[trt_months == 1 & visit_date <= cutoff_1mo]
  lost_3mo_trt <- operator3[trt_months == 3 & visit_date <= cutoff_3mo]
  
  # select the id2's of all the clients that have been lost
  lost_clients <- unique(rbind(lost_1mo_trt[, .(id2)], lost_3mo_trt[, .(id2)]))
  lost_clients[, `:=`(lost_to_treatement = TRUE, active = FALSE)]
  
  setkey(operator3, id2)
  active_clients <- operator3[!lost_clients][, .(id2)]
  active_clients[, `:=`(lost_to_treatement = FALSE, active = TRUE)]
  
  result <- rbind(lost_clients, active_clients)
  set(result, i = NULL, j = "fiscal_year_ending", value = end_fy)
  
  return(result)
  
}


fiscal_years       <- purrr::map(0:5, ~as.Date("2022-09-30") - years(.))
all_years_analysis <- purrr::map(fiscal_years, analyzeFiscalYear, operator= operator) %>% rbindlist()

all_yrs_analysis_wide<-dcast.data.table(all_years_analysis, id2 ~ fiscal_year_ending, value.var = "active")



# Merging visits data and  master client list 
client_visits_df <- merge(master_clientlist,all_yrs_analysis_wide, by = "id2")

## Check if the merged data has the same number of rows as the original data frames

if (nrow(client_visits_df) == (nrow(master_clientlist) + nrow(all_yrs_analysis_wide) - length(intersect(master_clientlist$id2, all_yrs_analysis_wide$id2)))) {
  print("Merged data has the same number of rows as the original data frames.")
} else {
  print("Merged data does not have the same number of rows as the original data frames.")
}

# we determine that the merged data does not have the same number of rows
#we extract id2 from both dataframes  to match them and determine inconsistencies 

id1 <- master_clientlist$id2
id2 <- all_years_analysis$id2

# Match the IDs to see if they are identical
if (all(id1 %in% id2) && all(id2 %in% id1)) {
  print("The IDs are identical.")
} else {
  print("The IDs are not identical.")
}

# Find the IDs that are not identical
diff_ids <- unique(c(setdiff(id1, id2), setdiff(id2, id1)))

# Tally the number of non-identical IDs
num_diff_ids <- length(diff_ids) # 
