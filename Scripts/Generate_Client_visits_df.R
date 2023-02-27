library("tidyverse")
library("data.table")
library("waterfalls")


visits_data <- read_csv("Data/Appointments.csv", na = c("", "NA", "NULL"))
master_clientlist <- read_csv("Dataout/master_clientlist.csv") #created from Scripts/Generate_master_clientlist.R

#Renaming the variables and creating id2 based on id and facility
visits_data <- visits_data %>% rename(im=`Implementing Partner`, facility=`Facility Name`, mfl_code=`Facility MFL Code`,
                                      id=`Client Number(De-identified)`, appoint_date=`Appointment Date`, 
                                      visit_date=`Actual Visit Date`) %>% 
  mutate(id2 =  paste0(mfl_code, "_", id)) %>% 
  filter(!is.na(appoint_date) & !is.na(visit_date) & !is.null(appoint_date)) %>% 
  filter(appoint_date != "NULL")



setDT(visits_data)

# change the dates  from character to date format

visits_data$visit_date <- as.Date(visits_data$visit_date, format = "%d/%m/%Y")
visits_data$appoint_date <- as.Date(visits_data$appoint_date, format = "%d/%m/%Y")



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

# create masterlist_2 with  client and visit data( right join - every id in master clinet list and all vists in clean vists data with ID2 apearing in matserclinetlist )
operator <- merge(clean_visits_data, master_clientlist, by ="id2", all.y = TRUE)

#data table will be sorted in ascending order based on the values in the id2 column.
setkey(operator, id2)

n_distinct(clean_visits_data$id2)-n_distinct(master_clientlist$id2)#-6575 clients in client list but with no visits 

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
  lost_clients[, `:=`(status = "lftu")]
  
  setkey(operator3, id2)
  active_clients <- operator3[!lost_clients][, .(id2)]
  active_clients[, `:=`(status = "Active")]
  
  result <- rbind(lost_clients, active_clients)
  set(result, i = NULL, j = "fiscal_year_ending", value = end_fy)
  
  return(result)
  
}


fiscal_years       <- purrr::map(0:5, ~as.Date("2022-09-30") - years(.))
all_years_analysis <- purrr::map(fiscal_years, analyzeFiscalYear, operator= operator) %>% rbindlist()

all_yrs_analysis_wide<-dcast.data.table(all_years_analysis, id2 ~ fiscal_year_ending, value.var = "status")

debug(analyzeFiscalYear)

analyzeFiscalYear(as.Date("2018-09-30"), operator = operator)

# Merging visits data and  master client list 
client_visits_df <- merge(master_clientlist,all_yrs_analysis_wide, by = "id2",all.x = TRUE)

#Any theyear after LFTU  should be marked LFTU 

setDT(client_visits_df)

client_visits_df[art_init_period == "FY19" & is.na(`2019-09-30`), `2019-09-30` := "New+LFTU"]
client_visits_df[art_init_period == "FY19" & `2019-09-30` == "Active", `2019-09-30` := "New+Active"]
client_visits_df[art_init_period == "FY19" & `2019-09-30` == "lftu", `2019-09-30` := "New+LFTU"]
client_visits_df[art_init_period == "Other Years" & is.na(`2019-09-30`),  `2019-09-30` := "lftu"]


client_visits_df[art_init_period == "FY20" & is.na(`2020-09-30`), `2020-09-30` := "New+LFTU"]
client_visits_df[art_init_period == "FY20" & `2020-09-30` == "Active", `2020-09-30` := "New+Active"]
client_visits_df[art_init_period == "FY20" & `2020-09-30` == "lftu", `2020-09-30` := "New+LFTU"]
client_visits_df[art_init_period == "Other Years" & is.na(`2020-09-30`),  `2020-09-30` := "lftu"]

client_visits_df[art_init_period == "FY21" & is.na(`2021-09-30`), `2021-09-30` := "New+LFTU"]
client_visits_df[art_init_period == "FY21" & `2021-09-30` == "Active", `2021-09-30` := "New+Active"]
client_visits_df[art_init_period == "FY21" & `2021-09-30` == "lftu", `2021-09-30` := "New+LFTU"]
client_visits_df[art_init_period == "Other Years" & is.na(`2021-09-30`),  `2021-09-30` := "lftu"]

client_visits_df[art_init_period == "FY22" & is.na(`2022-09-30`), `2022-09-30` := "New+LFTU"]
client_visits_df[art_init_period == "FY22" & `2022-09-30` == "Active", `2022-09-30` := "New+Active"]
client_visits_df[art_init_period == "FY22" & `2022-09-30` == "lftu", `2022-09-30` := "New+LFTU"]
client_visits_df[art_init_period == "Other Years" & is.na(`2022-09-30`),  `2022-09-30` := "lftu"]
#... add the rest of the years here

# handling lost to treatment propogation from one year to the next
client_visits_df[!is.na(`2019-09-30`) & is.na(`2020-09-30`), `2020-09-30` := "lftu"]
client_visits_df[!is.na(`2020-09-30`) & is.na(`2021-09-30`), `2021-09-30` := "lftu"]
client_visits_df[!is.na(`2021-09-30`) & is.na(`2022-09-30`), `2022-09-30` := "lftu"]

# should be zero
client_visits_df[art_init_period %in% c("FY19", "Other Years") & is.na(`2019-09-30`)]
client_visits_df[art_init_period %in% c("FY20", "Other Years") & is.na(`2020-09-30`)]
client_visits_df[art_init_period %in% c("FY21", "Other Years") & is.na(`2021-09-30`)]
client_visits_df[art_init_period %in% c("FY22", "Other Years") & is.na(`2022-09-30`)]

client_visits_df[!is.na(`2019-09-30`) & is.na(`2020-09-30`), `2020-09-30` := "lftu"]
client_visits_df[!is.na(`2020-09-30`) & is.na(`2021-09-30`), `2021-09-30` := "lftu"]
client_visits_df[!is.na(`2021-09-30`) & is.na(`2022-09-30`), `2022-09-30` := "lftu"]


client_visits_df[age_out_yr == "2022", `2022-09-30` := "Aged Out"]
client_visits_df[age_out_yr == "2021", `2021-09-30` := "Aged Out"]
client_visits_df[age_out_yr == "2020", `2020-09-30` := "Aged Out"]
client_visits_df[age_out_yr == "2019", `2019-09-30` := "Aged Out"]

client_visits_df[`2019-09-30` == "Aged Out", `2020-09-30` := "Aged Out"]
client_visits_df[`2020-09-30` == "Aged Out", `2021-09-30` := "Aged Out"]
client_visits_df[`2021-09-30` == "Aged Out", `2022-09-30` := "Aged Out"]
#Visualizing script 

head(client_visits_df)


#Plotdata<-viz_df[!is.na(fiscal_year_ending), .N, by = .(fiscal_year_ending, resolved_status)]




