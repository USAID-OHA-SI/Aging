# PROJECT:  Aging
# AUTHOR:   N.Maina & A.Chafetz | USAID
# PURPOSE:  identify status of patients in a given period
# REF ID:   be2d1a77 
# LICENSE:  MIT
# DATE:     2023-02-28
# UPDATED:  2023-03-03

# DEPENDENCIES ------------------------------------------------------------
  
  library("tidyverse")

  #not loaded, but uses explicit references to the tidylog package for reviewing filter effects

# IMPORT ------------------------------------------------------------------
  
  #read in visits data
  visits_data <- read_csv("Data/Appointments.csv", na = c("", "NA", "NULL"))
  
  #read in master client list data
  master_clientlist <- read_csv("Dataout/master_clientlist.csv") #created from Scripts/Generate_master_clientlist.R
  
# BASIC MUNGING -----------------------------------------------------------

  #Renaming the variables
    visits_data <- visits_data %>% 
      rename(im=`Implementing Partner`, 
             facility=`Facility Name`, 
             mfl_code=`Facility MFL Code`,
             id=`Client Number(De-identified)`, 
             appoint_date=`Appointment Date`, 
             visit_date=`Actual Visit Date`) 
  
  #create id2 to map onto master list
    visits_data <- visits_data %>% 
      mutate(id =  str_sub(id, -8)) %>% 
      unite(id2, c(mfl_code, id))
  
  #remove any visits that don't have a future appointment or an invalid date
    visits_data <-  visits_data %>%  
      tidylog::filter(!is.na(appoint_date), 
                      appoint_date != "01/01/1900")

  #convert the dates from character to date format for using in calculations
    visits_data <- visits_data %>% 
      mutate(across(c(visit_date, appoint_date), \(x) dmy(x)))

  #keep only observations where the appointment date occurs after visit date
    visits_data <- visits_data %>% 
      tidylog::filter(appoint_date >= visit_date)

  #reorder columns for ease of review
    visits_data <- visits_data %>% 
      relocate(id2, facility, .before = 1) %>% 
      relocate(visit_date, .before = appoint_date)
    
  #identify the latest visit date in dataset (filter out future LTFU/age out later)
    max_visit <- max(visits_data$visit_date, na.rm = TRUE)


# REMOVE DUPLICATES AND CREATE NEXT VISIT OBS -----------------------------

  # remove duplicate entries
    clean_visits_data <- tidylog::distinct(visits_data) 

  #Only working with clients who are in the master client list
  #Limitation: We'll need to drop duplicates based on id2
  # check for duplicates  based on id2
    clean_visits_data %>% distinct(id2) %>% nrow() #88,009 distinct clients based on 1,800810 observations in the visits_dataset

  #add in the next actual visit date for calculating duration between appointments
    clean_visits_data <- clean_visits_data %>% 
      arrange(id2, visit_date) %>% 
      group_by(id2) %>% 
      mutate(next_visit_date = lead(visit_date, order_by = visit_date)) %>% 
      ungroup()


# CALCULATE LTFU ----------------------------------------------------------

  #calculation duration between visits (proxy = 28 days after planned appt gap)
    clean_visits_data <- clean_visits_data %>% 
      mutate(visit_gap_planned = appoint_date - visit_date,
             visit_gap_allowed = appoint_date + days(28) - visit_date,
             visit_gap_actual = next_visit_date - visit_date) 

  #ltfu (vs rtt) status
    clean_visits_data <- clean_visits_data %>% 
      mutate(ltfu = case_when(appoint_date + days(28) > max_visit ~ FALSE,
                              is.na(next_visit_date) ~ TRUE,
                              visit_gap_actual > visit_gap_allowed ~ TRUE,
                              TRUE ~ FALSE),
             rtt = case_when(ltfu == TRUE & is.na(next_visit_date) ~ FALSE,
                             ltfu & !is.na(next_visit_date) ~ TRUE))
    
  #create type for adding in different datesets
    clean_visits_data <- clean_visits_data %>% 
      mutate(status = "Active",
             date = visit_date)

# IDENTIFYING AGING OUT ---------------------------------------------------

  #add ageout date as row
    date_ageout <- master_clientlist %>% 
      # tidylog::filter(date_age_out > date_art_init) %>% 
      tidylog::filter(date_age_out <= max_visit) %>% 
      tidylog::filter(id2 %in% clean_visits_data$id2) %>% 
      mutate(date = date_age_out,
             status = "Aged Out") 

  #merge on age data (for filtering)
    merged_visits_data <- clean_visits_data %>% 
      tidylog::left_join(master_clientlist,
                by = c("id2", "facility"))
    
  #relocate patient data closer to id
    merged_visits_data <- merged_visits_data %>% 
      relocate(sex:date_age_out, .after = id2)


# ADDING FUTURE OBSERVATIONS FOR LTFU -------------------------------------

  #add in LTFU date as row (will be future period ahead of last visit)
    date_ltfu <- merged_visits_data %>%
      filter(ltfu == TRUE) %>%
      mutate(date = visit_date + visit_gap_allowed + days(1),
             status = ifelse(rtt == FALSE, "LTFU", "IIT (LTFU -> RTT)")) %>%
      select(-visit_date:-visit_gap_actual) %>% 
      filter(date <= max(merged_visits_data$visit_date),
             date < date_age_out) 

  #bind date of ltfu date + age out date back onto dataset
    binded_visits_data <- merged_visits_data %>% 
      bind_rows(date_ltfu, date_ageout) %>% 
      arrange(id2, date)

  #remove patients after 15+ years old
    binded_visits_data <- binded_visits_data %>% 
      tidylog::filter(date <= date_age_out)
  
  #remove patients where only observation is aging out
    binded_visits_data <- binded_visits_data %>% 
      group_by(id2) %>% 
      tidylog::filter(!(status == "Aged Out" & n() == 1)) %>% 
      ungroup()

# IDENTIFY STATUS BY PERIOD -----------------------------------------------

  #apply fiscal quarter
    binded_visits_data <- binded_visits_data %>% 
      mutate(period = date %>% 
               quarter(with_year = TRUE, fiscal_start = 10) %>%
               str_replace("20", "FY") %>% 
               str_replace("\\.", "Q"),
             fiscal_year = period %>% 
               str_sub(3,4) %>% 
               paste0(20, .) %>% 
               as.integer())
    
  #arrange by status (Active to come after LTFU in event they occur on the same day)
    status_data <- binded_visits_data %>% 
      mutate(status = factor(status, c("LTFU", "IIT (LTFU -> RTT)", "Active", "Aged Out"))) %>% 
      arrange(id2, date, status)
    
  #filter by the last observed status per period to capture the status
    status_data <- status_data %>% 
      group_by(id2, period) %>% 
      filter(row_number() == max(row_number())) %>% 
      ungroup()

  #include new initiation (between bounds of visit dataset and excluded 15+)
    status_new <- master_clientlist %>% 
      mutate(period = date_art_init %>% 
               quarter(with_year = TRUE, fiscal_start = 10) %>%
               str_replace("20", "FY") %>% 
               str_replace("\\.", "Q"),
             fiscal_year = period %>% 
               str_sub(3,4) %>% 
               paste0(20, .) %>% 
               as.integer(),
             status = "New",
             date = date_art_init) %>%
      tidylog::filter(date_age_out > date_art_init) %>% 
      tidylog::filter(id2 %in% clean_visits_data$id2) %>% 
      tidylog::filter(between(date_art_init, min(clean_visits_data$visit_date, na.rm = TRUE), max(clean_visits_data$visit_date, na.rm = TRUE)))

  #bind on new initations
    status_data <- status_data %>% 
      mutate(status = as.character(status)) %>% 
      bind_rows(status_new) %>%
      arrange(id2, date)


# IDENTIFY STATUS BY FISCAL YEAR ------------------------------------------

    #arrange by status (Active to come after LTFU in event they occur on the same day)
    status_data_fy <- binded_visits_data %>% 
      mutate(status = factor(status, c("LTFU", "IIT (LTFU -> RTT)", "Active", "Aged Out"))) %>% 
      arrange(id2, date, status)
    
    #filter by the last observed status per period to capture the status
    status_data_fy <- status_data_fy %>% 
      group_by(id2, fiscal_year) %>% 
      filter(row_number() == max(row_number())) %>% 
      ungroup()
    
    #bind on new initations
    status_data_fy <- status_data_fy %>% 
      mutate(status = as.character(status)) %>% 
      bind_rows(status_new) %>%
      arrange(id2, date)
    
    
# AGGREGATE TABLE ---------------------------------------------------------

  #agg table of status by quarter (period)
  status_data %>% 
    count(fiscal_year, period, status) %>% 
    pivot_wider(names_from = status,
                values_from = n) %>% 
    relocate(New, Active, `IIT (LTFU -> RTT)`, .after = 2)


  #agg table of status by fiscal_year 
    status_data_fy %>% 
      count(fiscal_year, status) %>% 
      pivot_wider(names_from = status,
                  values_from = n) %>% 
      relocate(New, Active, `IIT (LTFU -> RTT)`, LTFU, .after = 2)



# EXPORT ------------------------------------------------------------------

  #export
    write_csv(binded_visits_data, "Dataout/aging_patient-status_date.csv", na = "")
    zip("Dataout/aging_patient-status_date.zip",
        "Dataout/aging_patient-status_date.csv", extras = "-j")
    unlink("Dataout/aging_patient-status_date.csv")
    
    write_csv(status_data, "Dataout/aging_patient-status_qtr.csv", na = "")
    zip("Dataout/aging_patient-status_qtr.zip",
        "Dataout/aging_patient-status_qtr.csv", extras = "-j")
    unlink("Dataout/aging_patient-status_qtr.csv")
    
    write_csv(status_data_fy, "Dataout/aging_patient-status_fy.csv", na = "")
    zip("Dataout/aging_patient-status_fy.zip",
        "Dataout/aging_patient-status_fy.csv", extras = "-j")
    unlink("Dataout/aging_patient-status_fy.csv")
  
    
    
    

   