# PROJECT:  Aging
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  export data for Sankey plot
# REF ID:   4641776b 
# LICENSE:  MIT
# DATE:     2023-03-03
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library("tidyverse")

# IMPORT ------------------------------------------------------------------
  
  #import data from `Generate_Client_visits_df.R`
    status_data_fy <- read_csv("Dataout/aging_patient-status_fy.zip")

# MUNGE -------------------------------------------------------------------

  #collapse fy status data into one row (when new, may have multiple rows)#
  #order status so that New is always last for string parsing later
  status_data_fy_collapsed <- status_data_fy %>% 
    mutate(status = ifelse(status == "IIT (LTFU -> RTT)", "IIT", status),
           status = fct_relevel(status, "New", after = Inf)) %>% 
    arrange(id2, fiscal_year, status) %>% 
    mutate(status = as.character(status)) %>%
    group_by(id2, fiscal_year) %>% 
    summarise(status_start_yr = paste0(status, collapse = " - "),
              .groups = "drop") 
  
  #expand each patient row to have every year and then pull the future year's status
  status_data_fy_collapsed <- status_data_fy_collapsed %>% 
    complete(fiscal_year, nesting(id2)) %>% 
    group_by(id2) %>% 
    mutate(status_end_yr = lead(status_start_yr, n = 1, order_by = fiscal_year)) %>% 
    ungroup() 
  
  #removed LTFU from base year and get ride of "New" status from base year
  status_data_fy_collapsed <- status_data_fy_collapsed %>% 
    mutate(status_start_yr = ifelse(str_detect(status_start_yr, "(LTFU|Aged Out)"), NA_character_, status_start_yr),
           status_start_yr = str_remove(status_start_yr, " - New")) %>% 
    filter(!(is.na(status_start_yr) & is.na(status_end_yr)))
  
  #move new to start for placement on plot (will move manually to mid point)
  status_data_fy_collapsed <- status_data_fy_collapsed %>% 
    mutate(status_start_yr = ifelse(is.na(status_start_yr) & str_detect(status_end_yr, "New"), "New", status_start_yr),
           status_end_yr = str_remove(status_end_yr, " - New"))
  
  
  #aggregate up by status groups for plot (and number for plot layout ordering)
  status_viz <- status_data_fy_collapsed %>% 
    count(fiscal_year, status_start_yr, status_end_yr, name = "patients", sort = TRUE) %>% 
    mutate(status_start_yr = case_match(status_start_yr,
                                        "Active" ~ "1. Active",
                                        "IIT" ~ "2. RTT",
                                        "New" ~ "3. New",
                                        # .default = status_start_yr), #check that no missing categories
                                        .default = "4. Unclassified"),
           status_end_yr = case_match(status_end_yr,
                                        "Active" ~ "1. Active",
                                        "IIT" ~ "2. IIT",
                                        "Aged Out" ~ "3. Aged Out",
                                        "LTFU" ~ "4. LTFU",
                                        # .default = status_end_yr) #check that no missing categories
                                        .default = "5. Unclassified")
    ) 
  
  #export each year separately for plotting in RawGraphs (https://app.rawgraphs.io/)
  walk(2019:2022,
       ~ status_viz %>% 
         filter(fiscal_year == .x) %>% 
         write_csv(paste0("Dataout/aging-viz_fy", .x, ".csv"), na = "")
  )
 
  #In RawGraphs, 
  # 1. load data (not saved by site)
  # 2. Choose a chart: Alluvial Diagram
  # 3. Mapping: Steps = status_start_year, status_end_year; Size = patients
  # 4. Customize: 
  #  width = 1280
  #  height = 720
  #  padding = 20
  #  Links opacity = .8
  # $. Export: svg and cleaned up in Adobe Illustrator (this can also be done in PowerPoint)