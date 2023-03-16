library(tidyverse)
library(scales)

status_data_fy <- vroom::vroom("Dataout/aging_patient-status_fy.zip")

#use a sample to make the work easier to review
set.seed(42)
sample_id <- unique(status_data_fy$id) %>% sample(100)

status_data_fy_test <- status_data_fy %>% 
  filter(id %in% sample_id)

#what is the total we should be seeing?
count(status_data_fy_test, status)

#collapse New row into end of year status
viz_collapsed <- status_data_fy_test %>% 
  mutate(status = fct_relevel(status, "New", after = Inf)) %>% 
  arrange(id, fiscal_year, status) %>% 
  mutate(status = as.character(status)) %>%
  summarise(status_start_yr = paste0(status, collapse = " - "),
            .by = c(id, fiscal_year))

#check
count(viz_collapsed, status_start_yr)

#create a wide view by id to know what to expect
viz_collapsed %>% 
  arrange(fiscal_year) %>% 
  pivot_wider(names_from = fiscal_year,
              values_from = status_start_yr)

#identify the min and max reporting year for each client (so we can remove extra values created in complete)
viz_collapsed <- viz_collapsed %>% 
  mutate(min_fy = min(fiscal_year),
         max_fy = max(fiscal_year),
         .by = id)

#expand each patient row to have every year 
viz_collapsed <- viz_collapsed %>% 
  complete(fiscal_year, nesting(id)) %>% 
  arrange(id, fiscal_year) %>% 
  mutate(added = case_when(is.na(status_start_yr) ~ TRUE))

#fill missing years with status (should just be IIT)
viz_collapsed <- viz_collapsed %>% 
  group_by(id) %>% 
  fill(ends_with("fy"), .direction = "downup") %>% 
  fill(status_start_yr, .direction = "down") %>% 
  mutate(status_start_yr = case_when(between(fiscal_year, min_fy, max_fy) ~ status_start_yr)) %>% 
  ungroup() %>% 
  select(-ends_with("fy"))

#remove New from filled values
viz_collapsed <- viz_collapsed %>% 
  mutate(status_start_yr = case_when(added == TRUE & status_start_yr == "New" ~ NA_character_,
                                     added == TRUE ~ str_remove(status_start_yr, " - New"),
                                     TRUE ~ status_start_yr)) %>%
  select(-added)

#flag statuses that are just New (ie don't have visit data during the year they were initiated)
viz_collapsed <- viz_collapsed %>% 
  mutate(orig_new = status_start_yr == "New")

#pull the future year's status
viz_collapsed <- viz_collapsed %>% 
  # complete(fiscal_year, nesting(id)) %>% 
  # arrange(id, fiscal_year) %>%
  mutate(status_end_yr = lead(status_start_yr, n = 1, order_by = fiscal_year),
         .by = id)

viz_collapsed <- viz_collapsed %>%  
  mutate(status_start_yr = ifelse(str_detect(status_end_yr, "New", negate = TRUE) | is.na(status_end_yr), status_start_yr,  "New"),
         across(ends_with("yr"), \(x) str_remove(x, " - New")),
         status_end_yr = ifelse(status_end_yr == "New", NA_character_, status_start_yr)
         # status_start_yr = ifelse(orig_new == TRUE, NA_character_, status_start_yr),
  )