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

count(status_data_fy_test, fiscal_year, status) %>% 
  pivot_wider(names_from = fiscal_year, values_from = n)

#collapse New row into end of year status
viz_collapsed <- status_data_fy_test %>% 
  mutate(status = fct_relevel(status, "New", after = Inf)) %>% 
  arrange(id, fiscal_year, status) %>% 
  mutate(status = as.character(status)) %>%
  summarise(status = paste0(status, collapse = " - "),
            .by = c(id, fiscal_year))

#check
count(viz_collapsed, status)

#create a wide view by id to know what to expect
viz_collapsed %>% 
  arrange(fiscal_year) %>% 
  pivot_wider(names_from = fiscal_year,
              values_from = status)

#identify the min and max reporting year for each client (so we can remove extra values created in complete)
viz_collapsed <- viz_collapsed %>% 
  mutate(min_fy = min(fiscal_year),
         max_fy = max(fiscal_year),
         .by = id)

#expand each patient row to have every year 
viz_collapsed <- viz_collapsed %>% 
  complete(fiscal_year, nesting(id)) %>% 
  arrange(id, fiscal_year) %>% 
  mutate(added = case_when(is.na(status) ~ TRUE))

#check
count(viz_collapsed, status)

#fill missing years with status (should just be IIT)
viz_collapsed <- viz_collapsed %>% 
  group_by(id) %>% 
  fill(ends_with("fy"), .direction = "downup") %>% 
  fill(status, .direction = "down") %>% 
  mutate(status = case_when(between(fiscal_year, min_fy, max_fy) ~ status)) %>% 
  ungroup() %>% 
  select(-ends_with("fy"))

#check
count(viz_collapsed, status)

#remove New from filled values
viz_collapsed <- viz_collapsed %>% 
  mutate(status = case_when(added == TRUE & status == "New" ~ NA_character_,
                                     added == TRUE ~ str_remove(status, " - New"),
                                     TRUE ~ status)) %>%
  select(-added)

#check
count(viz_collapsed, status)

viz_collapsed <- viz_collapsed %>% 
  mutate(status_end_yr = status %>% 
           str_remove(" - New") %>% 
           na_if("New")) 

#check
count(viz_collapsed, status_end_yr)

#pull the future year's status
viz_collapsed <- viz_collapsed %>% 
  mutate(status_prior_yr = ifelse(str_detect(status, "New"), "New",
    lag(status_end_yr, n = 1, order_by = fiscal_year)), .by = id)

#check
count(viz_collapsed, status_end_yr)
count(viz_collapsed, status_prior_yr)


#check start and end active totals match
full_join(viz_collapsed %>%
            filter(!is.na(status_end_yr)) %>% 
            count(fiscal_year, status = status_end_yr, name = "end"),
          viz_collapsed %>%
            filter(!is.na(status_prior_yr)) %>% 
            count(fiscal_year = fiscal_year - 1, status = status_prior_yr, name = "prior")
) %>% 
  print(n = Inf)


#check patients
map(sample_id[50:70],
    ~ viz_collapsed %>%
      select(id, fiscal_year, status, status_prior_yr, status_end_yr) %>% 
      filter(id == .x)
)

