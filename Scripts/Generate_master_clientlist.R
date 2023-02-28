# PROJECT:  Aging
# AUTHOR:   N.Maina & A.Chafetz | USAID
# PURPOSE:  create a master client list to combine with visit data
# REF ID:   3810929a 
# LICENSE:  MIT
# DATE:     2023-02-28
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)

  
# IMPORT ------------------------------------------------------------------
  
  #The client list
  clientlist <- read_csv("Data/AgingOut_Feb162023.csv")
  # str(clientlist)
  # dim(clientlist)

# MUNGE -------------------------------------------------------------------

  #Renaming the variables
    clientlist <- clientlist %>% 
      rename(im=`Implementing Partner`, 
             facility=`Facility Name`, 
             mfl_code=`Facility MFL Code`,
             county=`County of Treatment`,
             id=`Client Number(De-identified)`, 
             sex=Sex, 
             orphan=Orphan, 
             residence=`County of Residence`,
             dob=DOB, 
             date_art_init=`ART Initiation Date`, 
             reg_art_init=`ART Regimen at Initiation`,
             line_art_init=`ART  Regimen Line at Initiation`, 
             reg_art_current=`Current ART Regimen`,
             line_art_current=`Current ART Regimen Line`)

  #conver DOB to date
  clientlist <- clientlist %>% 
    mutate(dob = as_date(dob) %>% ymd)

  #Creating distinct clients based on ids
  # clientlist %>% distinct(id) #91,927 distinct clients based on client ids only from the 242,377 in the original dataset
  # clientlist %>% distinct(facility, id) #97,735 distinct clients based on client id and facility 
  # clientlist %>% distinct(mfl_code, id) #97,735 distinct clients based on client id and facility mfl code 
  ref_clientlist <- clientlist %>% distinct(facility, mfl_code, id) #97,745 distinct clients based on client id and facility mfl code
  # sum(duplicated(ref_clientlist))

  #creating id2 based on patient id and facility id( use aging out data from palladiaum)
  clientlist <- clientlist %>% 
    mutate(id =  str_sub(id, -8)) %>% 
    unite(id2, c(mfl_code, id)) 

    length(unique(clientlist$id2)) == nrow(ref_clientlist)
  
  #Creating master client list based on facility, mfl code, county, id, sex, dob, ART initiation date, ART regimen at initiation and current ART regimen
  #Leaving out the implementing partner, orphan, county of residence and the ART regimen lined both at initiation and current
  master_clientlist <- clientlist %>% 
    distinct(id2, sex, dob, facility, county,date_art_init, reg_art_current)

  # Checking the duplicates in 'master_clientlist' # 12 ids ( but 25  observations)in the mater clientlist are duplicates 
  dupl <- names(which(table(master_clientlist$id2)>1))
  master_clientlist<- filter(master_clientlist, !id2 %in% dupl) 


  #Identify point of aging out
    master_clientlist <- master_clientlist %>% 
      mutate(date_age_out = dob %m+% years(15))


# EXPORT ------------------------------------------------------------------

    write_csv(master_clientlist, "Dataout/master_clientlist.csv", na = "")

# EXPORATION --------------------------------------------------------------

  # How many persons age-up in each year? 
    master_clientlist %>% 
      mutate(date_age_out_fy = date_age_out %>% 
               quarter(with_year = TRUE, fiscal_start = 10) %>%
               str_replace("20", "FY") %>% 
               str_sub(end = 4)) %>% 
      count(date_age_out_fy) %>% 
      filter(date_age_out_fy %in% paste0("FY", 19:23))
  
  
  # How many persons age-up in each year vs entering? 
    master_clientlist %>% 
      mutate(date_art_init_fy = date_art_init %>% 
               quarter(with_year = TRUE, fiscal_start = 10) %>%
               str_replace("20", "FY") %>% 
               str_sub(end = 4),
             date_age_out_fy = date_art_init %>% 
               quarter(with_year = TRUE, fiscal_start = 10) %>%
               str_replace("20", "FY") %>% 
               str_sub(end = 4)) %>% 
      mutate(enter = date_art_init_fy,
             exit = date_age_out_fy) %>% 
      select(enter, exit) %>% 
      filter(!c(is.na(enter) & is.na(exit))) %>% 
      pivot_longer(everything(),
                   names_to = "type",
                   values_to = "fiscal_year",
                   values_drop_na = TRUE) %>% 
      count(type, fiscal_year, name = "value") %>% 
      pivot_wider(names_from = type) %>% 
      filter(fiscal_year %in% paste0("FY", 19:23))
  
  
  
  
