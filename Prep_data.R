library(dplyr)
library(tidyverse)
library(zoo)
library(stringr)
library(tigris)
library(lubridate)
library(tigris)
library(conflicted)

#declare preferences for conflicts
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("year", "lubridate")
conflict_prefer("month", "lubridate")
conflict_prefer("day", "lubridate")
conflict_prefer("lag", "dplyr")

#================CONNECTIVITY======================#

prep_connections_data <- function(){
  tract_15 <- read_csv("tract_data/tract15.csv") %>% 
    mutate(fips=as.numeric(str_sub(tractcode,end=-7))) %>%
    group_by(fips) %>%
    summarise(pcat_15 = mean(pcat_all))
  tract_16 <- read_csv("tract_data/tract16.csv") %>% 
    mutate(fips=as.numeric(str_sub(tractcode,end=-7))) %>%
    group_by(fips) %>%
    summarise(pcat_16 = mean(pcat_all))
  tract_17 <- read_csv("tract_data/tract17.csv") %>% 
    mutate(fips=as.numeric(str_sub(tractcode,end=-7))) %>%
    group_by(fips) %>%
    summarise(pcat_17 = mean(pcat_all))
  tract_18 <- read_csv("tract_data/tract18.csv") %>% 
    mutate(fips=as.numeric(str_sub(tractcode,end=-7))) %>%
    group_by(fips) %>%
    summarise(pcat_18 = mean(pcat_all))
  tract_19 <- read_csv("tract_data/tract19.csv") %>% 
    mutate(fips=as.numeric(str_sub(tractcode,end=-7))) %>%
    group_by(fips) %>%
    summarise(pcat_19 = mean(pcat_all))
  
  connections_wide <<- tract_15 %>% 
    inner_join(tract_16) %>%
    inner_join(tract_17) %>%
    inner_join(tract_18) %>%
    inner_join(tract_19) %>% 
    inner_join(read_csv("crosswalks/fips_states.csv"))
  
  connections_long <<- connections_wide %>% 
                        rename(`2015`=pcat_15,
                               `2016`=pcat_16,
                               `2017`=pcat_17,
                               `2018`=pcat_18,
                               `2019`=pcat_19) %>% 
                        gather(year,connectivity,`2015`:`2019`)
}

#================POLICIES======================#

prep_policies <- function(){
restrictions_wide <<- read_csv("policies/muni_broadband_policies.csv") %>%
  select(-`2020`)
funds_wide <<- read_csv("policies/broadband_funds.csv")
funds_long <<- funds_wide %>% gather(year,funding,`2015`:`2019`) %>% 
  inner_join(read_csv("crosswalks/fips_states.csv"))
restrictions_long <<- restrictions_wide %>% select(-law) %>% 
  gather(year,restrictiveness,`2015`:`2019`) %>% 
  inner_join(read_csv("crosswalks/fips_states.csv"))
}

#================CONTROLS======================#

prep_controls <- function(){
#import new york times covid dataset and clean 
nytcovid <- read_csv("covid/nytcovid_counties.csv") %>% 
  mutate(year=year(date), month=month(date), day=day(date)) %>% 
  filter(day==1 & !is.na(fips) & year!=2021) %>% 
  mutate(cases=replace_na(cases,0), deaths=replace_na(deaths,0)) %>%
  group_by(fips) %>%
  mutate(newcases=cases-lag(cases), newdeaths=deaths-lag(deaths)) %>% 
  filter(!is.na(newcases) & !(month < 4)) %>% 
  mutate(fips = as.numeric(fips))  #leave this for now

#population (2019) and population density (2010)
population <- read_csv("census_data/pop_chars.csv") %>% 
  select(-FID, -COUNTYNS, -ALAND, -AWATER, -NAME, -State)

#rural urban county codes 2013
rural <- read_csv("census_data/ruralurbancodes2013.csv") %>% 
  mutate(fips=as.numeric(FIPS)) %>% 
  select(-FIPS) %>% mutate(metro=0)
for (i in seq(1:nrow(rural))){
  if(rural[i,1]%in% c(1,2,3)){
    rural$metro[i] = 1
  }
}

#income data
income <- read_csv("census_data/income_data.csv") %>% 
  mutate(fips=as.numeric(fips_txt),
         inc19 = Median_Household_Income_2019) %>%
  select(fips,inc19)

#county-level controls, updated 2020 by Dorn x Autor
controls <- read_csv("census_data/controls_dorn_aut.csv") %>% 
  select(fips,l_shind_manuf_cbp,l_sh_routine33,l_task_outsource, #labor mkt
         l_sh_pop_f,l_sh_pop_edu_c,l_sh_fborn, #sex #education #foreign-born 
         l_sh_pop_age_1019, l_sh_pop_age_2029, l_sh_pop_age_3039, #age dist
         l_sh_pop_age_4049, l_sh_pop_age_5059, l_sh_pop_age_6069,
         l_sh_pop_age_7079, l_sh_pop_age_8000,
         l_sh_pop_white, l_sh_pop_black, l_sh_pop_asian, l_sh_pop_hispanic,
         majority_white, #raceeth 
         house20_r, house16_r, rep_control,#politics
         reg_midatl, reg_encen, reg_wncen, reg_escen, #geographic
         reg_wscen, reg_mount, reg_pacif)


#put all the controls together (labor mkt, demographics, covid)
full_controls <- inner_join(controls,population,by=c("fips"="GEOID"))
full_controls <- inner_join(fips_codes %>% 
                              mutate(fips=as.numeric(paste0(state_code,
                                                            county_code))),
                            full_controls,
                            by=c("fips"="fips"))
full_controls <- left_join(full_controls, nytcovid %>% 
                             filter(month==5) %>% #May 1 COVID count for April UER
                             select(fips,cases,deaths),
                           by=c("fips"="fips")) %>%  
  mutate(case_rate = cases/Population*1000,
         death_rate = deaths/Population*1000) %>%
  mutate(case_rate = replace_na(case_rate,0), #big assumption 
         death_rate = replace_na(death_rate,0)) %>%
  select(-cases,-deaths) 
full_controls <- left_join(full_controls, rural) 
full_controls <<- left_join(full_controls,income)
remove(controls,population,rural,nytcovid)
}

