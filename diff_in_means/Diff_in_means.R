library(dplyr)
library(tidyverse)
library(zoo)
library(stringr)
library(tigris)
library(lubridate)
library(tigris)
library(conflicted)
library(Rvoteview)

#declare preferences for conflicts
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("year", "lubridate")
conflict_prefer("month", "lubridate")
conflict_prefer("day", "lubridate")
conflict_prefer("lag", "dplyr")

#county-level controls, updated 2020 by Dorn x Autor
dorn_aut <- read_csv("house_2002_2016.csv") %>% 
select(congressionaldistrict,cty_fips,
       l_shind_manuf_cbp,l_sh_routine33,l_task_outsource, #labor mkt
       l_sh_pop_f,l_sh_pop_edu_c,l_sh_fborn, #sex #education #foreign-born 
       l_sh_pop_age_1019, l_sh_pop_age_2029, l_sh_pop_age_3039, #age dist
       l_sh_pop_age_4049, l_sh_pop_age_5059, l_sh_pop_age_6069,
       l_sh_pop_age_7079, l_sh_pop_age_8000,
       l_sh_pop_white, l_sh_pop_black, l_sh_pop_asian, l_sh_pop_hispanic,
       majority_white, #raceeth 
       reg_midatl, reg_encen, reg_wncen, reg_escen, #geographic
       reg_wscen, reg_mount, reg_pacif) %>% 
    rename(district=congressionaldistrict,
           fips=cty_fips)

#getting some political data 
#congressional district representation for various congresses
res_extra <- voteview_search("'free trade'", 
                             congress = c(105:110), chamber = "House")
  
cd_reps <- melt_rollcall(voteview_download(res_extra[c(1,12,15,22,40,49),]$id),
                          legiscols = c("state_abbrev","party_code","cqlabel"),
                          votecols = c("congress")) %>% 
  mutate(district=paste(state_abbrev,
                        as.numeric(str_extract(cqlabel,"[:digit:]+")))) %>% 
  select(-cqlabel, -id, -vname, -vote) %>% 
  group_by(district, congress, state_abbrev) %>% 
  summarise(party_code=mean(as.numeric(party_code))) %>% 
  mutate(party_code=ifelse(as.numeric(party_code)==200,1,0)) %>% 
  spread(congress,party_code,sep="_") 


#population density and household size (2010)
population <- read_csv("popdensity.csv") 

#rural urban county codes 2013
rural <- read_csv("rural.csv") 

#median family income (2010) from census API
income <- read_csv("medianinc_cty_2010.csv")

#join everything together
cty_data <- dorn_aut %>% 
  inner_join(cd_reps, by=c("district"="district")) %>% 
  inner_join(population, by=c("fips"="fips")) %>% 
  inner_join(rural, by=c("fips"="fips")) %>% 
  inner_join(income, by=c("fips"="fips")) %>% 
  relocate(29)
  



