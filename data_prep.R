### DATA PREPARATION FILE
### FOR U6614 DATA ANALYSIS FOR POLICY RESEARCH USING R
### BROADBAND POLICIES x CONNECTIVITY FINAL PROJECT
### Author: Liam Tay Kearney
### Date: April 2020

##(0)==========================SETUP============================#

library(dplyr)
library(tidyverse)
library(zoo)
library(stringr)
library(tigris)
library(lubridate)
library(tigris)
library(censusapi)
library(stargazer)

#Set census API key to environment
Sys.setenv(CENSUS_KEY="22c3b2e2538dd22d66c03c745f011d4fa1bc5bcf")

#Store github repository URL root to global env
root <<- "https://raw.githubusercontent.com/ltk2118/broadband_connectivity/main/"

##(1)================FCC CONNECTIVITY DATA======================#

#Connectivity data from the FCC's Form 477, from 2009-2019
#Data represents a score of proportion of households connected 
#to broadband in a county (sample = 3229 counties)
#Function stores long and wide form FCC data to global environment
prep_connections_data <- function(){
  
  #PART 1
  #COMPILE LONG FORM DATA
  #data in csv files labeled tract9.csv, tract10.csv ... tract19.csv
  for (i in seq(9,19)) {
    
    #reset metadata from tigris
    meta <- tigris::fips_codes %>% 
      mutate(fips=as.numeric(paste0(state_code,county_code)))
    
    year = as.character(i)
    
    #pull tract data from appropriate csv and collapse to county
    tract_data <- read_csv(paste0(root,"tract_data/tract",year,".csv")) %>% 
      mutate(fips=as.numeric(str_sub(tractcode,end=-7))) %>%
      select(fips,pcat_all) %>% 
      group_by(fips) %>% 
      summarise(pcat = mean(pcat_all,na.rm=TRUE)) %>% 
      mutate(year=i) #add year
      
      #join connections data with metadata if first year
      if(i==9){
             stack <- inner_join(meta, tract_data, by=c("fips"="fips"))
      }
    
      #join with metadata and rbind to stack if not first year
      else{
             stack <- stack %>% 
                         rbind(inner_join(meta, 
                                tract_data, by=c("fips"="fips")))
      }
    
  }
  
  #return long form data with numeric years
  connections_long <<- stack %>% mutate(year=as.numeric(year)+2000)
  
  #PART 2
  #COMPILE WIDE FORM DATA
  for (i in seq(9,19)) {
    
    #reset metadata from tigris
    meta <- tigris::fips_codes %>% 
      mutate(fips=as.numeric(paste0(state_code,county_code)))
    
    year = as.character(i)
    
    #pull tract data from appropriate csv and collapse to county
    tract_data <- read_csv(paste0(root,"tract_data/tract",year,".csv")) %>% 
      mutate(fips=as.numeric(str_sub(tractcode,end=-7))) %>%
      select(fips,pcat_all) %>% 
      group_by(fips) %>% 
      summarise(pcat = mean(pcat_all,na.rm=TRUE)) %>% 
      
      #add year suffix to the connectivity variable name
      rename_with(~sub("pcat",paste0("pcat_",year),.x),cols=c(pcat_all))
    
    #join connections data with metadata if first year
    if(i==9){
          stack <- inner_join(meta, tract_data, by=c("fips"="fips"))
    }
    
    #join connections data with stack if not first year
      else{
          stack <- stack %>% inner_join(tract_data, by=c("fips"="fips"))
    }
             
  }
  
  #return wide form data
  connections_wide <<- stack

}


##(2)=================CENSUS API PULL FUNCTIONS=================#

#helper function to pull county-level ACS5 data for specific variables
#can specify year(s) as numeric and state(s) as vector of numeric fips
#uses census variable product codes, input as string 
#default api is ACS 5-year, but can change to e.g. "acs1"
pull_acs_cty <- function(years,vars,states,api="acs5"){
  
  states <- paste0("state:",paste(sprintf("%02d",states),collapse = ","))
  
  #first year pull, the "stack"
  stack <- getCensus(
    name = paste0("acs/",api),
    vintage = min(years),
    vars = vars,
    region = "county",
    regionin = states) %>% 
    mutate(year=min(years))
  years <- years[years!=min(years)]
  
  #pull subsequent years and append to the "stack"
  for (i in years){
    append_to_stack <- getCensus(
      name = paste0("acs/",api),
      vintage = i,
      vars = vars,
      region = "county",
      regionin = states) %>% 
      mutate(year=i)
    
    stack <- stack %>% 
      rbind(append_to_stack)
  }
  
  #add metadata and return
  stack <- stack %>% 
    mutate(fips=as.numeric(paste0(state,county))) %>% 
    select(-state,-county)
  meta <- tigris::fips_codes %>% 
    mutate(fips=as.numeric(paste0(state_code,county_code)))
  stack <- inner_join(meta,
                      stack, by=c("fips"="fips"))
  
  return(stack)
  
}

#function to pull county-level data for a pre-defined set of variables 
#income, race/ethnicity, sex, age, urbanization and education
#can specify year(s) as numeric and state(s) as vector of numeric fips
#default api is ACS 5-year, but can change to e.g. "acs1"
#returns data frame with all variables in long form (if applicable)
pull_all_data <- function(years,states,api="acs5") {
  
  library(dplyr)
  
  #INCOME
  cens_income <- pull_acs_cty(years,
                                c("B19113_001E",  #median family income
                                  "B19001_001E",  #total 
                                  "B19001_002E",  #<10k
                                  "B19001_003E",  #10-14.999k
                                  "B19001_004E",  #15-19.999k
                                  "B19001_005E",  #20-24.999k
                                  "B19001_006E",  #25-29.999k
                                  "B19001_007E",  #30-34.999k
                                  "B19001_008E",  #35-39.999k
                                  "B19001_009E",  #40-44.999k
                                  "B19001_010E",  #45-49.999k
                                  "B19001_011E",  #50-59.999k
                                  "B19001_012E",  #60-74.999k
                                  "B19001_013E",  #75-99.999k
                                  "B19001_014E",  #100-124.999k
                                  "B19001_015E",  #125-149.999k
                                  "B19001_016E",  #150-199.999k
                                  "B19001_017E"),  #200k+
                                states,api) %>% 
                  mutate(api = api, #store api (useful later)
                         inc_10_under = B19001_002E/B19001_001E,   #granular income bins
                         inc_10_19 = (B19001_003E+B19001_004E)/B19001_001E, 
                         inc_20_29 = (B19001_005E+B19001_006E)/B19001_001E,
                         inc_30_39 = (B19001_007E+B19001_008E)/B19001_001E,
                         inc_40_49 = (B19001_009E+B19001_010E)/B19001_001E,
                         inc_50_59 = B19001_011E/B19001_001E,
                         inc_60_74 = B19001_012E/B19001_001E,
                         inc_75_99 = B19001_013E/B19001_001E,
                         inc_100_150 = (B19001_014E+B19001_015E)/B19001_001E,
                         incF_150_over = (B19001_016E+B19001_017E)/B19001_001E) %>% 
                  mutate(inc2_20_less = inc_10_under + inc_10_19,  #aggregated income bins
                         inc2_20_39 = inc_20_29 + inc_30_39,
                         inc2_40_59 = inc_40_49 + inc_50_59,
                         inc2_60_99 = inc_60_74 + inc_75_99,
                         inc2F_100_over = inc_100_150 + incF_150_over) %>% 
                  rename(median_hs_inc = B19113_001E) %>% 
                  select(-starts_with("B0"),-starts_with("B1"))
        #note, I use "F" in varnames to indicate they are multicollinear with 
        #previous vars, and will lead to a singularities if regressed together
        #this makes it easier to choose covariates programatically later
  
  #RACE, ETHNICITY, SEX
  cens_race_eth_sex <- pull_acs_cty(years,
                                      c("B02001_002E",   #white (race)
                                        "B02001_003E",   #black (race) 
                                        "B03003_003E",   #hispanic (eth, 2009+ only)
                                        "B01001_002E",   #male population
                                        "B01003_001E"),  #total population
                                      states,api) %>%
                        mutate(prop_white = B02001_002E/B01003_001E,
                               propF_black = B02001_003E/B01003_001E,
                               prop_hisp = B03003_003E/B01003_001E,
                               prop_male = B01001_002E/B01003_001E,
                               pop_total = B01003_001E) %>% 
                        select(-starts_with("B0"),-starts_with("B1"))
  
  #EDUCATION LEVELS
  cens_educ <- pull_acs_cty(years,
                              c("B06009_002E",   #less than high school grad
                                "B06009_003E",   #high school grad
                                "B06009_004E",   #some college
                                "B06009_005E",   #bachelor's
                                "B06009_006E",   #graduate
                                "B06009_001E"),  #total population
                                          states,api) %>% 
                mutate(prop_less_hs = B06009_002E/B06009_001E,
                       prop_hs_grad = B06009_003E/B06009_001E,
                       prop_some_coll = B06009_004E/B06009_001E,
                       prop_bachel = B06009_005E/B06009_001E,
                       propF_grad = B06009_006E/B06009_001E) %>% 
                select(-starts_with("B0"),-starts_with("B1"))
  
  #AGE 
  cens_age <- pull_acs_cty(years,
                             c("B06001_001E",  #total
                               "B06001_002E",  #<5
                               "B06001_003E",  #5-17
                               "B06001_004E",  #18-24
                               "B06001_005E",  #25-34
                               "B06001_006E",  #35-44
                               "B06009_007E",  #45-54
                               "B06009_008E",  #55-64
                               "B06009_011E",  #65-74
                               "B06009_012E",  #75-84
                               "B06009_013E"),  #>85
                             states,api) %>% 
                mutate(age_17_under = (B06001_002E+B06001_003E)/B06001_001E,
                       age_18_24 = B06001_004E/B06001_001E, #granular age bins
                       age_25_34 = B06001_005E/B06001_001E,
                       age_35_44 = B06001_006E/B06001_001E,
                       age_45_54 = B06009_007E/B06001_001E,
                       age_55_64 = B06009_008E/B06001_001E,
                       age_65_74 = B06009_011E/B06001_001E,
                       age_75_84 = B06009_012E/B06001_001E,
                       ageF_85_over = B06009_013E/B06001_001E) %>% 
                mutate(age2_17_under = age_17_under,
                       age2_18_34 = age_18_24 + age_25_34,  #aggregated age bins
                       age2_35_54 = age_35_44 + age_45_54,
                       age2_55_74 = age_55_64 + age_65_74,
                       age2F_75_over = age_75_84 + ageF_85_over) %>% 
                select(-starts_with("B0"),-starts_with("B1"))
              
  #LABOR FORCE & POVERTY
  cens_labor <- pull_acs_cty(years,
                              c("B17005_001E",  #total pop
                                "B17005_002E",  #total below poverty level
                                "B17005_003E", #male below pov
                                "B17005_008E", #female below pov
                                "B17005_014E", #male above pov
                                "B17005_019E", #female above pov
                                "B17005_004E",  #male below pov, in labf
                                "B17005_006E",  #male below pov, unemployed
                                "B17005_009E",  #female below pov, in labf
                                "B17005_011E", #female below pov, unemployed
                                "B17005_013E", #total at or above poverty level
                                "B17005_015E",  #male above pov, in labf
                                "B17005_017E",  #male above pov, unemployed
                                "B17005_020E",  #female above pov, in labf
                                "B17005_022E"),  #female above pov, unemployed
                                      states,api) %>% 
                 mutate(prop_poverty = B17005_002E/B17005_001E,
                        prop_ue_rate = (B17005_006E+B17005_011E+B17005_017E+B17005_022E)/(B17005_004E+B17005_009E+B17005_015E+B17005_020E),
                        prop_particip_rate = (B17005_004E+B17005_009E+B17005_015E+B17005_020E)/B17005_001E,
                        prop2_m_ue_rate = (B17005_006E+B17005_017E)/(B17005_004E+B17005_015E),
                        prop2_f_ue_rate = (B17005_011E+B17005_022E)/(B17005_009E+B17005_020E),
                        prop2_m_particip_rate = (B17005_004E+B17005_015E)/(B17005_003E+B17005_014E),
                        prop2_f_particip_rate = (B17005_009E+B17005_020E)/(B17005_008E+B17005_019E)) %>% 
                  select(-starts_with("B1"))
  
  #URBANIZATION (2010 data, not time-varying)
  cens_urban <- read_csv(paste0(root,"county_data/urban_popdens_2010.csv")) %>% 
                    dplyr::filter(state_code %in% states) %>% 
                    mutate(geog_metro=ifelse(rucc<=3,1,0),
                           geog_urb_non_metro =ifelse(rucc>=4 & rucc<=7,1,0),
                           geogF_rural=ifelse(rucc>=8,1,0))
  
  
  #STATE LEGISLATURE AND GOVERNOR (NCSL)
  #create dummies for R, D and divided control of legislature and state
  #for governor, it is either R or D, so only include a dummy for R governor
  state_leg <- read_csv(paste0(root,"state_government_2010_2019.csv")) %>% 
    mutate(pol_leg_R = ifelse(legis_control=="R",1,0),
           pol_leg_D = ifelse(legis_control=="D",1,0),
           polF_leg_Divided = ifelse(legis_control=="V",1,0),
           pol_gov_R = ifelse(gov_party=="R",1,0),
           pol_state_R = ifelse(state_control=="R",1,0),
           pol_state_D = ifelse(state_control=="D",1,0),
           polF_state_Divided = ifelse(state_control=="V",1,0)) %>% 
    inner_join(tigris::fips_codes[,c(1:4)] %>% unique(),
               by=c("state"="state_name")) %>% 
    rename("state_name"=state, "state"=state.y) %>% 
    mutate(fips=as.numeric(paste0(state_code,county_code))) %>% 
    select(-legis_control,-gov_party,-state_control) %>%
    dplyr::filter(state_code %in% sprintf("%02d",states)) %>% 
    dplyr::filter(year %in% years)
  
  state_leg[is.na(state_leg)] <- 0  #set Nas to 0 (e.g. Nebraska, unicameral)
  
  #CONNECTIVITY (FCC DATA)
  #already imported and cleaned with helper function
  connectivity <- connections_long %>% 
                    dplyr::filter(state_code %in% sprintf("%02d",states)) %>% 
                    dplyr::filter(year %in% years) 
  
  #PUTTING TOGETHER AND RETURNING FULL DATA FRAME
  #get rid of duplicate columns before joining
  duplicates <- c("state", "state_name", "state_code",
                  "county_code", "county", "county_name")
  
  cens_all <- cens_income %>% 
    
            inner_join(select(cens_race_eth_sex,-any_of(duplicates)), 
                       by=c("fips","year")) %>% 
            inner_join(select(cens_educ,-any_of(duplicates)), 
                       by=c("fips","year")) %>%
            inner_join(select(cens_age,-any_of(duplicates)), 
                       by=c("fips","year")) %>%
             inner_join(select(cens_labor,-any_of(duplicates)), 
                        by=c("fips","year")) %>%
            inner_join(select(cens_urban,-any_of(duplicates)), 
                       by=c("fips")) %>%
            inner_join(select(state_leg,-any_of(duplicates)), 
                       by=c("fips","year")) %>% 
            inner_join(select(connectivity,-any_of(duplicates)), 
                       by=c("fips","year")) %>% 
            
  return(cens_all %>% 
            relocate(c(8,9)))
}

#function to run two sample unequal variance t-tests on means
#only works on data frame resulting from function call of pull_all_data
#only works if the pull_all_data function call has two states
#returns data frame with means and p-values
t_tests <- function(censusdata){
  
  #race, ethnicity, sex
  res_white <- t.test(prop_white ~ state_name, data=censusdata)
  res_Fblack <- t.test(propF_black ~ state_name, data=censusdata)
  res_hisp <- t.test(prop_hisp ~ state_name, data=censusdata)
  res_male <- t.test(prop_male ~ state_name, data=censusdata)
  
  #education
  res_educ_lhs <- t.test(prop_less_hs ~ state_name, data=censusdata)
  res_educ_hs <- t.test(prop_hs_grad ~ state_name, data=censusdata)
  res_educ_coll <- t.test(prop_some_coll ~ state_name, data=censusdata)
  res_educ_bach <- t.test(prop_bachel ~ state_name, data=censusdata)
  res_educF_grad <- t.test(propF_grad ~ state_name, data=censusdata)
  
  #income, granular bins
  res_income <- t.test(median_hs_inc ~ state_name, data=censusdata)
  #res_inc_10_less <- t.test(inc_10_under ~ state_name, data=censusdata)
  #res_inc_10_19 <- t.test(inc_10_19 ~ state_name, data=censusdata)
  #res_inc_20_29 <- t.test(inc_20_29 ~ state_name, data=censusdata)
  #res_inc_30_39 <- t.test(inc_30_39 ~ state_name, data=censusdata)
  #res_inc_40_49 <- t.test(inc_40_49 ~ state_name, data=censusdata)
  #res_inc_50_59 <- t.test(inc_50_59 ~ state_name, data=censusdata)
  #res_inc_60_74 <- t.test(inc_60_74 ~ state_name, data=censusdata)
  #res_inc_75_99 <- t.test(inc_75_99 ~ state_name, data=censusdata)
  #res_inc_100_150 <- t.test(inc_100_150 ~ state_name, data=censusdata)
  #res_incF_150_over <- t.test(incF_150_over ~ state_name, data=censusdata)
  
  #income, aggregated bins
  res_inc2_20_less <- t.test(inc2_20_less ~ state_name, data=censusdata)
  res_inc2_20_39 <- t.test(inc2_20_39 ~ state_name, data=censusdata)
  res_inc2_40_59 <- t.test(inc2_40_59 ~ state_name, data=censusdata)
  res_inc2_60_99 <- t.test(inc2_60_99 ~ state_name, data=censusdata)
  res_inc2F_100_over <- t.test(inc2F_100_over ~ state_name, data=censusdata)
  
  #age, granular bins
  #res_age_17_under <- t.test(age_17_under ~ state_name, data=censusdata)
  #res_age_18_24 <- t.test(age_18_24 ~ state_name, data=censusdata)
  #res_age_25_34 <- t.test(age_25_34 ~ state_name, data=censusdata)
  #res_age_35_44 <- t.test(age_35_44 ~ state_name, data=censusdata)
  #res_age_45_54 <- t.test(age_45_54 ~ state_name, data=censusdata)
  #res_age_55_64 <- t.test(age_55_64 ~ state_name, data=censusdata)
  #res_age_65_74 <- t.test(age_65_74 ~ state_name, data=censusdata)
  #res_age_75_84 <- t.test(age_75_84 ~ state_name, data=censusdata)
  #res_age_85_over <- t.test(ageF_85_over ~ state_name, data=censusdata)
  
  #age, aggregated bins
  res_age2_17_under <- t.test(age2_17_under ~ state_name, data=censusdata)
  res_age2_18_34 <- t.test(age2_18_34 ~ state_name, data=censusdata)
  res_age2_35_54 <- t.test(age2_35_54 ~ state_name, data=censusdata)
  res_age2_55_74 <- t.test(age2_55_74 ~ state_name, data=censusdata)
  res_age2F_75_over <- t.test(age2F_75_over ~ state_name, data=censusdata)
  
  #labor force
  res_prop_poverty = t.test(prop_poverty ~ state_name, data=censusdata)
  res_prop_ue_rate = t.test(prop_ue_rate ~ state_name, data=censusdata)
  res_prop_particip_rate = t.test(prop_particip_rate ~ state_name, data=censusdata)
  #res_prop2_m_ue_rate = t.test(prop2_m_ue_rate ~ state_name, data=censusdata)
  #res_prop2_f_ue_rate = t.test(prop2_f_ue_rate ~ state_name, data=censusdata)
  #res_prop2_m_particip_rate = t.test(prop2_m_particip_rate ~ state_name, data=censusdata)
  #res_prop2_f_particip_rate = t.test(prop2_f_particip_rate ~ state_name, data=censusdata)
  
  #urbanization & connectivity
  res_urban <- t.test(pct_ua_uc ~ state_name, data=censusdata)
  #res_urban_area <- t.test(pct_ua ~ state_name, data=censusdata)
  #res_urban_cluster <- t.test(pct_uc ~ state_name, data=censusdata)
  res_rural <- t.test(pct_rural ~ state_name, data=censusdata)
  #res_pop_dens <- t.test(pop_dens ~ state_name, data=censusdata)
  res_hh_size <- t.test(avg_hh_size ~ state_name, data=censusdata)
  res_geog_metro <- t.test(geog_metro ~ state_name, data=censusdata)
  #res_geog_urb_non_metro <- t.test(geog_urb_non_metro ~ state_name, data=censusdata)
  res_geog_rural <- t.test(geogF_rural ~ state_name, data=censusdata)
  res_rucc <- t.test(rucc ~ state_name, data=censusdata)
  res_connect <- t.test(pcat ~ state_name, data=censusdata)
  
  #store results of all t_tests in diff in means table
  
  #store abbreviations for labeling table later
  state_abb <- censusdata %>% select(state,state_code) %>% unique()
  
  #initialize table
  t_tests <-  data.frame(matrix(1.00, 
                                nrow = length(ls(pattern="res_"))+1,
                                ncol = 4)) %>% 
    
    #rename variables appropriately
    rename(variable = X1, 
           Mean_1 = X2, Mean_2 = X3, p_value = X4) %>% 
    
    rename_with(~sub("Mean_1", paste0("Mean_",state_abb[1,1]),.x),
                cols=c(Mean_1)) %>% 
    
    rename_with(~sub("Mean_2", paste0("Mean_",state_abb[2,1]),.x),
                cols=c(Mean_2)) 
  
  observations <- censusdata$state %>%
    table() %>%
    as.numeric()
  
  #fill the table with appropriate values from all t-tests
  
  #fill first column with number of obs in each group
  t_tests[1,1] <- "n obs"
  t_tests[1,2:4] <- c(observations[1],observations[2],NA)
  
  #fill rest of the columns with coefs/pvalues from tests
  for (i in seq(1:length(ls(pattern="res_")))){  
    store <- mget(ls(pattern = "res_")[i])
    store <- store[[1]]
    t_tests[i+1,1] <- store$data.name %>% 
      str_extract("\\w*") %>% 
       tolower()
    est <- store$estimate %>% 
              as.numeric()
    t_tests[i+1,2] <- est[1]
    t_tests[i+1,3] <- est[2]
    t_tests[i+1,4] <- store$p.value
  }
  
  return(t_tests)
  
}


##(3)===============CENSUS ACS BROADBAND DATA======================#

#helper function to pull census broadband data (2013-2019)
#Taken from ACS 1 (note: this is only about 25% of counties) 
#Compared to 100% of counties in ACS5
#Unfortunately ACS 5 data not available prior to 2017 
pull_acs_internet <- function(years,states,api="acs1") {
  
  cens_internet <<- pull_acs_cty(years,
                                      c("B28008_004E", #total with broadband  
                                        "B28005_002E", #total under 18
                                        "B28005_005E", #total under 18 with broadband
                                        "B28005_008E", #total 18-64
                                        "B28005_011E", #total age 18-64 with broadband
                                        "B28005_014E", #total 65+
                                        "B28005_017E", #total age65+ with broadband
                                        "B28009A_001E", #total white
                                        "B28009A_004E", #total white with broadband
                                        "B28006_002E", #total less than high school
                                        "B28006_005E", #less than high school with broadband
                                        "B28006_008E", #total some college 
                                        "B28006_011E", #some college, with broadband
                                        "B28008_001E"),  #total population
                                      states,api) %>%
    mutate(prop_bb_total = B28008_004E/B28008_001E,
           prop_bb_under18 = B28005_005E/B28005_002E,
           prop_bb_18_64 = B28005_011E/B28005_008E,
           prop_bb_over65 = B28005_017E/B28005_014E,
           prop_bb_white = B28009A_004E/B28009A_001E,
           prop_bb_nonwhite = (B28008_004E-B28009A_004E)/(B28008_001E-B28009A_001E),
           prop_bb_lesshs = B28006_005E/B28006_002E,
           prop_bb_somecol = B28006_011E/B28006_008E) %>% 
    select(-starts_with("B2"))
}


#(4)================POLICY DATA======================#

#time series from 2013-2019
#all binary categorical vars for each year
#any indicates whether the state had any laws restricting muni broadband
#severe indicates whether the state had 2+ laws restricting muni broadband 
prep_policies <- function(){
  
    crosswalks <- read_csv(paste0(root,"county_data/fips_states.csv"))
  
    any_wide <<- read_csv(paste0(root,"policies/any_restrictions.csv"))
      
    severe_wide <<- read_csv(paste0(root,"policies/severe_restrictions.csv"))
    
    #for long form data, add county fips codes to facilitate joining later
    any_long <<- any_wide %>% gather(year,any_restrict,`2013`:`2019`) %>% 
      inner_join(crosswalks) %>% 
      mutate(year=as.numeric(year))
    
    severe_long <<- severe_wide %>% gather(year,severe_restrict,`2013`:`2019`) %>% 
      inner_join(crosswalks) %>% 
      mutate(year=as.numeric(year))
    
}

#(5)================BUILD PRIMARY DATASETS FOR ANALYSIS======================#

#Extract the main datasets and save them to the global environment 
#Run the data prep functions defined above 
prep_connections_data() #5 seconds
prep_policies()

#Build census data panels 
acs5_2013_2019 <<- pull_all_data(2013:2019,c(1:56),api="acs5") #25 seconds
acs1_2013_2019_connect  <<- pull_acs_internet(2013:2019,c(1:56),api="acs1")
acs5_2010 <<- pull_all_data(2010,c(5,21),api="acs5") #5 seconds
acs5_2012_2014 <<- pull_all_data(2012:2014,c(5,21),api="acs5") #15 seconds
acs5_2010_2019 <<- pull_all_data(2010:2019,c(5,21,40),api="acs5") #25 seconds

#Attach connectivity and policy data for regression analysis
panel_sample <- acs5_2013_2019 %>%  #start with full census data
  inner_join(any_long %>%
         select(-c("name","state","state_code")),
        by=c("fips","year")) %>%  #join with policy data
  inner_join(severe_long %>%
          select(-c("name","state","state_code")),
        by=c("fips","year")) %>% 
  inner_join(acs1_2013_2019_connect %>%   #join with acs 1 connectivity data
          select(-c("state","state_code","state_name",
                                "county_code", "county")),
        by=c("fips","year"))


#(6)======================TABLES AND PLOTS==============================#

#Prepare maps of connectivity and policies and store to global environment
prep_maps <- function(){
  
  library(tmap)
  library(spData)
  library(sf)
  library(tmaptools)
  library(rmapshaper)
  library(shinyjs)
  library(shiny)
  library(dplyr)
  
  #load shape files and functions 
  load(url("https://github.com/ltk2118/broadband_connectivity/blob/main/shape_files.RData?raw=true"))
  
  #if not already called, please call the following:
  #prep_connections_data() 
  #prep_policies()
  
  # Define the shape and the layer elements
  cuts <- c(0, 1, 2, 3, 4, 5)
  
  #generate map and store to global env
  fcc_pcat_map <<- tm_shape(shp, projection = 2163) +
    tm_polygons("pcat",
                breaks = cuts,
                palette = "BuPu", 
                border.col = "white", 
                border.alpha = 0.1,
                title="FCC Score") +
    tm_legend(legend.position = c("left","bottom")) + 
    tm_borders(col = "black",lwd=0.1) +
    tm_facets(by="year") +
    tm_layout(
      main.title = "Form 477 Connectvity, 2009-2019", 
      main.title.position = "left",
      main.title.size = 1)
  
  #facet map of restriction policies 
  any_state <- any_long %>% 
    select(-fips) %>% 
    unique() %>% 
    dplyr::filter(!state_code %in% c(11,66,60,69,72,78)) %>%
    dplyr::filter(year%in%c(2013,2019)) %>% 
    group_by(year,state,state_code) %>% 
    summarise(any_restrict=mean(any_restrict,na.rm=TRUE)) 
  
  #join with sf shape of states
  state_any_restrictions <- inner_join(contiguous_states,
                                       any_state,by=c("STUSPS"="state"))
  
  #generate map and store to global env
  any_restrictions_map <<- tm_shape(state_any_restrictions, projection = 5070) + 
    tm_polygons("any_restrict",palette = "-Spectral", style="fixed",
                breaks=c(-0.5,0.5,1.5),
                labels=c("No","Yes"),
                title = "Any restrictions") + 
    tm_facets(by="year") + 
    tm_layout(
      main.title = "State Municipal Broadband Restrictions", 
      main.title.position = "left",
      main.title.size = 1)
  
  #facet map of severe restriction policies
  severe_state <- severe_long %>% 
    select(-fips) %>% 
    unique() %>% 
    dplyr::filter(!state_code %in% c(11,66,60,69,72,78)) %>%
    dplyr::filter(year%in%c(2013,2019)) %>% 
    group_by(year,state,state_code) %>% 
    summarise(severe_restrict=mean(severe_restrict,na.rm=TRUE)) 
  
  #join with sf shape of states
  state_severe_restrictions <- inner_join(contiguous_states,
                                          severe_state,by=c("STUSPS"="state"))
  
  #generate map and store to global env
  severe_restrictions_map <<- tm_shape(state_severe_restrictions, projection = 5070) + 
    tm_polygons("severe_restrict",palette = "-Spectral", style="fixed",
                breaks=c(-0.5,0.5,1.5),
                labels=c("No","Yes"),
                title="Severe Restrictions") + 
    tm_facets(by="year") + 
    tm_layout(
      main.title = "State Municipal Broadband Restrictions", 
      main.title.position = "left",
      main.title.size = 1)
  
  library(dplyr) #put dplyr back on top (again)
  
}

#Coding for the FCC Form 477 Connectivity Score, to present as table
pcat_dictionary <<- read_csv(paste0(root,"county_data/pcat_dictionary.csv")) %>%
  rename(Score = `Code "pcat"`)

#Summary statistics, selected variables, 2013-2019, 
#function call direct to stargazer text output
summary_stats_fcc <- function(){
  
  #subset and summarize to stargazer output
  fcc_scores_2013_2019 <- acs5_2013_2019 %>% 
    select(fips, pcat, year) %>% spread(year,pcat) %>% 
    select(-fips)
  
  stargazer(fcc_scores_2013_2019, type="latex",
            summary.stat = c("n", "mean", "sd"),
            font.size = "small",
            header = FALSE,
            no.space = TRUE,
            digits = 2, title = "FCC Connectivity Scores")
                              
}
summary_stats_acs5 <- function(){
  
  #subset and summarize to stargazer output
    sub_acs5_2013_2019 <- acs5_2013_2019 %>% 
      select(starts_with("prop_")|starts_with("propF")|
               starts_with("age2")|starts_with("inc2")|
               starts_with("pct")|starts_with("geog")|
               starts_with("avg")|starts_with("pol")|
               starts_with("pcat")|starts_with("pct_")|
               starts_with("pop_")) %>% 
      relocate(c(38,39)) %>% 
      relocate(13,.after=3) %>% 
      relocate(14,.after=9) %>%
      relocate(40,.after=24) %>% 
      mutate(pop_total = pop_total/1000)
    
    stargazer(sub_acs5_2013_2019, type="latex",
              summary.stat = c("n", "mean", "sd", "min", "max"),
              digits = 2, notes = "County-year observations, 2013-2019",
              title = "Summary Statistics, ACS5 Estimates 2013-2019",
              font.size = "footnotesize",
              header = FALSE,
              no.space = TRUE,
              covariate.labels = c("FCC Connectivity Score",
                                   "total population 000s",
                                   "% white",
                                   "% black",
                                   "% hispanic",
                                   "% male",
                                   "% education less than high school",
                                   "% education high school grad",
                                   "% education some college",
                                   "% education bachelor degree",
                                   "% education graduate degree",
                                   "% below poverty line",
                                   "unemployment rate",
                                   "participation rate",
                                   "% age 17 or under",
                                   "% age 18-34",
                                   "% age 35-54",
                                   "% age 55-74",
                                   "% age 75 or over",
                                   "% income 20k or less",
                                   "% income 20-39.999k",
                                   "% income 40-59.999k",
                                   "% income 60-99.999k",
                                   "% income 100k or more",
                                   "population density",
                                   "% live urban",
                                   "% live urbanized areas",
                                   "% live urbanized clusters",
                                   "% live rural areas",
                                   "% counties metro",
                                   "% counties non-metro urban",
                                   "% counties rural",
                                   "average household size",
                                   "Rep state legislature",
                                   "Dem state legislature",
                                   "Divided legislature",
                                   "Rep governor",
                                   "Rep state control",
                                   "Dem state control",
                                   "Divided control"))

}
summary_stats_acs1 <- function(){
  
  acs1_broadband_2013_2019 <- acs1_2013_2019_connect  %>% 
    select(fips, prop_bb_total, year) %>% spread(year,prop_bb_total) %>% 
    select(-fips) %>% drop_na()
  
  
  stargazer(acs1_broadband_2013_2019, type="latex",
            summary.stat = c("n", "mean", "sd","min","max"),
            digits = 2, notes="County-year observations, 2013-2019",
            title = "Proportion of Households with Broadband, ACS 1",
            font.size = "footnotesize",
            header = FALSE,
            no.space = TRUE)
           
}

#store t-test table for difference in means to global
diff_in_means <- function(){
  library(dplyr)
  t_2010 <- t_tests(acs5_2010) %>% 
    rename(Metric=variable, `Mean 2010 (AR)`=Mean_AR, 
           `Mean 2010 (KY)`=Mean_KY, `p-value`=p_value)
  t_2010[c(18,19,8,9,10,11,13,14:31),]<-t_2010[c(19,18,11,10,9,8,31,13:30),]
  t_2010[c(19,20,30,31),] <- t_2010[c(20,19,31,30),]
  t_2010$Metric <- c("Counties","Age 17 & Under", "Age 18-34","Age 35-54","Age 55-74", "Age 75 & Over",
                       "Connectivity Score",  "% Less than High School", "% High School Grad",
                       "% Some College",  "% Bachelor's Degree", "% Graduate Degree",
                       "% White", "% Black", "% Metro", "% Rural", "Average Household Size",
                       "% Hispanic", "% Income 20k or less", "% Income 20-39.999k", "% Income 40-59.999k",
                       "% Income 60-99.999k", "% Income over 100k", "Median Houeshold Income", "% Male",
                       "Participation Rate", "Poverty Rate", "Unemployment Rate", "RUCC Score", 
                       "% Live Urban", "% Live Rural")
  
  t_2010_output <<- t_2010  
}
make_plots <- function(){
  
  library(ggthemes)
  
  #visualize changes in connectivity over the period
  #time series plot
  DiD_timeseries <<- acs5_2010_2019 %>% 
    group_by(state_name,year) %>% 
    summarise(mean_pcat=mean(pcat,na.rm=TRUE)) %>% 
    ggplot() +
    aes(y=mean_pcat, x=year, col=state_name) +
    geom_line() + 
    scale_x_continuous(breaks=seq(2010,2020,by=2)) +
    labs(color = "State") +
    ylab("Mean FCC Score") +
    xlab("Year") + 
    ggtitle("Connectivity over time, AR, KY and OK",
            subtitle = 2010-2019) +
    theme_calc()
  
  #bar plot for differences over DiD comparison period
  #use t-test function to return group means 
  #KY connectivity seems to have increased by more over the period
  DiD_bar <<- t_tests(acs5_2010)  %>% 
    select(-p_value) %>% 
    filter(variable=="pcat") %>% 
    mutate(period="pre") %>% 
    rbind(t_tests(acs5_2012_2014) %>% 
            mutate(period="post") %>%  
            select(-p_value) %>% 
            filter(variable=="pcat")) %>%
    rename(AR=Mean_AR, KY=Mean_KY) %>% 
    gather(state,mean_connect,AR:KY) %>% 
    ggplot() +
    aes(y=mean_connect,x=state,
        fill=forcats::fct_rev(as.factor(period)))+
    geom_bar(stat="identity",position="dodge") + 
    labs(fill = "Period") +
    theme(legend.position = "bottom") + 
    ylab("Mean FCC Score") +
    xlab("State") + 
    ggtitle("Connectivity in Arkansas (AR) and Kentucky (KY)",
            subtitle = "Pre & Post Arkansas Municipal Broadband Ban") + 
    theme_calc()
  
}

#(7)======================DIFF IN DIFF==============================#

#stargazer output for the DiD Regression analysis 
#function call direct to stargazer text output
prep_DiD <- function(post){
  
  #Treatment: AR
  #Control: KY
  #Before: 2010
  #After: 2012-2014 mean
  
  #pull required data and take means 
  #recode state as binary categorical (1=AK, 0=KY)
  #recode before/after as binary categorical (0=Before, 1=After)
  #collapse into two obs for each county by taking means
  #doing this because "after" is defined as the average of 2012-2014
  
  cens_DiD <- pull_all_data(c(2010,post),c(5,21),api="acs5") %>% 
    mutate(after = ifelse(year==2010,0,1)) %>% 
    mutate(treat = ifelse(state=="AR",1,0)) %>%
    select(-year) %>% 
    group_by(api,state,county,fips,after,treat) %>% 
    summarise(across(median_hs_inc:pcat,~mean(.x,na.rm = TRUE)))
  
  #Regression 1, without covariates
  DiD_1 <- lm(formula=pcat~treat+after+treat*after,
              data=cens_DiD)
  
  #Regression 2, same but weighting by county population
  DiD_2 <- lm(formula=pcat~treat+after+treat*after,
              weights = pop_total,
              data=cens_DiD)
  
  #Regression 3, add covariates
  covariates <- colnames(cens_DiD) %>% 
    stringr::str_extract("age2\\_.*|inc2\\_.*|prop\\_.*|avg.*|geog\\_.*") %>% 
    as.data.frame() %>% drop_na() %>% unlist() %>% 
    paste(collapse = " + ")
  
  DiD_3 <- lm(formula=paste("pcat~treat+after+treat*after+",covariates),
              weights = pop_total,
              data=cens_DiD)

  stargazer(DiD_1,DiD_2,DiD_3,type="latex",
            dep.var.labels.include = FALSE,
            dep.var.caption = "Mean FCC Score",
            keep = c("treat","after"),
            covariate.labels = c("Restrict", "Post","Restrict x Post"),
            title = paste("Difference in Difference Estimates, Post =",
                          paste(as.character(post)[1],as.character(post)[length(as.character(post))],sep="-")),
            digits=2,
            font.size = "small",
            header = FALSE,
            no.space = TRUE,
            add.lines = list(c("Pop Weights", "No","Yes","Yes"),
                             c("Controls","No","No","Yes")),
            df=FALSE)
}

#(8)======================PANEL REGRESSION==============================#

#a helpful function to cluster SEs; specify "state" or "fips" (county) level
cluster_ses <- function(regression_object,cluster_unit,data=panel_sample){
  coeftest(regression_object, 
           cluster.vcov(regression_object,
                        cbind(data[,which(colnames(data)==cluster_unit)]),
                        df_correction = T))[,2]
}

#regressions for "any restrictions"
panel_regs_any <- function(){
  #now we use a much better metric as the outcome variable
  #census data on % of households with a broadband connection, 2013-2019
  #and granular data on % of different subgroups with broadband connections
  #the only problem is that this data is only available via ACS1
  #so the sample size is reduced by 75% (only 1/4 of counties sampled)
  #i.e. there is a trade-off
  
  library(multiwayvcov)
  library(sandwich)
  library(lmtest)
  
  
  #PREPARE COVARIATES 
  all_covariates <- colnames(acs5_2013_2019) %>% 
    str_extract("age\\_.*|inc\\_.*|prop\\_.*|avg.*|pol\\_.*") %>% 
    as.data.frame() %>% drop_na() %>% unlist() %>% 
    paste(collapse = " + ")
  
  politics_covariates <- colnames(acs5_2013_2019) %>% 
    str_extract("pol\\_.*") %>% 
    as.data.frame() %>% drop_na() %>% unlist() %>% 
    paste(collapse = " + ")
  
  income_covariates <- colnames(acs5_2013_2019) %>% 
    str_extract("inc\\_.*") %>% 
    as.data.frame() %>% drop_na() %>% unlist() %>% 
    paste(collapse = " + ")
  
  age_covariates <- colnames(acs5_2013_2019) %>% 
    str_extract("age\\_.*") %>% 
    as.data.frame() %>% drop_na() %>% unlist() %>% 
    paste(collapse = " + ")
  
  education_covariates <- colnames(acs5_2013_2019) %>% 
    str_extract("prop\\_less.*|prop\\_hs.*|prop\\_some.*|prop\\_bach.*") %>% 
    as.data.frame() %>% drop_na() %>% unlist() %>% 
    paste(collapse = " + ")
  
  labmkt_covariates <- colnames(acs5_2013_2019) %>% 
    str_extract(".*\\_rate") %>% 
    as.data.frame() %>% drop_na() %>% unlist() %>% 
    paste(collapse = " + ")
  
  
  ##Store panel objects locally for stargazer output
  
  panel_1a <- lm(formula= prop_bb_total ~ any_restrict,
                 weights=pop_total,
                 data=panel_sample)
  
  panel_1b <- lm(formula= prop_bb_total ~ any_restrict + 
                   as.factor(year),
                 weights=pop_total,
                 data=panel_sample)
  
  panel_1c <- lm(formula= prop_bb_total ~ any_restrict + 
                   as.factor(fips),
                 weights=pop_total,
                 data=panel_sample)
  
  
  panel_1d <- lm(formula= prop_bb_total ~ any_restrict + 
                   as.factor(fips) + as.factor(year),
                 weights=pop_total,
                 data=panel_sample)
  
  panel_1e <- lm(formula= paste0("prop_bb_total ~ any_restrict+as.factor(fips) + as.factor(year)+",
                                 politics_covariates),
                 weights=pop_total,
                 data=panel_sample)
  
  panel_1f <- lm(formula= paste0("prop_bb_total ~ any_restrict+as.factor(fips) + as.factor(year)+",
                                 age_covariates),
                 weights=pop_total,
                 data=panel_sample)
  
  
  panel_1g <- lm(formula= paste0("prop_bb_total ~ any_restrict+as.factor(fips) + as.factor(year)+",
                                 income_covariates,"+",labmkt_covariates,"+",education_covariates),
                 weights=pop_total,
                 data=panel_sample)
  
  
  panel_1h <- lm(formula= paste0("prop_bb_total ~ any_restrict+as.factor(fips) + as.factor(year)+",
                                 all_covariates),
                 weights=pop_total,
                 data=panel_sample)
  
  #use state-level clustered standard errors
  seANY <<- list(cluster_ses(panel_1a,"state"), 
              cluster_ses(panel_1b,"state"),
              cluster_ses(panel_1c,"state"),
              cluster_ses(panel_1d,"state"),
              cluster_ses(panel_1e,"state"),
              cluster_ses(panel_1f,"state"),
              cluster_ses(panel_1g,"state"),
              cluster_ses(panel_1h,"state"))
  
  #compile list of lm objects
  panelsANY_1a_to_1h <<- list(panel_1a,
                          panel_1b,
                          panel_1c,
                          panel_1d,
                          panel_1e,
                          panel_1f,
                          panel_1g,
                          panel_1h)
}
#function call direct to stargazer text output
panel_any_out <- function(){
  stargazer(panelsANY_1a_to_1h,
            dep.var.labels.include = FALSE,
            dep.var.caption = "Proportion of Households with a Broadband Connection",
            keep.stat = c("adj.rsq","n","f"),
            covariate.labels = "Any Restriction",
            add.lines=list(c("State FEs","No","No","Yes","Yes","Yes","Yes","Yes","Yes"),
                           c("Year FEs","No","Yes","No","Yes","Yes","Yes","Yes","Yes"),
                           c("Politics Controls","No","No","No","No","Yes","No","No","Yes"),
                           c("Age Controls","No","No","No","No","No","Yes","No","Yes"),
                           c("Income, Educ Controls","No","No","No","No","No","No","Yes","Yes"),
                           c("Urbanization Controls","No","No","No","No","No","No","No","Yes")),
            type="latex",
            se=seANY,
            digits = 3,
            df=FALSE,
            font.size = "footnotesize",
            header = FALSE,
            no.space = TRUE,
            column.sep.width = "1pt",
            keep=c("any_restrict","Constant"),
            title="Panel regressions: any restrictions vs connectivity outcomes")
}


#regressions for "severe restrictions"
panel_regs_severe <- function(){
  #now we use a much better metric as the outcome variable
  #census data on % of households with a broadband connection, 2013-2019
  #and granular data on % of different subgroups with broadband connections
  #the only problem is that this data is only available via ACS1
  #so the sample size is reduced by 75% (only 1/4 of counties sampled)
  #i.e. there is a trade-off
  
  library(multiwayvcov)
  library(sandwich)
  library(lmtest)
  
  #PREPARE COVARIATES
  all_covariates <- colnames(acs5_2013_2019) %>% 
    str_extract("age\\_.*|inc\\_.*|prop\\_.*|avg.*|pol\\_.*") %>% 
    as.data.frame() %>% drop_na() %>% unlist() %>% 
    paste(collapse = " + ")
  
  politics_covariates <- colnames(acs5_2013_2019) %>% 
    str_extract("pol\\_.*") %>% 
    as.data.frame() %>% drop_na() %>% unlist() %>% 
    paste(collapse = " + ")
  
  income_covariates <- colnames(acs5_2013_2019) %>% 
    str_extract("inc\\_.*") %>% 
    as.data.frame() %>% drop_na() %>% unlist() %>% 
    paste(collapse = " + ")
  
  age_covariates <- colnames(acs5_2013_2019) %>% 
    str_extract("age\\_.*") %>% 
    as.data.frame() %>% drop_na() %>% unlist() %>% 
    paste(collapse = " + ")
  
  education_covariates <- colnames(acs5_2013_2019) %>% 
    str_extract("prop\\_less.*|prop\\_hs.*|prop\\_some.*|prop\\_bach.*") %>% 
    as.data.frame() %>% drop_na() %>% unlist() %>% 
    paste(collapse = " + ")
  
  labmkt_covariates <- colnames(acs5_2013_2019) %>% 
    str_extract(".*\\_rate") %>% 
    as.data.frame() %>% drop_na() %>% unlist() %>% 
    paste(collapse = " + ")
  
  panel_1a <- lm(formula= prop_bb_total ~ severe_restrict,
                 weights=pop_total,
                 data=panel_sample)
  
  panel_1b <- lm(formula= prop_bb_total ~ severe_restrict + 
                   as.factor(year),
                 weights=pop_total,
                 data=panel_sample)
  
  panel_1c <- lm(formula= prop_bb_total ~ severe_restrict + 
                   as.factor(fips),
                 weights=pop_total,
                 data=panel_sample)
  
  
  panel_1d <- lm(formula= prop_bb_total ~ severe_restrict + 
                   as.factor(fips) + as.factor(year),
                 weights=pop_total,
                 data=panel_sample)
  
  panel_1e <- lm(formula= paste0("prop_bb_total ~ severe_restrict+as.factor(fips) + as.factor(year)+",
                                 politics_covariates),
                 weights=pop_total,
                 data=panel_sample)
  
  panel_1f <- lm(formula= paste0("prop_bb_total ~ severe_restrict+as.factor(fips) + as.factor(year)+",
                                 age_covariates),
                 weights=pop_total,
                 data=panel_sample)
  
  
  panel_1g <- lm(formula= paste0("prop_bb_total ~ severe_restrict+as.factor(fips) + as.factor(year)+",
                                 income_covariates,"+",labmkt_covariates,"+",education_covariates),
                 weights=pop_total,
                 data=panel_sample)
  
  
  panel_1h <- lm(formula= paste0("prop_bb_total ~ severe_restrict+as.factor(fips) + as.factor(year)+",
                                 all_covariates),
                 weights=pop_total,
                 data=panel_sample)
  
  seSEV <<- list(cluster_ses(panel_1a,"state"), 
              cluster_ses(panel_1b,"state"),
              cluster_ses(panel_1c,"state"),
              cluster_ses(panel_1d,"state"),
              cluster_ses(panel_1e,"state"),
              cluster_ses(panel_1f,"state"),
              cluster_ses(panel_1g,"state"),
              cluster_ses(panel_1h,"state"))
  
  panelsSEV_1a_to_1h <<- list(panel_1a,
                          panel_1b,
                          panel_1c,
                          panel_1d,
                          panel_1e,
                          panel_1f,
                          panel_1g,
                          panel_1h)
}
#function call direct to stargazer text output
panel_severe_out <- function(){
  stargazer(panelsSEV_1a_to_1h,
            dep.var.labels.include = FALSE,
            dep.var.caption = "Proportion of Households with a Broadband Connection",
            keep.stat = c("adj.rsq","n","f"),
            covariate.labels = "Severe Restriction",
            add.lines=list(c("State FEs","No","No","Yes","Yes","Yes","Yes","Yes","Yes"),
                           c("Year FEs","No","Yes","No","Yes","Yes","Yes","Yes","Yes"),
                           c("Politics Controls","No","No","No","No","Yes","No","No","Yes"),
                           c("Age Controls","No","No","No","No","No","Yes","No","Yes"),
                           c("Income, Educ Controls","No","No","No","No","No","No","Yes","Yes"),
                           c("Urbanization Controls","No","No","No","No","No","No","No","Yes")),
            type="latex",
            se=seSEV,
            digits = 3,
            df=FALSE,
            font.size = "footnotesize",
            header = FALSE,
            no.space = TRUE,
            column.sep.width = "1pt",
            keep=c("severe_restrict","Intercept"),
            title="Panel regressions: severe restrictions vs connectivity outcomes")
}





