##=================CENSUS API PULL FUNCTIONS=================#

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
