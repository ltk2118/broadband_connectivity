library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(stargazer)

#prepares EDA and saves plots/tables to global
prep_eda <- function(){
    
    #make dfs of averages
    state_avg_connections <<- connections_wide %>% 
      mutate(delta_connect = (pcat_19-pcat_15)/5) %>% 
      group_by(state) %>% 
      summarise(delta_connect = mean(delta_connect,na.rm=TRUE))
    
    state_avg_restrictiveness <<- restrictions_wide %>% 
      mutate(delta_res = `2019`-`2015`)
    
    state_avg_funds <<- funds_wide %>% 
      mutate(delta_funds = `2019`-`2015`)
    
    #plot 2019 restrictions x connectivity
    connections_wide %>% 
      inner_join(state_avg_restrictiveness, by=c("state"="state")) %>% 
        ggplot() +
          aes(x=`2019`,y=pcat_19) +
          geom_point() + 
          geom_smooth(method="lm",formula=y~x) +
          ggtitle("Simple Differences, Scatter")
      
    #plot 2019 funds (level) x connectivity
    connections_wide %>% 
      inner_join(state_avg_funds, by=c("state"="state")) %>% 
        ggplot() +
          aes(x=`2019`,y=pcat_19) +
          geom_point() + 
          geom_smooth(method="lm",formula=y~x) +
          ggtitle("Simple Differences, Scatter")
        
    #delta restrictions x delta connectivity
    plot_1 <<- state_avg_connections %>% 
      inner_join(state_avg_restrictiveness, by=c("state"="state")) %>% 
      select(-law,-(`2015`:`2019`)) %>% 
      ungroup() %>% 
        ggplot() +
          aes(x=delta_res,y=delta_connect,label=state) +
          ylab("Avg yearly change in connectivity score") + 
          xlab("Change in restrictiveness score") +
          theme(axis.title.x = element_text(size=8)) +
          theme(axis.title.y = element_text(size=8)) +
          geom_point() + 
          geom_smooth(method="lm",formula=y~x,col="red") +
          geom_text_repel(size=2) +
          labs(subtitle="Simple Differences (Restrictiveness), Scatter") 
    
    #delta funds x delta connectivity
    plot_2 <<- state_avg_connections %>% 
      inner_join(state_avg_funds, by=c("state"="state")) %>% 
      select(-(`2015`:`2019`)) %>% ungroup() %>% 
        ggplot() +
          aes(x=delta_funds,y=delta_connect,label=state) +
          ylab("Avg yearly change in connectivity score") + 
          xlab("Change in funding status") +
          theme(axis.title.x = element_text(size=8)) +
          theme(axis.title.y = element_text(size=8)) +
          geom_point() + 
          geom_smooth(method="lm",formula=y~x,col="red") +
          geom_text_repel(size=2) +
          labs(subtitle="Simple Differences (Funds), Scatter") 
    
    #county level dif in means for res
    table_1 <<- connections_wide %>% 
      inner_join(state_avg_restrictiveness, by=c("state"="state")) %>% 
      mutate(delta_connect = (pcat_19-pcat_15)/5) %>%
      group_by(delta_res) %>% 
      summarise(`avg connectivity change`= mean(delta_connect),
                  n=n()) %>% 
      knitr::kable()
    
    #county level diff in means for funds
    table_2 <<- connections_wide %>% 
      inner_join(state_avg_funds, by=c("state"="state")) %>% 
      mutate(delta_connect = (pcat_19-pcat_15)/5) %>%
      group_by(delta_funds) %>% 
      summarise(`avg connect change`=mean(delta_connect),
                n=n()) %>% 
      knitr::kable()
}

#prepares regressions and saves to global for direct call with stargazer
prep_regressions <- function(){
  
  #prepare full dataset for regression analysis
  full_data <<- connections_wide %>% 
    inner_join(state_avg_funds, by=c("state"="state")) %>% 
    inner_join(state_avg_restrictiveness, 
               by=c("state"="state"),
               suffix=c("_fun","_res")) %>% 
    inner_join(full_controls,by=c("fips"="fips")) %>% 
    select(-state_code.x,-state.x,-law,-name_res,-name_fun) %>% 
    rename(state_code=state_code.y,state=state.y) %>% 
    mutate(delta_connect = (pcat_19-pcat_15)/5)

  #first against change in funds, witih 6 specifications
  lm1 <<- lm(delta_connect ~ delta_funds,
            data=full_data, weights = Population)
  
  lm2 <<- lm(delta_connect ~ delta_funds + l_sh_pop_edu_c + inc19 + Popdens,
            data=full_data, weights = Population)
  
  lm3 <<- lm(delta_connect ~ delta_funds + l_sh_pop_edu_c + inc19 + Popdens +
              l_sh_pop_age_1019 +
              l_sh_pop_age_2029 +
              l_sh_pop_age_3039 +
              l_sh_pop_age_4049 +
              l_sh_pop_age_5059 +
              l_sh_pop_age_6069 +
              l_sh_pop_age_7079 +
              l_sh_pop_white + 
              l_sh_pop_black +
              l_sh_pop_asian + 
              l_sh_pop_hispanic,
              data=full_data, weights = Population)
  lm4 <<- lm(delta_connect ~ delta_funds + l_sh_pop_edu_c + inc19 + Popdens +
              l_sh_pop_age_1019 +
              l_sh_pop_age_2029 +
              l_sh_pop_age_3039 +
              l_sh_pop_age_4049 +
              l_sh_pop_age_5059 +
              l_sh_pop_age_6069 +
              l_sh_pop_age_7079 +
              l_sh_pop_white + 
              l_sh_pop_black +
              l_sh_pop_asian + 
              l_sh_pop_hispanic + 
              l_shind_manuf_cbp + 
              l_sh_routine33 + 
              l_task_outsource +
              l_sh_pop_f,
              data=full_data, weights = Population)
  
  lm5 <<- lm(delta_connect ~ delta_funds + l_sh_pop_edu_c + inc19 + Popdens +
              majority_white + rep_control,
                 data=full_data, weights = Population)
  lm6 <<- lm(delta_connect ~ delta_funds + l_sh_pop_edu_c + inc19 + Popdens +
              reg_midatl + reg_encen + reg_wncen + reg_escen + 
              reg_mount + reg_pacif,
            data=full_data, weights = Population)
  
  #second against change in restrictiveness, same 6 specifications
  lm7 <<- lm(delta_connect ~ delta_res,
            data=full_data, weights = Population)
  
  lm8 <<- lm(delta_connect ~ delta_res + l_sh_pop_edu_c + inc19 + Popdens,
            data=full_data, weights = Population)
  
  lm9 <<- lm(delta_connect ~ delta_res + l_sh_pop_edu_c + inc19 + Popdens +
              l_sh_pop_age_1019 +
              l_sh_pop_age_2029 +
              l_sh_pop_age_3039 +
              l_sh_pop_age_4049 +
              l_sh_pop_age_5059 +
              l_sh_pop_age_6069 +
              l_sh_pop_age_7079 +
              l_sh_pop_white + 
              l_sh_pop_black +
              l_sh_pop_asian + 
              l_sh_pop_hispanic,
              data=full_data, weights = Population)
  
  lm10 <<- lm(delta_connect ~ delta_res + l_sh_pop_edu_c + inc19 + Popdens +
               l_sh_pop_age_1019 +
               l_sh_pop_age_2029 +
               l_sh_pop_age_3039 +
               l_sh_pop_age_4049 +
               l_sh_pop_age_5059 +
               l_sh_pop_age_6069 +
               l_sh_pop_age_7079 +
               l_sh_pop_white + 
               l_sh_pop_black +
               l_sh_pop_asian + 
               l_sh_pop_hispanic + 
               l_shind_manuf_cbp + 
               l_sh_routine33 + 
               l_task_outsource +
               l_sh_pop_f,
               data=full_data, weights = Population)
  
  lm11 <<- lm(delta_connect ~ delta_res + l_sh_pop_edu_c + inc19 + Popdens +
               majority_white + rep_control,
             data=full_data, weights = Population)
  
  lm12 <<- lm(delta_connect ~ delta_res + l_sh_pop_edu_c + inc19 + Popdens +
               reg_midatl + reg_encen + reg_wncen + reg_escen + 
               reg_mount + reg_pacif,
               data=full_data, weights = Population)
}


