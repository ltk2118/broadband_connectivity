#Output for u6614 final project
#April 2020, LTK

#source files
source("Regressions.R")
source("Prep_data.R")

#initialize everything
prep_connections_data()
prep_policies()
prep_controls()
prep_eda()
prep_regressions()

#simple diffs, change in restrictiveness vs change in connectivity scatter
plot_1

#simple diffs, change in funds vs change in connectivity scatter
plot_2

#difference in means table for connectivity, by change in restrictiveness
table_1

#difference in means table for connectivity, by change in funds
table_2

#simple difference regressions of connectivity against change in funds
stargazer(lm1,lm2,lm3,lm4,lm5,lm6,type="text",keep=c("delta_funds"),
          keep.stat=c("f","adj.rsq"),
          add.lines=list(c("IncomeEduc","No","Yes","Yes","Yes","Yes","Yes"),
                         c("Demographic","No","No","Yes","Yes","No","No"),
                         c("Labor force","No","No","No","Yes","Yes","Yes"),
                         c("Political","No","No","No","No","Yes","No"),
                         c("Geographic","No","No","No","No","No","Yes")))

#simple difference regressions of connectivity against change in restrictiveness
stargazer(lm7,lm8,lm9,lm10,lm11,lm12,type="text",keep=c("delta_res"),
          keep.stat=c("f","adj.rsq"),
          add.lines=list(c("IncomeEduc","No","Yes","Yes","Yes","Yes","Yes"),
                         c("Demographic","No","No","Yes","Yes","No","No"),
                         c("Labor force","No","No","No","Yes","Yes","Yes"),
                         c("Political","No","No","No","No","Yes","No"),
                         c("Geographic","No","No","No","No","No","Yes")))