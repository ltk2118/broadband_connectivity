# broadband_connectivity

A project completed for Columbia University's School of International and Public Affairs **SIPAU6614 Data Analysis for Policy Research using R**.

### How do restrictions on municipal broadband affect connectivity outcomes?

The venturing of local governments (municipalities) into the broadband market has proven to be a controversial (and partisan) issue. Opening the broadband market, which had previously always been the realm of private telecommunications companies, to public entities, has landed policymakers in the crosshairs of telecommunications companies as well as other interest groups.

We utilize two datasets on tract-level internet access services connections (FCC) and household internet access (ACS-5), examining variation in access rates for states with and without municipal broadband restrictions from 2013-2019.

Our empirical strategy is divided into two parts. First, we exploit an exogenous policy variation in Arkansas to estimate a Difference in Difference (DiD) model. Second, we employ panel regression techniques with state fixed effects and clustered standard errors. We find no evidence that severe municipal restrictions have an impact on the proportion of households with a broadband connection. Indeed, policy variation along this axis explains almost none of the variation in observed connectivity outcomes.

## Contents

1. Presentation (in .html - ioslides)
2. Report (in .pdf and .Rmd)
3. `census_pulls.R` - a function to retrieve data from through the American Community Survey (ACS) API.
4. `data_prep.R` - a script for data cleaning.
5. Data files (county and tract connectivity data, and municipal broadband restrictiveness coding).