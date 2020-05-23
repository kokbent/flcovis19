# covis19

Visualizations (and some analysis) to support local efforts to mitigate effects of the COVID-19 pandemic. 

## Approach 

Our approach will be to begin with a small sample of counties (Miami Dade, Broward, Sumter, Alachua), and then proceed to all-county and state-wide analyses.  We want to support the communication of FL-specific information to news organizations and decision-makers.  

## Initial targets

Our initial targets were set at the first meeting, 21 May, 2020.   

1. Deploy an awesome visualization for recent new cases.  Find the right data and develop graphic conventions to illustrate new case dynamics meaningfully.  Make FL case curves available online. 
   * use FL data for counties with event date 
   * add a smoothed line
   * optionally, address weekend effect graphically
   * denote the uncertainty of the past X days (e.g., gray out)
   * create versions for all counties, and statewide
   * publish this to a server
   * if data are auto-updated, make it auto-update
1. Integrate mobility data to support forecasting by locale.  Develop and evaluate visualization (and possibly, modeling) approaches to anticipate changes in healthcare burden based on changes in mobility.  
   * evaluate sources of mobility data
   * acquire useful county-specific mobility data
   * address admissions, cases, and tests
   * consider weekend effect in mobility data
   * use variability by county to evaluate predictability of burden 
1. Integrate the influence of co-morbidities and healthcare disparities.  Find out how county-specific burden is influenced by disparities and co-morbidities.  
   * consider effects of age 
   * consider minority distributions from census and American Community Survey
   * include comorbidity data from that survey via DoH
   * integrate information on under-served areas
