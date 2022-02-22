project_data<-read.csv("https://raw.githubusercontent.com/info-201b-wi22/exploratory-analysis-sara-mustredr09/main/Juvenile_Justice_Dashboard_-_HS_Completion.csv?token=GHSAT0AAAAAABQIEWSA6BKJCPJ3VN3C5VHSYQYGLCA")

library(tidyr)
library(tidyverse)
hs_outcome_table <- project_data %>% 
  group_by(CohortYearTTL) %>% 
  filter(DemographicGroup == "Race/Ethnicity") %>% 
  filter(JJOffenderType == "Not Justice Involved") %>% 
  filter(HSOutcome == "HS Diploma") %>% 
  rename("Pct_Not_Justice_Involved" = "Pct") %>% 
  select(-c(DemographicGroup, JJOffenderType, RedactedPct, HSOutcome)) %>% 
  rename("Race/Ethnicity" = "DemographicValue")
 
view(hs_outcome_table)
