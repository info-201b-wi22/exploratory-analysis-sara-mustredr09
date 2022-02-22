project_data<-read.csv("https://raw.githubusercontent.com/info-201b-wi22/exploratory-analysis-sara-mustredr09/main/Juvenile_Justice_Dashboard_-_HS_Completion.csv?token=GHSAT0AAAAAABQIEWSA6BKJCPJ3VN3C5VHSYQYGLCA")

library(tidyverse)
library(dplyr)
library(ggplot2)

# A dodged bar chart showing the high school outcomes for each offender type
# (justice-involved, not justice-involved, juvenile offender, status offender)
avg_pct_offenders <- project_data %>%
  filter(DemographicGroup == "All Students") %>% 
  filter(DemographicValue == "All Students") %>% 
  filter(CohortYearTTL > 2015) %>% 
  rename("Percent_of_Cohort" = "Pct") %>% 
  rename("Justice_Involvement" = "JJOffenderType")


ggplot(data = avg_pct_offenders) +
  geom_col(
    mapping = aes(x = Justice_Involvement, y = Percent_of_Cohort, fill = HSOutcome), position = "dodge"
  ) + scale_y_continuous(breaks = seq(0, .83, .1)) + facet_wrap(~CohortYearTTL)

  

