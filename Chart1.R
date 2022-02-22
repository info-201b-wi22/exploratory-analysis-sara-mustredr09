project_data<-read.csv("https://raw.githubusercontent.com/info-201b-wi22/exploratory-analysis-sara-mustredr09/main/Juvenile_Justice_Dashboard_-_HS_Completion.csv?token=GHSAT0AAAAAABQIEWSA6BKJCPJ3VN3C5VHSYQYGLCA")

library(tidyverse)
library(dplyr)
library(ggplot2)

# A dodged bar chart showing the high school outcomes for each offender type
# (justice-involved, not justice-involved, juvenile offender, status offender)
avg_pct_offenders <- project_data %>%
  filter(DemographicGroup == "All Students") %>% 
  filter(DemographicValue == "All Students")

pct_dropout <- project_data %>% 
  group_by(JJOffenderType) %>% 
  filter(HSOutcome == "Dropout") %>% 
  summarise(avg_pct_dropout = mean(Pct, na.rm = TRUE))
  
ggplot(data = avg_pct_offenders) +
  geom_col(
    mapping = aes(x = JJOffenderType, y = RedactedPct, fill = HSOutcome), position = "dodge"
  )

