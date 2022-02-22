project_data<-read.csv("https://raw.githubusercontent.com/info-201b-wi22/exploratory-analysis-sara-mustredr09/main/Juvenile_Justice_Dashboard_-_HS_Completion.csv?token=GHSAT0AAAAAABQIEWSA6BKJCPJ3VN3C5VHSYQYGLCA")
library(tidyverse)
library(dplyr)
library(ggplot2)

# Scatterplot representing Dropout rate per Justice Involvement category. 
# We chose this chart to reveal how involvement with the criminal justice system might affect
# dropout rates.

pct_dropouts <- project_data %>% 
  group_by(JJOffenderType) %>% 
  filter(HSOutcome == "Dropout") %>% 
  filter(CohortYearTTL > 2016) %>% 
  summarise(avg_pct_dropouts = mean(Pct, na.rm = TRUE))
  
ggplot(data = pct_dropouts) +
  geom_point(
    mapping = aes(x = JJOffenderType, y = avg_pct_dropouts, color = JJOffenderType)
  ) 
