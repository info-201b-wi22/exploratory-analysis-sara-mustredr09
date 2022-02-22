project_data<-read.csv("https://raw.githubusercontent.com/info-201b-wi22/exploratory-analysis-sara-mustredr09/main/Juvenile_Justice_Dashboard_-_HS_Completion.csv?token=GHSAT0AAAAAABQIEWSA6BKJCPJ3VN3C5VHSYQYGLCA")
View(project_data)
install.packages("tidyverse")
library(tidyverse)
library(dplyr)
library(ggplot2)

pct_graduated <- project_data %>% 
  group_by(DemographicValue) %>% 
  filter(HSOutcome == "HS Diploma") %>% 
  filter(DemographicValue == "Black or African American") %>%
  summarise(avg_pct_graduted = mean(Pct, na.rm = TRUE))


p1 <- ggplot(data = pct_graduated) + geom_line(aes(y = Pct, x = CohortYearTTL, 
                                               group = DemographicValue), 
                                               data = project_data)
p1 + labs(title = "HS outcome by race", x = "Year", y = "Percent With Diploma", 
          caption = "Data: data.wa.gov")

