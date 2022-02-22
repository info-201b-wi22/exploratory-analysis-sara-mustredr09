Juvenile_Justice_Dashboard_HS_Completion<-read.csv("https://raw.githubusercontent.com/info-201b-wi22/exploratory-analysis-sara-mustredr09/main/Juvenile_Justice_Dashboard_-_HS_Completion.csv?token=GHSAT0AAAAAABQIEWSA6BKJCPJ3VN3C5VHSYQYGLCA")
install.packages("tidyverse")
library(dplyr)

# We were originally going to count the amount of times that each value appeared, but the research
# sample included the same amount of people per Demographic Group, so we decided to use percentages.

# 1.What is the percentage of HS Diplomas per group?
# Functions to use: summarize ( mode?)
#Groupby by demographic value

pct_hs_dip<-Juvenile_Justice_Dashboard_HS_Completion %>% 
  group_by(DemographicValue) %>% 
  filter(HSOutcome == 'HS Diploma') %>%
  summarize(pct_inv = mean(Pct, na.rm = TRUE))

# 2a. What is the percentage of dropouts per Demographic Value?
pct_dropouts<-Juvenile_Justice_Dashboard_HS_Completion %>% 
  group_by(DemographicValue) %>% 
  filter(HSOutcome == 'Dropout') %>%
  summarize(pct_inv = mean(Pct, na.rm = TRUE))

# 2b. What is the percentage of GED per Demographic Value?
pct_ged<-Juvenile_Justice_Dashboard_HS_Completion %>% 
  group_by(DemographicValue) %>% 
  filter(HSOutcome == 'GED') %>%
  summarize(pct_inv = mean(Pct, na.rm = TRUE))

#---------------------------------------------------------------------------------------
# 3. Average percentage of not justice involved per demographic value

avg_pct_nojustice <- Juvenile_Justice_Dashboard_HS_Completion %>% 
  filter(JJOffenderType == 'Not Justice Involved') %>% 
  group_by(DemographicValue) %>% 
  summarise(pct_not_involved = mean(Pct, na.rm = TRUE))

# 4. Average percentage of justice involved (justice involved, juvenile offender, status offender) per demographic value
#RedactedPct
                                
avg_pct_justice_inv<- Juvenile_Justice_Dashboard_HS_Completion %>% 
  filter(JJOffenderType == 'Justice Involved') %>% 
  group_by(DemographicValue) %>%
  summarise(pct_inv = mean(Pct, na.rm = TRUE))

avg_pct_juv_off<- Juvenile_Justice_Dashboard_HS_Completion %>% 
  filter(JJOffenderType =='Juvenile Offender') %>% 
  group_by(DemographicValue) %>% 
  summarise(pct_juv = mean(Pct, na.rm = TRUE))

avg_pct_status_off<- Juvenile_Justice_Dashboard_HS_Completion %>% 
  filter(JJOffenderType =='Status Offender') %>% 
  group_by(DemographicValue) %>% 
  summarise(pct_stat = mean(Pct, na.rm = TRUE))

avg_pct_offenders<-full_join(avg_pct_juv_off,avg_pct_status_off, by="DemographicValue")

avg_pct_criminal_record<-full_join(avg_pct_justice_inv,avg_pct_offenders, by="DemographicValue") %>% 
  mutate(pct_crim_rec=mean(pct_inv:pct_stat,na.rm=TRUE))

# 5. Proportion of dropouts w/justice involvement
all_justice_in <- Juvenile_Justice_Dashboard_HS_Completion %>% filter(JJOffenderType != 'Not Justice Involved') %>% 
  count()

dropouts_w_justice <- Juvenile_Justice_Dashboard_HS_Completion %>% filter(JJOffenderType != 'Not Justice Involved') %>% 
  filter(HSOutcome == 'Dropout') %>% count()

prop_dropouts_justice_in <- dropouts_w_justice / all_justice_in
  
  
# 7. Proportion of not justice involved w/HS Diploma
all_justice_not_in <- Juvenile_Justice_Dashboard_HS_Completion %>% filter(JJOffenderType == 'Not Justice Involved') %>% 
  count()

HS_Diploma_nojustice <- Juvenile_Justice_Dashboard_HS_Completion %>% filter(JJOffenderType == 'Not Justice Involved') %>% 
  filter(HSOutcome == 'HS Diploma') %>% count()

prop_HSdiploma_nojustice <- HS_Diploma_nojustice / all_justice_not_in
