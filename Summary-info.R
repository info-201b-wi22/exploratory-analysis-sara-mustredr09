Juvenile_Justice_Dashboard_HS_Completion<-read.csv("https://raw.githubusercontent.com/info-201b-wi22/exploratory-analysis-sara-mustredr09/main/Juvenile_Justice_Dashboard_-_HS_Completion.csv?token=GHSAT0AAAAAABQIEWSA6BKJCPJ3VN3C5VHSYQYGLCA")
install.packages("tidyverse")
library(dplyr)
# 1. Which  demographic value has the highest percentage of HS diplomas?
# Which group has the 
# Functions to use: summarize ( mode?)
#Groupby by demographic value

pct_hs_dip<- Juvenile_Justice_Dashboard_HS_Completion %>% 
  group_by(DemographicValue) %>% 
  filter(HSOutcome == 'HS Diploma') %>% 
  filter(Pct == max(Pct))

#Probs going with this one

pct_hs_dip<- Juvenile_Justice_Dashboard_HS_Completion %>% 
  filter(HSOutcome == 'HS Diploma') %>%
  group_by(DemographicValue) %>% 
  summarise(pct_hs_dip = mean(Pct, na.rm = TRUE))

pct_hs_dip<- Juvenile_Justice_Dashboard_HS_Completion %>% 
  filter(HSOutcome == 'HS Diploma') %>%
  group_by(DemographicValue) %>% 
  count(Pct) %>% 
  top_n(1)
  
?summarise

find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

# 2. Which Demographic value has the highest percentage of people with a criminal record ?
# Functions to use: summarize ( mode?)
# Groupby by demographic value
offender_type_outcome_race<-Juvenile_Justice_Dashboard_HS_Completion %>% 
  filter(JJOffenderType == 'Justice Involved') %>% 
  group_by(DemographicValue) %>% 
  filter(HSOutcome==max(HSOutcome,na.rm=TRUE)) %>% 
  
pct_justice_inv<- Juvenile_Justice_Dashboard_HS_Completion %>% 
  filter(JJOffenderType == 'Justice Involved') %>% 
  group_by(DemographicValue) %>% 
  summarise(pct_inv = mean(Pct, na.rm = TRUE))

#---------------------------------------------------------------------------------------
# 3. Average percentage of not justice involved per demographic value

pct_nojustice <- Juvenile_Justice_Dashboard_HS_Completion %>% filter(JJOffenderType == 'Not Justice Involved') %>% 
  group_by(DemographicValue) %>% summarise(pct_not_involved = mean(Pct, na.rm = TRUE))

# 4. Average percentage of justice involved (justice involved, juvenile offender, status offender) per demographic value
#RedactedPct

pct_criminal_record<-full_join(pct_justice_inv,pct_offenders, by="DemographicValue")
pct_offenders<-full_join(pct_juv_off,pct_status_off, by="DemographicValue")

                                
pct_justice_inv<- Juvenile_Justice_Dashboard_HS_Completion %>% 
  filter(JJOffenderType == 'Justice Involved') %>% 
  group_by(DemographicValue) %>% 
  summarise(pct_inv = mean(Pct, na.rm = TRUE))

pct_juv_off<- Juvenile_Justice_Dashboard_HS_Completion %>% 
  filter(JJOffenderType =='Juvenile Offender') %>% 
  group_by(DemographicValue) %>% 
  summarise(pct_juv = mean(Pct, na.rm = TRUE))

pct_status_off<- Juvenile_Justice_Dashboard_HS_Completion %>% 
  filter(JJOffenderType =='Status Offender') %>% 
  group_by(DemographicValue) %>% 
  summarise(pct_stat = mean(Pct, na.rm = TRUE))

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
