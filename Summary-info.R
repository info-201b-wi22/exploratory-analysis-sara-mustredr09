project_data<-read.csv("https://raw.githubusercontent.com/info-201b-wi22/exploratory-analysis-sara-mustredr09/main/Juvenile_Justice_Dashboard_-_HS_Completion.csv?token=GHSAT0AAAAAABQIEWSA6BKJCPJ3VN3C5VHSYQYGLCA")
install.packages("tidyverse")
library(dplyr)
# 1. Which word (hs outcome) pops up the most for each race?
 # Functions to use: summarize ( mode?)
#Groupby by demographic value
# 2. Which word (offender type) pops up the most for each race?
 # Functions to use: summarize ( mode?)
# Groupby by demographic value
# 3. Average percentage of not justice involved per demographic value

pct_nojustice <- Juvenile_Justice_Dashboard_HS_Completion %>% filter(JJOffenderType == 'Not Justice Involved') %>% 
  group_by(DemographicValue) %>% summarise(pct_not_involved = mean(Pct, na.rm = TRUE))

# 4. Average percentage of justice involved (justice involved, juvenile offender, status offender) per demographic value
#RedactedPct
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
