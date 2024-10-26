# Packages:
library(dplyr)
library(knitr)
library(readr)

# Datasets
mythics = read.csv("Datasets/ow_mythicskins.csv") %>% select(-X)
atvi = read.csv("Datasets/atvi_clean") %>% select(-X) %>% arrange(Date)
msft = read.csv("Datasets/msft_clean") %>% select(-X) %>% arrange(Date)

atvi = atvi %>% mutate(company = 'activision', .before = Price) %>% mutate(ticker = 'atvi', .after = day)
msft = msft %>% mutate(company = 'microsoft', .before = Price) %>% mutate(ticker = 'msft', .after = day)

msft_subset = subset(msft, Date > '2023-10-12')

# joining the two stocks datasets onto each other
stocks = rbind(atvi, msft_subset) 
stocks = stocks %>% mutate(id = 1:nrow(stocks)) %>% rename(volume = Vol.) %>% rename(percent_change = Change..)



