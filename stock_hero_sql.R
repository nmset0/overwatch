# Packages:
library(dplyr)
library(knitr)
library(readr)
library(DBI)
library(RSQLite)



# Datasets
mythics = read.csv("Datasets/ow_mythicskins.csv") %>% select(-X) %>% arrange(release_date) %>% mutate(id = 1:nrow(mythics))
atvi = read.csv("Datasets/atvi_clean") %>% select(-X) %>% arrange(Date)
msft = read.csv("Datasets/msft_clean") %>% select(-X) %>% arrange(Date)

atvi = atvi %>% mutate(company = 'activision', .before = Price) %>% mutate(ticker = 'atvi', .after = day)
msft = msft %>% mutate(company = 'microsoft', .before = Price) %>% mutate(ticker = 'msft', .after = day)

msft_subset = subset(msft, Date > '2023-10-12')

# joining the two stocks' datasets onto each other
stocks = rbind(atvi, msft_subset) 
stocks = stocks %>% mutate(id = 1:nrow(stocks)) %>% rename(volume = Vol.) %>% rename(percent_change = Change..)




# SQLite code, I felt this was more efficient than R code
con = dbConnect(RSQLite::SQLite(), "overwatch.db")

dbExecute(con, "DROP TABLE IF EXISTS stocks;")
dbExecute(con, "DROP TABLE IF EXISTS mythics;")

# Create stocks table
dbExecute(con, "
CREATE TABLE stocks (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    Date DATE,
    year INTEGER,
    month INTEGER,
    day INTEGER,
    ticker TEXT,
    company TEXT,
    Price REAL,
    Open REAL,
    High REAL,
    Low REAL,
    Close REAL,   -- Fixed typo from 'Llose' to 'Close'
    volume INTEGER,
    percent_change REAL
);")

dbWriteTable(con, "stocks", stocks, append = TRUE, row.names = FALSE)


# Create mythics table
dbExecute(con, "
CREATE TABLE mythics (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    release_date DATE,
    year INTEGER,
    month INTEGER,
    day INTEGER,
    hero TEXT,
    mythic_skin TEXT,
    season INTEGER,
    price_usd INTEGER,
    price_prisms INTEGER
);")

dbWriteTable(con, "mythics", mythics, append = TRUE, row.names = FALSE)

# Execute the JOIN query
stocks_sql = dbGetQuery(con, "
  SELECT s.*, m.hero, m.mythic_skin, m.season, m.price_usd, m.price_prisms
  FROM stocks s
  JOIN mythics m ON s.Date = m.release_date;
")
dbDisconnect(con)

stocks_skins_sql = stocks_sql %>% select(-Close)
View(stocks_sql)
