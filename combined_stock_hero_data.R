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



# Load required libraries
library(RSQLite)


# Connect to the SQLite database
con <- dbConnect(RSQLite::SQLite(), "overwatch.sql")
dbExecute(con, "DROP TABLE IF EXISTS stocks;")
dbExecute(con, "DROP TABLE IF EXISTS mythics;")
# Create stocks table
dbExecute(con, "
CREATE TABLE IF NOT EXISTS stocks (
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
    Llose REAL,
    volume INTEGER,
    percent_change REAL
);")

# Create mythics table
dbExecute(con, "
CREATE TABLE IF NOT EXISTS mythics (
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

# Assuming 'stocks' and 'mythics' data frames are already loaded in RStudio
# Insert data into stocks table
dbWriteTable(con, "stocks", stocks, append = TRUE, row.names = FALSE)

# Insert data into mythics table
dbWriteTable(con, "mythics", mythics, append = TRUE, row.names = FALSE)

# Disconnect from the database
dbDisconnect(con)

