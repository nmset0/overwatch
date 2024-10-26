# Packages:
library(readr)
library(knitr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(patchwork)

# Loading Datasets

# stock market data for Microsoft since 2020
msft = read.csv("Datasets/Microsoft_MSFT_StockPrice.csv")
#stock market data for Activision Blizzard from 2020 until acquisition
atvi = read_csv("Datasets/ActivisionBlizzard_ATVI_StockPrice.csv")

# ow_hero_data = read.csv("Datasets/OverwatchHeroData_clean.csv) 

# Data cleaning
msft$Vol. = gsub('M', "", msft$Vol.)
msft$Change.. = gsub('%', "", msft$Change..)
msft$Date = as.Date(msft$Date, format = "%m/%d/%Y")

msft = msft %>% mutate(year = year(msft$Date), .after = Date) %>% 
  mutate(month = month(msft$Date), .after = year) %>% 
  mutate(day = day(msft$Date), .after = month)

atvi$Vol. = gsub('M', "", atvi$Vol.)
atvi$`Change %` = gsub('%', "", atvi$`Change %`)
atvi$Date = as.Date(atvi$Date, format = "%m/%d/%Y")

atvi = atvi %>% mutate(year = year(atvi$Date), .after = Date) %>% 
  mutate(month = month(atvi$Date), .after = year) %>% 
  mutate(day = day(atvi$Date), .after = month)

msft = msft %>% mutate(id = 1:nrow(msft), .before = Date)
atvi = atvi %>% mutate(id = 1:nrow(atvi), .before = Date)

# ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----

#  Activision stock value the day 'Overwatch 2' is released
stock_value_OW2release = subset(atvi, Date == '2022-10-04') %>% select(-Date)
stock_value_OW2release %>% kable(caption = "Activision stock value the day 'Overwatch 2' is released")

#  Activision stock value the final day before Microsoft acquires 
stock_value_preacquired = subset(atvi, Date == '2023-10-12')  %>% select(-Date)
stock_value_OW2release %>% kable(caption = "Stock value of Activision on release day of 'Overwatch 2'")
stock_value_preacquired %>% kable(caption = "Stock value of Activision the day before acquisition by Microsoft")

# combined
activision_dates = rbind(stock_value_OW2release, stock_value_preacquired)
activision_dates = activision_dates %>% mutate(Date = c('2022-10-04', '2023-10-12'), .before = year)

# Stock of Microsoft the day they buy Activision
stock_value_buyatvi = subset(msft, Date == '2023-10-13')
stock_value_buyatvi %>% kable(caption = "Stock value the day of Microsoft acquisition of Activision")

# microsoft_dates = rbind(stock_value_buyatvi)

# Data on Mythic hero skins
mythic = read.csv("Datasets/ow_mythicskins.csv") %>% select(-X)
mythic$release_date = format(as.Date(mythic$release_date, format = "%m/%d/%Y"), "%Y-%m-%d")
mythic_noWidow = subset(mythic, hero != 'Widowmaker')
temp_frame_mythic_dates_noWidowmaker = data.frame(mythic_noWidow$release_date)
colnames(temp_frame_mythic_dates_noWidowmaker) = c("release_date")


month_colors = c("1" = "deepskyblue", "2" = "cadetblue1", "3" = "cyan", 
                  "4" = "aquamarine", "5" = "darkolivegreen1", "6" = "chartreuse", 
                  "7" = "gold", "8" = "darkgoldenrod2", "9" = "orange3", 
                  "10" = "brown", "11" = "magenta", "12" = "purple")

msft$month = factor(msft$month, levels = names(month_colors))
atvi$month = factor(atvi$month, levels = names(month_colors))

lw = 0.2

# Stock value graph of Microsoft
msft_price = ggplot(data = msft, aes(x = as.Date(Date), y = Price, group = 1, color = month)) + 
  geom_line(linewidth=0.8) + 
  scale_x_date(name = "Month/Year", date_labels = "%b %Y", date_breaks = "1 month") + 
  scale_color_manual(values = month_colors) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none") + 
  geom_vline(xintercept = as.Date('2023-10-13'), color = "red3", linewidth = lw) + 
  labs(title = "Stock Price of Microsoft")

msft_price = msft_price +
  geom_point(data = stock_value_buyatvi, aes(x = as.Date(Date), y = Price), color = "black")

msft_price


# Stock value graph of Activision
atvi_price = ggplot(data = atvi, aes(x = as.Date(Date), y = Price, group = 1, color = month)) +
  geom_line(linewidth=0.8) + scale_x_date(name = "Month/Year", date_labels = "%b %Y", date_breaks = "1 month") + 
  scale_color_manual(values = month_colors) + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none") + 
  labs(title = "Stock Price of Activision")

atvi_price_postRelease = subset(atvi, Date >= '2022-09-01')
atvi_price2 = ggplot(data = atvi_price_postRelease, aes(x=as.Date(Date), y = Price, group = 1, color = month)) +
  geom_line(linewidth=0.8) + scale_x_date(name = "Month/Year", date_labels = "%b %Y", date_breaks = "1 month") + 
  scale_color_manual(values = month_colors) + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none") + 
  labs(title = "Stock Price of Activision Between 1 Sep. 2022 & 1 Nov. 2023")

atvi_price2 = atvi_price2 +
  geom_point(data = activision_dates, aes(x = as.Date(Date), y = Price), color = "black")

atvi_price2 = atvi_price2 + 
  geom_vline(xintercept = as.Date(temp_frame_mythic_dates_noWidowmaker$release_date[1]), color = "red3", linewidth = lw, alpha = 0.5) + 
  geom_vline(xintercept = as.Date(temp_frame_mythic_dates_noWidowmaker$release_date[2]), color = "red3", linewidth = lw, alpha = 0.5) + 
  geom_vline(xintercept = as.Date(temp_frame_mythic_dates_noWidowmaker$release_date[3]), color = "red3", linewidth = lw, alpha = 0.5) +
  geom_vline(xintercept = as.Date(temp_frame_mythic_dates_noWidowmaker$release_date[4]), color = "red3", linewidth = lw, alpha = 0.5) + 
  geom_vline(xintercept = as.Date(temp_frame_mythic_dates_noWidowmaker$release_date[5]), color = "red3", linewidth = lw, alpha = 0.5) + 
  geom_vline(xintercept = as.Date(temp_frame_mythic_dates_noWidowmaker$release_date[6]), color = "red3", linewidth = lw, alpha = 0.5) +
  geom_vline(xintercept = as.Date(temp_frame_mythic_dates_noWidowmaker$release_date[7]), color = "red3", linewidth = lw, alpha = 0.5) + 
  geom_vline(xintercept = as.Date(temp_frame_mythic_dates_noWidowmaker$release_date[8]), color = "red3", linewidth = lw, alpha = 0.5) + 
  geom_vline(xintercept = as.Date(temp_frame_mythic_dates_noWidowmaker$release_date[9]), color = "red3", linewidth = lw, alpha = 0.5) +
  geom_vline(xintercept = as.Date(temp_frame_mythic_dates_noWidowmaker$release_date[10]), color = "red3", linewidth = lw, alpha = 0.5) + 
  geom_vline(xintercept = as.Date(temp_frame_mythic_dates_noWidowmaker$release_date[11]), color = "red3", linewidth = lw, alpha = 0.5) + 
  geom_vline(xintercept = as.Date(temp_frame_mythic_dates_noWidowmaker$release_date[12]), color = "red3", linewidth = lw, alpha = 0.5)

atvi_price2





  
  
  
