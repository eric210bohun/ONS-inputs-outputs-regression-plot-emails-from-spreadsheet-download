# a script that should anticipate new ONS agricultural price filenames, then download, convert to accessible format, extract
# specific prices, plot them, identify trends, and then email the plots. 

# load libraries
library(tidyverse)
library(rvest)
library(RSelenium)
library(V8)
library(htmlunit)
library(cronR)
library(emayili)
library(magrittr)
library(lubridate)
library(gridExtra)
library(ggthemes)
library(readODS)
library(readxl)
library(RColorBrewer)
# define url as object
url <- "https://www.gov.uk/government/statistics/agricultural-price-indices"

# read url as html object
h1 <- read_html(url)

# extract nodes from html object
p1 <- h1 %>% html_nodes("script") %>% html_text()

#scrape latest API .ods filename from p1[8]
pattern1 <- "[a-z]{5}://[a-z]{6}.[a-z]{10}.[a-z]{7}.[a-z]{3}.[a-z]{2}/[a-z]{10}/[a-z]{7}/[a-z]{6}/[a-z]{7}/[a-z]{10}_[a-z]{4}/[a-z]{4}/\\d\\d\\d\\d\\d\\d/[A-Z]{3}-[a-z]{14}-\\d\\d[a-z]{3}\\d\\d.[a-z]{3}"
API_latest_ods <- str_extract(p1[9], pattern1)

#download latest API monthly .ods dataset & change decimals to two
download.file(API_latest_ods, "~/projects/agri_ss/latest_API.ods", method = "wget")

# extraction of various prices paid by/to farmers (indexed to annual 2015 prices = 100)
all_data <- (read_ods("/home/eric/projects/agri_ss/latest_API.ods", sheet = "Outputs_monthly_2011_onwards", col_names = FALSE))
all_data <- all_data[4:ncol(all_data)]
# colnames(all_data) <- slice(all_data, 7)
date <- slice(all_data, 7)
date <- as.character(as.list(date))
date[length(date)] <- substring(date[length(date)], 1, 6)
animals_for_slaughter_or_export <- as.numeric(slice(all_data, 58))
animal_products <- as.numeric(slice(all_data, 73))
cooking_apples <- as.numeric(slice(all_data, 46))
onions_brown <- as.numeric(slice(all_data, 40))
wheat_for_bread <- as.numeric(slice(all_data, 16))

# extraction of monthly inputs - straight fertilizer [21] costs, animal feed stuffs [36], vetinary services [34], motor fuels [18], seeds [13](indexed to annual 2015 prices = 100)
all_data2 <- read_ods("/home/eric/projects/agri_ss/latest_API.ods", sheet = "Inputs_monthly_2011_onwards", col_names = FALSE)
all_data2 <- all_data2[4:ncol(all_data2)]
seeds <- as.numeric(slice(all_data2, 13))
motor_fuels <- as.numeric(slice(all_data2, 18))
vetinary_services <- as.numeric(slice(all_data2, 34))
animal_feed_stuffs <- as.numeric(slice(all_data2, 36))
straight_fertilizer <- as.numeric(slice(all_data2, 21))

# plot of all monthly output prices
selection1 <- bind_rows("date" = date, "animals_for_slaughter_or_export" = animals_for_slaughter_or_export, "animal_products" = animal_products, "cooking_apples" = cooking_apples, "onions_brown" = onions_brown, "wheat_for_bread" = wheat_for_bread)
selection1$date <- as.Date.POSIXct(parse_date_time(selection1$date, "%m-%y"))
selection1 <- selection1 %>% pivot_longer(-date, names_to = "product", values_to = "API")
selection1 <-  selection1 %>% ggplot(aes(x=date, y=c(animals_for_slaughter_or_export, animal_products, cooking_apples, onions_brown, wheat_for_bread))) +
          scale_x_date() + theme_economist() + theme(legend.key.width=unit(2,"cm"),legend.key.height=unit(0.5,"cm"),legend.position = c(0.29, 0.85))+ xlab("Jan 2011 to Present") + ylab("API - indexed for 2015 = 100") + geom_line(mapping = aes(linetype = product, color = product))
ggsave("~/projects/agri_ss/five_API_outputs_plot.png", plot = selection1)   

# plot of animal slaughter/export with vetinary and animal feed stuffs
selection2 <- bind_rows("date" = date, "animals_for_slaughter_or_export" = animals_for_slaughter_or_export, "vetinary_services" = vetinary_services, "animal_feed_stuffs" = animal_feed_stuffs)
selection2$date <- as.Date.POSIXct(parse_date_time(selection2$date, "%m-%y"))
selection2 <- selection2 %>% pivot_longer(-date, names_to = "product", values_to = "API")
selection2 <- selection2 %>% ggplot(aes(x=date, y=c(animals_for_slaughter_or_export, vetinary_services, animal_feed_stuffs))) +
  scale_x_date() + theme_economist() + theme(legend.key.width=unit(2,"cm"),legend.key.height=unit(0.5,"cm"),legend.position = c(0.35, 0.75))+ xlab("Jan 2011 to Present") + ylab("API - indexed for 2015 = 100") + geom_line(mapping = aes(linetype = product, color = product)) + geom_smooth(method = "lm", se = TRUE) + geom_hline(yintercept=100, col = "blue")
ggsave("~/projects/agri_ss/export_slaughter_and _two_independant_variables_plot.png", plot = selection2)

# plot of onions with motor fuel/seeds/fertilizer
selection3 <- bind_rows("date" = date, "brown_onions" = onions_brown, "motor_fuel" = motor_fuels, "seeds" = seeds, "fertilizer_straight"=straight_fertilizer)
selection3$date <- as.Date.POSIXct(parse_date_time(selection3$date, "%m-%y"))
selection3 <- selection3 %>% pivot_longer(-date, names_to = "product", values_to = "API")
selection3 <- selection3 %>% ggplot(aes(x=date, y=c(onions_brown, motor_fuels, seeds, straight_fertilizer))) +
  scale_x_date() + theme_economist() + theme(legend.key.width=unit(2,"cm"),legend.key.height=unit(0.5,"cm"),legend.position = c(0.50, 0.75))+ xlab("Jan 2011 to Present") + ylab("API - indexed for 2015 = 100") + geom_line(mapping = aes(linetype = product, color = product)) + geom_smooth(method = "lm", se = TRUE)
# regression comparison of whether it might make sense to switch from onions to cooking apples?
selection4 <- bind_rows("date" = date, "cooking_apples" = cooking_apples, "motor_fuel" = motor_fuels, "seeds" = seeds, "fertilizer_straight"=straight_fertilizer)
selection4$date <- as.Date.POSIXct(parse_date_time(selection4$date, "%m-%y"))
selection4 <- selection4 %>% pivot_longer(-date, names_to = "product", values_to = "API")
selection4 <- selection4 %>% ggplot(aes(x=date, y=c(onions_brown, motor_fuels, seeds, straight_fertilizer))) +
  scale_x_date() + theme_economist() + theme(legend.key.width=unit(2,"cm"),legend.key.height=unit(0.5,"cm"),legend.position = c(0.50, 0.75))+ xlab("Jan 2011 to Present") + ylab("API - indexed for 2015 = 100") + geom_line(mapping = aes(linetype = product, color = product)) + geom_smooth(method = "lm", se = TRUE)
selection5 <- grid.arrange(selection3, selection4, ncol = 2)
ggsave("~/projects/agri_ss/onions_or_apples_with_independant_variables_plot.png", plot = selection5)

# set up of email alert
email_eric <- envelope()
email_eric <- email_eric %>% from("oedipusatcolonussheffield@gmail.com") %>% to("eric210bohun@gmail.com")
email_eric <- email_eric %>% subject("Agrcultural Price Indicators - Monthly Scrape & Plots ")
email_eric <- email_eric %>% text("Check out what's happening to UK farming inputs & outputs")
email_eric <- email_eric %>% attachment("~/projects/agri_ss/export_slaughter_and _two_independant_variables_plot.png")
email_eric <- email_eric %>% attachment("~/projects/agri_ss/onions_or_apples_with_independant_variables_plot.png")
email_eric <- email_eric %>% attachment("~/projects/agri_ss/five_API_outputs_plot.png")
smtp <- server(host = "smtp.gmail.com",
               port = 465,
               username = "oedipusatcolonussheffield@gmail.com",
               password = "XXXXXXXXX")
smtp(email_eric, verbose = TRUE)
