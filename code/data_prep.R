library(tidyverse)
library(lubridate)

# https://www.gov.uk/guidance/about-the-price-paid-data
# If you use or publish our Price Paid Data, you must add the following attribution statement:
# Contains HM Land Registry data © Crown copyright and database right 2020. This data is licensed under the Open Government Licence v3.0.
# http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/

url = 'http://prod.publicdata.landregistry.gov.uk.s3-website-eu-west-1.amazonaws.com/pp-complete.txt'

data = read_csv(url, col_names = c("trans_id", "price", "trans_date", "postcode",
                                   "property_type", "new_build", "estate_type", "paon", "saon",
                                   "street", "locality", "town", "district", "county", "cat_type", "record_status"))

greater_london_districts <- c("BARKING AND DAGENHAM","BARNET", "BEXLEY", "BRENT", "BROMLEY",
                              "CAMDEN", "CITY OF LONDON", "CROYDON", "EALING", "ENFIELD", 
                              "GREENWICH","HACKNEY", "HAMMERSMITH AND FULHAM", "HARINGEY", "HARROW", 
                              "HAVERING", "HILLINGDON","HOUNSLOW", "ISLINGTON", "KENSINGTON AND CHELSEA", 
                              "KINGSTON UPON THAMES","LAMBETH","LEWISHAM", "MERTON", "NEWHAM", 
                              "REDBRIDGE", "RICHMOND UPON THAMES", "SOUTHWARK", "SUTTON","TOWER HAMLETS", 
                              "WALTHAM FOREST", "WANDSWORTH", "CITY OF WESTMINSTER")

data$saon <- ifelse(is.na(data$saon), "", data$saon)

lhd <- data %>% filter(district %in% greater_london_districts 
                       & cat_type == "A"
                       & estate_type == "F") %>% 
  mutate(trans_date = ymd(trans_date),
         address = paste0(paon , saon ,' ', street)) %>% 
  select(-c('cat_type', 'town', 'county', 'paon', 'saon', 'street', 'locality', 'record_status', 'estate_type', 'trans_id'))

# Remove known errors in the data
lhd <- lhd %>% filter(!postcode %in% c('SY3 6DQ', 'B74 2QT', 'NP19 0BG', 'CW8 1NB')) 

# make price an integer to save disk space
lhd$price <- as.integer(lhd$price)

# create year and month columns from the transaction date
lhd <- lhd %>% 
  mutate(transaction_year = as.integer(year(trans_date)),
         transaction_month = as.integer(month(trans_date)))

# create a 10 year dataset drop data from 2020 as it is an incomplete year
lhd <- lhd %>% 
  filter(transaction_year >= 2009 & transaction_year != 2020)

# https://www.ons.gov.uk/methodology/geography/licences
# You may re-use this information (not including logos or Northern Ireland data) free of charge in any format or medium, under the terms of the relevant # # data owners' licence. 
# In addition, the following attribution statements must be acknowledged or displayed whenever the owners data is used:

# Contains Ordnance Survey data © Crown copyright and database right 2020

# Contains Royal Mail data © Royal Mail copyright and database right 2020

# Source: Office for National Statistics licensed under the Open Government Licence v.3.0

url <- "https://www.freemaptools.com/download/full-postcodes/ukpostcodes.zip"
tmp <- tempfile()
download.file(url, tmp)
pc <- read_csv(unz(tmp, 'ukpostcodes.csv'))
pc <- pc %>% select(-id)
lhd <- lhd %>% 
  left_join(pc, by = 'postcode')
lhd$new_build <- ifelse(lhd$new_build == "Y", TRUE, FALSE)
lhd$district <- ifelse(lhd$district == "CITY OF WESTMINSTER", 'WESTMINSTER', lhd$district)
# Remove properties without spatial coords
lhd <- lhd %>% filter(!is.na(latitude))

# write the dataset to disk
write_rds(lhd, here::here('data', 'lhd.rds'), compress = "xz")

# clean up memory
rm('data', 'greater_london_districts', 'lhd', 'pc', 'tmp','url')

