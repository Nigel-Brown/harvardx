repos <- 'https://cran.rstudio.com/'
if(!require(tidyverse)) install.packages("tidyverse", repos = repos)
if(!require(tidymodels)) install.packages("tidymodels", repos = repos)
if(!require(scales)) install.packages("scales", repos = repos)
if(!require(leaflet)) install.packages("leaflet", repos = repos)
if(!require(mapview)) install.packages("mapview", repos = repos)

# clean up 
rm(repos)

library(tidyverse)
library(tidymodels)
library(scales)
library(lubridate)
library(leaflet)
library(mapview)

# set theme for charts
theme_set(theme_minimal())

# load the dataset lhd
lhd <- read_rds(here::here('data', 'lhd.rds'))

# data wrangling for correlation plot
cols <- sapply(lhd, is.logical)
lhd[,cols] <- lapply(lhd[,cols], as.integer)
cols <- sapply(lhd, is.character)
lhd[,cols] <- lapply(lhd[,cols], as.factor)

lhd <- lhd %>% 
  mutate(type_enum = case_when(property_type == 'T' ~ 1L,
                               property_type == 'D' ~ 2L,
                               property_type == 'S' ~ 3L,
                               property_type == 'F' ~ 4L,
                               property_type == 'O' ~ 5L),
         outward_code = as.factor(substr(postcode, 1, 3)),
         numeric_dt = as.numeric(trans_date))


sum_price <- lhd %>% 
  group_by(transaction_year, district, property_type) %>% 
  summarise(avg_price = mean(price),
            min_price = min(price),
            max_price = max(price))

lhd <- inner_join(lhd, sum_price)
glimpse(lhd)

lhd %>% 
  filter(transaction_year %in% c("2009", "2019") & property_type == "F") %>% 
  ggplot(aes(district, price)) +
  geom_boxplot(aes(color = district), show.legend = FALSE) +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5)) + 
  scale_y_log10(labels = comma) +
  facet_wrap(~transaction_year, nrow = 2) +
  labs(
    title = "Movement of flat/masonette prices per district 2009 - 2019",
    subtitle = "Camden & Kensigton and Chelsea have seen the largest price growth",
    x = NULL,
    y = "Price (log10)",
    caption = 'Contains HM Land Registry data © Crown copyright and database right 2020.'
  )

lhd %>% 
  filter(transaction_year %in% c("2009", "2019") & property_type == "T") %>% 
  ggplot(aes(district, price)) +
  geom_boxplot(aes(color = district), show.legend = FALSE) +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5)) + 
  scale_y_log10(labels = comma) +
  facet_wrap(~transaction_year, nrow = 2) +
  labs(
    title = "Movement of terraced house prices per district 2009 - 2019",
    subtitle = "The price increase has been similar across all districts",
    x = NULL,
    y = "Price (log10)",
    caption = 'Contains HM Land Registry data © Crown copyright and database right 2020.'
  )

lhd %>% 
  filter(transaction_year %in% c("2009", "2019") & property_type == "D") %>% 
  ggplot(aes(district, price)) +
  geom_boxplot(aes(color = district), show.legend = FALSE) +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5)) + 
  scale_y_log10(labels = comma) +
  facet_wrap(~transaction_year, nrow = 2) +
  labs(
    title = "Movement of detached house prices per district 2009 - 2019",
    subtitle = "The spread of prices in Hounslow and Westminster has narrowed,
    while in Kensington and Chelsea it has widened",
    x = NULL,
    y = "Price (log10)",
    caption = 'Contains HM Land Registry data © Crown copyright and database right 2020.'
  )


lhd$trans_dt <- as.numeric(as.POSIXct(cab_small_sample$pickup_datetime,
                                                    format = '%d%b%y'))
cab_small_sample
nlevels(lhd$outward_code)
nlevels(lhd$district)
nlevels(lhd$postcode)
nlevels(lhd$address)
# Examine the dimensions of the data
dim(lhd)

# EDA
summary(lhd)  

glimpse(lhd)


# is there a trend of which month most sales occur on ?
lhd %>% 
  group_by(transaction_month) %>% 
  summarise(n = n(), .group = 'drop') %>% 
  ggplot(aes(transaction_month, n)) + 
  geom_col(show.legend = FALSE, fill = 'steelblue') +
  scale_x_discrete(limits = c('Jan', "Feb", "Mar", "Apr", "May",
                              "Jun", "Jul", "Aug", "Sep", "Oct",
                              "Nov", "Dec")) +
  labs(
    title = "Summer months are the most popular for house purchases",
    x = NULL,
    y = "Number of sale transcations",
    caption = 'Contains HM Land Registry data © Crown copyright and database right 2020.'
  )



# how does the avg price increase year on year ?
lhd %>% 
  group_by(transaction_year) %>% 
  summarise(avg_price = mean(price)) %>% 
  ggplot(aes(transaction_year, avg_price)) + 
  geom_line(size = 1, color = 'steelblue') +
  scale_y_continuous(labels = comma) + 
  scale_x_continuous(breaks= pretty_breaks()) + 
  labs(
    title = "The increase average housing cost in Greater London",
    subtitle = "A distinct slowdown in the increase since 2017",
    x = "Year",
    y = "Average sales price",
    caption = 'Contains HM Land Registry data © Crown copyright and database right 2020.')


# top 10 most expensive districts properties are over 3 million ?
top10_most_expensive_districts <- lhd %>% 
  group_by(district) %>%
  filter(price > 3000000) %>% 
  summarise(properties_greater_than_3_million_GBP = n(), .groups = 'drop') %>% 
  arrange(desc(properties_greater_than_3_million_GBP)) %>% 
  slice(1:10)
knitr::kable(top10_most_expensive_districts)



three_mil_plus <- lhd %>% 
  filter(price > 3000000) %>% 
  select(property_type, latitude, longitude, price)

# save image for later use in PDF report
map_3_mill_plus <- three_mil_plus %>% 
leaflet() %>% 
  addTiles() %>% 
  addMarkers(clusterOptions = markerClusterOptions())

webshot::install_phantomjs()
mapshot(map_3_mill_plus, file = here::here('images', 'map_3_mill_plus.png'))

under_100K <- lhd %>% 
  filter(price < 100000) %>% 
  select(property_type, latitude, longitude, price)

# save image for later use in PDF report
map_under_100K <- under_100K %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(clusterOptions = markerClusterOptions())

mapshot(map_under_100K, file = here::here('images', 'map_under_100K.png'))



# Which type of property sold the most ?
sales_by_type <- lhd %>% 
  group_by(property_type) %>%
  mutate(property_type = 
           case_when(property_type == 'T' ~ "Terraced",
                     property_type == 'D' ~ "Detached",
                     property_type == 'S' ~ "Semi-Detached",
                     property_type == 'F' ~ "Flat/Maisonette",
                     property_type == 'O' ~ "Other")) %>% 
  summarise(number_of_properties = n(), .groups = 'drop') %>% 
  arrange(desc(number_of_properties)) 

knitr::kable(sales_by_type)

# which year had the most sales
lhd %>% 
  mutate(year = year(trans_date)) %>% 
  group_by(year) %>%
  summarise(number_of_properties = n(), .groups = 'drop') %>% 
  ggplot(aes(year, number_of_properties)) +
  geom_col(fill = 'steelblue') + 
  scale_x_continuous(breaks= pretty_breaks()) +
  labs(
    title = "Since 2014 the volume of house sales has been decreasing",
    x = NULL,
    y = "Properties sold",
    caption = 'Contains HM Land Registry data © Crown copyright and database right 2020.'
  )


# plot distribution of house prices
lhd %>% 
  ggplot(aes(price)) + 
  geom_histogram(bins = 20, fill = 'steelblue', color = 'black') + 
  scale_x_log10(label = comma) +
  scale_y_log10(label = comma) +
  labs(
    title = "House prices appear to be log-normally distributed",
    x = 'Price (log10)',
    y = 'count'
  )

glimpse(lhd)

lhd_feature <- lhd %>% 
  select(price, transaction_month, transaction_year, new_build, type_enum, numeric_dt, latitude, longitude, avg_price)

# plot correlation coeffs
library(corrplot)

corrplot(cor(lhd_feature), type = 'upper', order="hclust",
         col=c("black", "white"),
         bg="lightblue",tl.col = "black")



qplot(transaction_year, 
      price, 
      data = lhd, 
      geom = c("point", "smooth"), 
      alpha = I(1 / 5))

