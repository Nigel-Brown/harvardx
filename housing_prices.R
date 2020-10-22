repos <- 'https://cran.rstudio.com/'
if(!require(tidyverse)) install.packages("tidyverse", repos = repos)
if(!require(tidymodels)) install.packages("tidymodels", repos = repos)
if(!require(scales)) install.packages("scales", repos = repos)
if(!require(leaflet)) install.packages("leaflet", repos = repos)
if(!require(mapview)) install.packages("mapview", repos = repos)
if(!require(lubridate)) install.packages("lubridate", repos = repos)
if(!require(funModeling)) install.packages("funModeling", repos = repos)
if(!require(tidycensus)) install.packages("tidycensus", repos = repos)


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
df <- read_csv(here::here('data', 'kc_house_data.csv'))


# data wrangling
# remove features with a predominant number of zeros
status <- df_status(df, print_results = FALSE)
remove_vars <-  status %>% filter(status$p_zeros > 0.6) %>% pull(variable)
df <- df %>% select(-one_of(remove_vars))

df <- df %>% 
  mutate(year = year(date),
         month = month(date),
         age = year - yr_built) %>% 
  select(-date)

to_convert <- df %>% 
  select(-c('id', 'bathrooms', 'lat', 'long')) %>% 
  colnames()

df <- df %>% 
  mutate_at(to_convert, as.integer)

df <- df %>% 
  filter(bedrooms != 0) %>% 
  mutate(bedrooms = replace(bedrooms, bedrooms==33, 3))



dim(df)  

df %>% 
  ggplot(aes(bedrooms, price)) +
  geom_point()

df %>% 
  group_by(bedrooms) %>% 
  ggplot(aes(condition, price)) +
  geom_boxplot()

summary(df$bedrooms)

library(leaflet)
# create map plots for the RMD file
bed_pal <- colorFactor(c("#000080", "red"), 1:2)

m <- df %>% 
  filter(bedrooms <= 2) %>% 
  leaflet() %>%
  addProviderTiles('CartoDB.Positron') %>%
  addCircles(lng = ~long, 
             lat = ~lat,
             fillOpacity = .3,
             popup = ~bedrooms,
             color = ~bed_pal(bedrooms)) %>%
  addLegend(pal = bed_pal, values = ~bedrooms, title = "# Bedrooms")

webshot::install_phantomjs()
mapshot(m, file = here::here('images', '1_2_beds.png'))


bed_pal <- colorFactor(c("blue", "red"), 3:4)
m <- df %>% 
  filter(bedrooms > 2 & bedrooms < 5  ) %>% 
  leaflet() %>%
  addProviderTiles('CartoDB.Positron') %>%
  addCircles(lng = ~long, 
             lat = ~lat,
             fillOpacity = .3,
             popup = ~bedrooms,
             color = ~bed_pal(bedrooms)) %>%
  addLegend(pal = bed_pal, values = ~bedrooms, title = "# Bedrooms")

mapshot(m, file = here::here('images', '3_4_beds.png'))

bed_pal <- colorFactor(c( "blue","green","red"), 5:11)
m <- df %>% 
  filter(bedrooms > 5) %>% 
  leaflet() %>%
  addProviderTiles('CartoDB.Positron') %>%
  addCircles(lng = ~long, 
             lat = ~lat,
             fillOpacity = .3,
             popup = ~bedrooms,
             color = ~bed_pal(bedrooms)) %>%
  addLegend(pal = bed_pal, values = ~bedrooms, title = "# Bedrooms")
mapshot(m, file = here::here('images', '5plus_beds.png'))










.# top 10 most expensive districts based on the number of properties over 3 million GBP?
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

# 10 least expensive districts based on the number of properties under 100000 GBP?
least_expensive_districts <- lhd %>% 
  group_by(district) %>%
  filter(price <  100000) %>% 
  summarise(properties_less_than_100K_GBP = n(), .groups = 'drop') %>% 
  arrange(desc(properties_less_than_100K_GBP)) %>% 
  slice(1:10)
knitr::kable(least_expensive_districts)

under_100K <- lhd %>% 
  filter(price < 100000) %>% 
  select(property_type, latitude, longitude, price)

# save image for later use in PDF report
map_under_100K <- under_100K %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(clusterOptions = markerClusterOptions())

mapshot(map_under_100K, file = here::here('images', 'map_under_100K.png'))

# which year had the most sales
lhd %>% 
  group_by(transaction_year) %>%
  summarise(number_of_properties = n(), .groups = 'drop') %>% 
  ggplot(aes(transaction_year, number_of_properties)) +
  geom_col(fill = 'steelblue') + 
  scale_x_continuous(breaks= pretty_breaks()) +
  labs(
    title = "Since 2014 the volume of house sales has been decreasing",
    x = NULL,
    y = "Properties sold",
    caption = 'Contains HM Land Registry data © Crown copyright and database right 2020.'
  )

# are the sales evenly spread across Greater London ?
df %>% 
  group_by(zipcode) %>% 
  summarise(n = n(), .groups = 'drop') %>% 
  ggplot(aes(reorder(zipcode, -n), n)) +
  geom_col(color = "black", fill = 'steelblue') +
  theme(axis.text.x  = element_text(angle=-90, hjust=0)) +
  labs(
    title = "Number of observed sales by zipcode",
    x= "Zip code",
    y = "Properties sold"
  )


# Set the random number stream using `set.seed()` so that the results can be 
# reproduced later. 
set.seed(123, sample.kind = 'Rounding')

# Save the split information for an 80/20 split of the data, stratifying on price
lhd_split <- initial_split(lhd, prob = 0.80, strata = price)


lhd_train <- training(lhd_split)
lhd_test  <-  testing(lhd_split)
lhd_cv <-  vfold_cv(lhd_train)

names(lhd)

lhd_rec <- recipe(price ~ transaction_year + outward_code + district + new_build + property_type + num_of_sales, data = lhd_train) %>% 
  step_log(price, base = 10) %>% 
  step_other(district, threshold = 0.01) %>% 
  step_dummy(all_nominal()) %>% 
  prep()


lhd_rec

lm_mod <- linear_reg(penalty = tune(),
                     mixture = tune()) %>% 
  set_engine("glmnet")

wf <- workflow() %>% 
  add_recipe(lhd_rec) %>% 
  add_model(lm_mod)

res <-  wf %>% 
  tune_grid(resamples = lhd_cv,
            grid = 10,
            metrics = metric_set(rmse))
res
best_params <-   res %>%
  select_best(metric = "rmse")
best_params
# Refit using the entire training data
reg_res <- wf %>% 
  finalize_workflow(best_params) %>%
  fit(data = lhd_train)

class(lhd_test)

reg_res %>%
  predict(new_data = juice(lhd_rec, as.data.frame(hd_test))) %>%
  bind_cols(lhd_test, .) %>%
  mutate(price = log10(price)) %>% 
  select(price, .pred) %>% 
  rmse(price, .pred)


library(patchwork)
library(splines)

plot_smoother <- function(deg_free) {
  ggplot(lhd_train, aes(x = latitude, y = price)) + 
    geom_point(alpha = .2) + 
    scale_y_log10() +
    geom_smooth(
      method = lm,
      formula = y ~ ns(x, df = deg_free),
      col = "red",
      se = FALSE
    ) +
    ggtitle(paste(deg_free, "Spline Terms"))
}

( plot_smoother(10) + plot_smoother(50) ) / ( plot_smoother(100) + plot_smoother(500) )
