repos <- 'https://cran.rstudio.com/'

if(!require(tidyverse)) install.packages("tidyverse", repos = repos)
if(!require(tidymodels)) install.packages("tidymodels", repos = repos)
if(!require(scales)) install.packages("scales", repos = repos)
if(!require(leaflet)) install.packages("leaflet", repos = repos)
if(!require(lubridate)) install.packages("lubridate", repos = repos)
if(!require(tictoc)) install.packages("tictoc", repos = repos)

# clean up 
rm(repos)

library(tidyverse)
library(tidymodels)
library(scales)
library(lubridate)
library(leaflet)
library(tictoc)

# set theme for charts
theme_set(theme_minimal())


# load the dataset lhd
df <- read_csv(here::here('data', 'kc_house_data.csv'))

## data wrangling

# remove features with a predominant number of zeros
status <- df_status(df, print_results = FALSE)
remove_vars <-  status %>% filter(status$p_zeros > 0.6) %>% pull(variable)
df <- df %>% select(-one_of(remove_vars))

# create year and month columns
df <- df %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  select(-date)

# convert data format to integers
to_convert <- df %>% 
  select(-c('id', 'bathrooms', 'lat', 'long')) %>% 
  colnames()

df <- df %>% 
  mutate_at(to_convert, as.integer)

# remove anomolous data
df <- df %>% 
  filter(bedrooms != 0) %>% 
  mutate(bedrooms = replace(bedrooms, bedrooms==33, 3))

# clean update memory
rm(remove_vars, status, to_convert)

library(leaflet)

## create map plots for the RMD file

# create a palette for the map
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

## Create a palette for the map
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

# clean up memory
rm(m, bed_pal)

# Convert price to a log10 value
df <- df %>% mutate(price = log10(price))


# Set the random number stream using `set.seed()` so that the results can be 
# reproduced later. 
set.seed(54321)

# Save the split information for an 80/20 split of the data, stratifying on price
df_split <- initial_split(df, prob = 0.8, strata = price)

df_train <- training(df_split)
df_test  <-  testing(df_split)

write_rds(df_train, here::here('data', 'train.rds'))
write_rds(df_train, here::here('data', 'test.rds'))

# create CV object from training data
df_cv <- vfold_cv(df_train)

# create lm recipe
df_rec <- 
  recipe(price ~ ., data = df_train) %>%
  update_role(id, new_role = "ID") %>%
  step_other(c(bedrooms,bathrooms, zipcode), threshold = 0.01) %>% 
  step_log(starts_with("sqft_"), base = 10) %>% 
  prep()

df_rec

# create model
lm_mod <- linear_reg() %>% 
  set_engine("lm")

# create workflow
lm_wflow <- workflow() %>% 
  add_model(lm_mod) %>% 
  add_recipe(df_rec)

# Tic Toc used to time the model fitting
tic()

lm_fit <- 
  lm_wflow %>% 
  fit(data = df_train)

lm_fit %>% 
  pull_workflow_fit() %>% 
  tidy()

toc()


lm_test_res <- 
  predict(lm_fit, new_data = df_test %>% select(-price)) 

lm_test_res <- bind_cols(lm_test_res, df_test %>% select(price))


lm_test_res

lm_test_res %>% 
  ggplot(aes(price, .pred)) + 
  # Create a diagonal line:
  geom_abline(lty = 2) + 
  geom_point(alpha = 0.5) + 
  labs(y = "Predicted Sale Price (log10)", x = "Sale Price (log10)") +
  # Scale and size the x- and y-axis uniformly:
  coord_obs_pred()


res_metrics <- metric_set(rmse, rsq, mae)
res_metrics(lm_test_res, truth = price, estimate = .pred)

## Random Forest model

rf_spec <- 
  rand_forest(
    mtry = tune(),
    trees = 1000,
    min_n =tune()) %>% 
  set_engine("ranger") %>% 
  set_mode("regression")


rf_wflow <- 
  workflow() %>% 
  add_model(rf_spec) %>% 
  add_recipe(df_rec)

# The tuning takes a while go get a coffee !
tic()
doParallel::registerDoParallel()

set.seed(123)
tune_res <- tune_grid(
  rf_wflow,
  resamples = df_cv,
  grid = 20
)

write_rds(tune_res, here::here('data', 'tune_res.rds'))
toc()

tune_res %>%
  collect_metrics() %>%
  filter(.metric == "rsq") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "RSQ")

rf_grid <- grid_regular(
  mtry(range = c(4, 20)),
  min_n(range = c(2, 10)),
  levels = 5
)

rf_grid

# Clean up

rm(tune_res)

set.seed(456)

tic()
doParallel::registerDoParallel()

regular_res <- tune_grid(
  rf_wflow,
  resamples = df_cv,
  grid = rf_grid
)
toc()

best_rmse <- select_best(regular_res, "rmse")
write_rds(best_rmse, here::here('data', 'best.rds'))

final_rf <- finalize_model(
  rf_spec,
  best_rmse
)

library(vip)
tic()
final_rf_vip <- final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(price ~ .,
      data = juice(df_rec) %>% select(-id)
  ) 
vip_gg <- vip(final_rf_vip) +
  geom_col(fill = 'skyblue', color = 'black') +
  labs(
    subtitle = 'North vs South is of major importance',
    x = "Variable",
    y = "Importance"
  )
ggsave(vip_gg, file = here::here('images', 'vip.png'))

toc()

# Final workflow for random forest
final_wf <- workflow() %>%
  add_recipe(df_rec) %>%
  add_model(final_rf)


# Model is fit against the original split, 
# takes roughly a minute to run
tic()
final_res <- final_wf %>%
  last_fit(df_split)
toc()

final_res <-  final_res %>%  collect_metrics()

write_rds(final_res, here::here('data', 'final.rds'))

# XGBoost Specification
xgb_spec <- boost_tree(
  trees = 1000, 
  tree_depth = tune(), 
  min_n = tune(), 
  loss_reduction = tune(),                     
  sample_size = tune(),
  mtry = tune(),        
  learn_rate = tune(),                        
) %>% 
  set_engine("xgboost") %>% 
  set_mode("regression")


# XGBoost tuning grid
xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), df_train),
  learn_rate(),
  size = 30
)

# XGBoost workflow
xgb_wf <- workflow() %>%
  add_formula(price ~ bedrooms + bathrooms + sqft_living + sqft_lot +
                floors + condition + grade + sqft_above + yr_built + zipcode + lat +
                long  + sqft_living15 + sqft_lot15 + year + month) %>%
  add_model(xgb_spec)

xgb_wf




