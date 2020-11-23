# Load library packages ---------------------------------------------------

repos <- 'https://cran.rstudio.com/'

if(!require(tidyverse)) install.packages("tidyverse", repos = repos)
if(!require(tidymodels)) install.packages("tidymodels", repos = repos)
if(!require(scales)) install.packages("scales", repos = repos)
if(!require(leaflet)) install.packages("leaflet", repos = repos)
if(!require(lubridate)) install.packages("lubridate", repos = repos)
if(!require(tictoc)) install.packages("tictoc", repos = repos)
if(!require(funModeling)) install.packages("funModeling", repos = repos)
if(!require(webshot)) install.packages("webshot", repos = repos)
if(!require(mapview)) install.packages("mapview", repos = repos)
if(!require(here)) install.packages("here", repos = repos)
if(!require(vip)) install.packages("vip", repos = repos)

## clean up 
rm(repos)

library(tidyverse)
library(tidymodels)
library(scales)
library(lubridate)
library(leaflet)
library(tictoc)
library(funModeling)
library(vip)
library(webshot)
library(mapview)
library(here)

# Set theme for plots ---------------------------------------------------
theme_set(theme_minimal())

# Used in saving of images 
install_phantomjs(force = TRUE)

# Load the dataset lhd ---------------------------------------------------
df <- read_csv(here('data', 'kc_house_data.csv'))

# Data wrangling ---------------------------------------------------

# Remove features with a predominant number of zeros ---------------------------------------------------
status <- df_status(df, print_results = FALSE)
remove_vars <-  status %>% filter(status$p_zeros > 0.6) %>% pull(variable)
df <- df %>% select(-one_of(remove_vars))

# Create year and month columns ---------------------------------------------------
df <- df %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  select(-date)

# Convert data format to integers ---------------------------------------------------
to_convert <- df %>% 
  select(-c('id', 'bathrooms', 'lat', 'long')) %>% 
  colnames()

df <- df %>% 
  mutate_at(to_convert, as.integer)

# Remove anomolous data ---------------------------------------------------
df <- df %>% 
  filter(bedrooms != 0) %>% 
  mutate(bedrooms = replace(bedrooms, bedrooms==33, 3))

## Clean up 
rm(remove_vars, status, to_convert)

# Create map plots for the RMD file ---------------------------------------------------

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

mapshot(m, file = here('images', '1_2_beds.png'))

bed_pal <- colorFactor(c("#000080", "red"), 3:4)
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

# Convert price to a log10 value ---------------------------------------------------
df <- df %>% mutate(price = log10(price))


# Set the random number seed using `set.seed()` so that the results can be 
# reproduced later. 
set.seed(54321)

# Save the split for an 80/20 split of the data, stratifying on price ---------------------------------------------------
df_split <- initial_split(df, prob = 0.8, strata = price)

df_train <- training(df_split)
df_test  <-  testing(df_split)

write_rds(df_train, here::here('data', 'train.rds'))
write_rds(df_train, here::here('data', 'test.rds'))

# Create cross validation object from training data ---------------------------------------------------
df_cv <- vfold_cv(df_train)

# Create recipe ---------------------------------------------------
df_rec <- 
  recipe(price ~ ., data = df_train) %>%
  update_role(id, new_role = "ID") %>%
  step_other(c(bedrooms,bathrooms, zipcode), threshold = 0.01) %>% 
  step_log(starts_with("sqft_"), base = 10) %>% 
  prep()

# Create lm model ---------------------------------------------------
lm_mod <- linear_reg() %>% 
  set_engine("lm")

# Create lm workflow ---------------------------------------------------
lm_wflow <- workflow() %>% 
  add_model(lm_mod) %>% 
  add_recipe(df_rec)


# Fit lm model ---------------------------------------------------
# Tic Toc used to time the model fitting
tic()
lm_fit <- 
  lm_wflow %>% 
  fit(data = df_train)

lm_fit %>% 
  pull_workflow_fit() %>% 
  tidy()
toc()

# Predict price using the fitted lm model -------------------------------------------
lm_test_res <- 
  predict(lm_fit, new_data = df_test %>% select(-price)) 

lm_test_res <- bind_cols(lm_test_res, df_test %>% select(price))

# Capture results RMSE & R^2 ---------------------------------------------------
res_metrics <- metric_set(rmse, rsq)
lm_res <- res_metrics(lm_test_res, truth = price, estimate = .pred)
rmse <- lm_res %>% filter(lm_res$.metric == 'rmse') %>% select(.estimate)
rsq <- lm_res %>% filter(lm_res$.metric == 'rsq') %>% select(.estimate)

result <- tibble(Method = "Linear Regression Model", RMSE = rmse$.estimate, RSQ = rsq$.estimate)

# clean up
rm(df, lm_fit, lm_mod, lm_wflow, res_metrics, lm_res, lm_test_res, rmse, rsq)

# Random Forest model ---------------------------------------------------

# Create rf spec ---------------------------------------------------
rf_spec <- rand_forest(
    mtry = tune(),
    trees = 1000,
    min_n =tune()) %>% 
  set_engine("ranger") %>% 
  set_mode("regression")

# Create rf workflow ---------------------------------------------------
rf_wflow <- 
  workflow() %>% 
  add_model(rf_spec) %>% 
  add_recipe(df_rec)

# Tune hyperparameters ---------------------------------------------------
# The tuning takes a while go get a coffee !
tic()
doParallel::registerDoParallel()
set.seed(12345)
tune_res <- tune_grid(
  rf_wflow,
  resamples = df_cv,
  grid = 20
)

write_rds(tune_res, here::here('data', 'tune_res.rds'))
toc()


# Hyperparameter tuning results ---------------------------------------------------
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

# Hyperparameter 2nd tuning using a regular grid ---------------------------------------------------
rf_grid <- grid_regular(
  mtry(range = c(4, 20)),
  min_n(range = c(2, 10)),
  levels = 5
)

set.seed(456)

# Tuning takes a while, be patient
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

# Final model using best hyperparameters -------------------------------------
final_rf <- finalize_model(
  rf_spec,
  best_rmse
)


# Random Forest variable importance ----------------------------------------
# Takes time be patient
tic()
final_rf_vip <- final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(price ~ .,
      data = juice(df_rec) %>% select(-id)
  )

# plot importance and save for reporting 
vip_gg <- vip(final_rf_vip) +
  geom_col(fill = 'skyblue', color = 'black') +
  labs(
    subtitle = 'North vs South is of major importance',
    x = "Variable",
    y = "Importance"
  )
ggsave(vip_gg, file = here::here('images', 'vip.png'))

# clean up
rm(vip_gg)
toc()

# Final workflow for random forest ------------------------------------
final_wf <- workflow() %>%
  add_recipe(df_rec) %>%
  add_model(final_rf)


# Model is fit against the original split, 
# takes roughly a minute to run
tic()
rf_res <- final_wf %>%
  last_fit(df_split)
toc()

rf_res <- collect_metrics(rf_res)

rmse <- rf_res %>% filter(rf_res$.metric == 'rmse') %>% select(.estimate)
rsq <- rf_res %>% filter(rf_res$.metric == 'rsq') %>% select(.estimate)

result <- bind_rows(result, tibble(Method = "Random Forest Model",  RMSE = rmse$.estimate, RSQ = rsq$.estimate))

write_rds(rf_res, here::here('data', 'rf_res.rds'))

# Clean up
rm(regular_res, final_rf_vip, final_rf, rf_grid, rf_res, rf_spec, rf_wflow, rmse, rsq, final_wf, tune_res, best_rmse)

# XGBoost Specification ----------------------------------
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


# XGBoost tuning grid -----------------------------------
xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), df_train),
  learn_rate(),
  size = 30
)

# XGBoost workflow -------------------------------------
xgb_wf <- workflow() %>%
  add_formula(price ~ bedrooms + bathrooms + sqft_living + sqft_lot +
                floors + condition + grade + sqft_above + yr_built + zipcode + lat +
                long  + sqft_living15 + sqft_lot15 + year + month) %>%
  add_model(xgb_spec)


# tune the hyperparameters and save the predictions
# this take a while, be patient
tic()
doParallel::registerDoParallel()

set.seed(12345)
xgb_res <- tune_grid(
  xgb_wf,
  resamples = df_cv,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE)
)

xgb_metrics <- collect_metrics(xgb_res)
write_rds(xgb_metrics, here::here('data', 'xgb_metrics.rds'))

best_tab <-  show_best(xgb_res, "rsq")
write_rds(best_tab, here::here('data', 'xgb_best.rds'))
best_rsq <-  select_best(xgb_res, "rsq" )
write_rds(best_rsq, here::here('data', 'xgb_best_rsq.rds'))
toc()


# Final xgb workflow ------------------------------------------
final_xgb_wf <- finalize_workflow(
  xgb_wf,
  best_rsq
)


# Variable importance of the xgboost model -----------------------------------
tic()
doParallel::registerDoParallel()

final_xgb_vip <- final_xgb_wf %>%
  fit(data = df_train) %>%
  pull_workflow_fit() 
toc()


xgb_vip_gg <- final_xgb_vip %>% vip() +
  geom_col(fill = 'steelblue', color = 'black') +
  labs(
    subtitle = 'Living space is most important',
    x = "Variable",
    y = "Importance"
  )

ggsave(xgb_vip_gg, file = here::here('images', 'xgb_vip.png'))


rm(xgb_vip_gg )

# run the model using the initial split, the results will be evaluated against the test set.
tic()
final_res <- last_fit(final_xgb_wf, df_split)
toc()
xgb_res <- collect_metrics(final_res)
rmse <- xgb_res %>% filter(xgb_res$.metric == 'rmse') %>% select(.estimate)
rsq <- xgb_res %>% filter(xgb_res$.metric == 'rsq') %>% select(.estimate)

result <- bind_rows(result, tibble(Method = "XGBoost Model",  RMSE = rmse$.estimate, RSQ = rsq$.estimate))

write_rds(xgb_res, here::here('data', 'xgb_res.rds'))
write_rds(result, here::here('data', 'result.rds'))
result
 








