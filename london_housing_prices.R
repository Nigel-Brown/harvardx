repos <- 'https://cran.rstudio.com/'
if(!require(tidyverse)) install.packages("tidyverse", repos = repos)
if(!require(tidymodels)) install.packages("tidymodels", repos = repos)
if(!require(scales)) install.packages("scales", repos = repos)

library(tidyverse)
library(tidymodels)
library(scales)


lhd <- read_rds(here::here('data', 'lhd.rds'))
