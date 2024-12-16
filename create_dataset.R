
library(tidyverse)

state_median_income <- read_csv(file = "./data/state_median_income.csv")
state_labour <- read_csv(file = "./data/state_total_labour_participation.csv")
state_unemployment <- read_csv(file = "./data/state_total_unemployment.csv")
state_median_house_price_index <- read_csv(file = "./data/state_median_house_price_index.csv")
us_median_income <- read_csv(file = "./data/total_median_income.csv")
us_labour <- read_csv(file = "./data/us_total_labour_participation.csv")
us_labour_by_characteristics <- read_csv(file="./data/us_labour_by_characteristics.csv")
us_unemployment <- read_csv(file = "./data/us_total_unemployment.csv")
us_unemployment_by_characteristics <- read_csv(file = "./data/us_unemployment_by_characteristics.csv")
us_median_house_price <- read_csv(file = "./data/total_median_hose_price.csv")


data_list <- list(
  state_median_income,
  state_labour,
  state_unemployment,
  state_median_house_price_index,
  us_median_income,
  us_labour,
  us_labour_by_characteristics,
  us_unemployment,
  us_unemployment_by_characteristics,
  us_median_house_price
)
reduce(data_list,full_join,by="date") ->dataset_project
dataset_project |> 
  pivot_longer(!date,names_to = "index",values_to = "value") -> dataset_project
write_csv(dataset_project,file = "./data/project_dataset.csv")