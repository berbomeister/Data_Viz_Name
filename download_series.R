library(fredr)
fredr_set_key("")

library(tidyverse)

unemployment_us <-fredr(series_id = "UNRATE")
unemployment_us |> 
  select(date,value) |> 
  rename(US_unemployment_rate = value)|> write_csv(file = "./data/us_total_unemployment.csv")


state_ur <- str_c(state.abb, "UR")
map_dfr(state_ur, fredr) |> 
  select(date,series_id,value)|> 
  pivot_wider(names_from = series_id,values_from = value) |> 
  rename_with(
    \(x) str_sub(x,1,2) |> str_c("_UR"),
    ends_with("UR"))|> 
  write_csv(file = "./data/state_total_unemployment.csv")


raw_data <- read_csv(file="./data/file.csv")
raw_data |> mutate(
  month = str_remove(Period,"M") |> as.numeric(),
  date = make_date(Year,month)
) |> select(date,`Series ID`,Value)|> 
  pivot_wider(names_from = `Series ID`, values_from = Value) |> 
  select(-LNS14000000) |> 
  rename(
    UR_men = LNS14000001,
    UR_women = LNS14000002,
    UR_white = LNS14000003,
    UR_white_men = LNS14000004,
    UR_white_women = LNS14000005,
    UR_black = LNS14000006,
    UR_black_men = LNS14000007,
    UR_black_women = LNS14000008,
    UR_hispanic = LNS14000009,
    UR_16_19 = LNS14000012,
    UR_16_19_men = LNS14000013,
    UR_16_19_women = LNS14000014,
    UR_16_19_white = LNS14000015,
    UR_16_19_white_men = LNS14000016,
    UR_16_19_white_women = LNS14000017,
    UR_16_19_black = LNS14000018,
    UR_16_19_black_men = LNS14000019,
    UR_16_19_black_women = LNS14000020,
    UR_16_19_hispanic = LNS14000021,
    UR_16_19_hispanic_men = LNS14000022,
    UR_16_19_hispanic_women = LNS14000023,
    UR_20_plus = LNS14000024,
    UR_20_plus_men = LNS14000025,
    UR_20_plus_women = LNS14000026,
    UR_20_plus_white = LNS14000027,
    UR_20_plus_white_men = LNS14000028,
    UR_20_plus_white_women = LNS14000029,
    UR_20_plus_black = LNS14000030,
    UR_20_plus_black_men = LNS14000031,
    UR_20_plus_black_women = LNS14000032,
    UR_20_plus_hispanic = LNS14000033,
    UR_20_plus_hispanic_men = LNS14000034,
    UR_20_plus_hispanic_women = LNS14000035,
    UR_20_24 = LNS14000036,
    UR_20_24_men = LNS14000037,
    UR_20_24_women = LNS14000038,
    UR_25_plus = LNS14000048,
    UR_25_plus_men = LNS14000049,
    UR_25_plus_women = LNS14000050,
    UR_25_64 = LNS14000060,
    UR_25_64_men = LNS14000061,
    UR_25_64_women = LNS14000062,
    UR_16_17 = LNS14000086,
    UR_18_19 = LNS14000088,
    UR_25_34 = LNS14000089,
    UR_35_44 = LNS14000091,
    UR_45_54 = LNS14000093
  ) |> 
  write_csv(file="./data/us_unemployment_by_characteristics.csv")


us_labour_participation <- fredr(series_id = "CIVPART")
us_labour_participation |> select(date,series_id,value) |> 
  pivot_wider(names_from = series_id,values_from = value) |> 
  rename(
    labour_participation = CIVPART
  ) |> 
  write_csv(file="./data/us_total_labour_participation.csv")


foo <- function(x) {
  num_vector = c(1,2,4,5,6,8,9,10,12,13,15,16:42,44:51,53:56)
  index <- which(num_vector == x)
  
  state.abb[index]
}

state_nums<-c(1,2,4,5,6,8,9,10,11,12,13,15,16:42,44:51,53:56)
state_labour_codes <- str_c("LBSSA", str_pad(state_nums, width = 2, pad = "0"))
# state_labour_codes
map_dfr(state_labour_codes,fredr) |> 
  select(date,series_id,value) |> 
  pivot_wider(names_from = series_id,values_from = value) |> 
  rename(DC_labour_participation_rate = LBSSA11) |> 
  rename_with(
    \(x) str_remove(x,"LBSSA") |> as.numeric() |> foo() |> str_c("_labour_participation_rate"),
    starts_with("LBSSA")
  ) |> 
  write_csv(file="./data/state_total_labour_participation.csv")


total_median_income <- fredr(series_id = "MEHOINUSA672N")
total_median_income |> 
  select(date,value) |>
  rename(US_median_income = value) |> 
  write_csv(file="./data/total_median_income.csv")


state_median_income_ids <- fredr_series_search_id(search_text = "MEHOINUS*672N")$id[-1]
map_dfr(state_median_income_ids,fredr) |> 
  select(date,series_id,value) |> 
  pivot_wider(names_from = series_id,values_from = value) |>
  rename_with(
    \(x) str_sub(x,9,10) |> str_c("_median_income"),
    -date
  ) |> 
  write_csv(file="./data/state_median_income.csv")

total_house_price <- fredr(series_id = "MSPUS")
total_house_price |>
  select(date,value) |> rename(US_median_house_price = value) |> 
  write_csv(file="./data/total_median_house_price.csv")


state_house_price_id <- fredr_series_search_id(search_text = "*STHPI")$id
map_dfr(state_house_price_id[c(1:5,11,17:61)],fredr) |>
  select(date,series_id,value) |>
  pivot_wider(names_from = series_id,values_from = value) |>
  rename_with(\(x) str_sub(x,1,2) |> str_c("_house_price_index"),ends_with("STHPI")) |> 
  write_csv(file="./data/state_median_house_price_index.csv")


LPR_by_characteristics <- read_csv(file="./data/LFPRbyraceethnicityandsex.csv")
LPR_by_characteristics |> distinct() |> mutate(date = ymd(Year,truncated = 2)) |> 
  select(-Year) |>
  pivot_wider(names_from = `Measure Names`,values_from = `Measure Values`) |>
  rename_with(
    \(x) str_replace_all(x,pattern = " ",replacement = "_") |>  str_c("_LPR"),
    -date) |> write_csv(file="./data/us_labour_by_characteristics.csv")

