library(tidyverse)
library(janitor)
library(skimr)

#Import data and begin initial exploration

data <- read_csv(file = 'data-raw/Soccer Force Plate Data.csv') |> 
  #formats column names to a standard format adjsut some data types like date
  clean_names() |> 
  mutate(date=mdy(date)) 

#filter down to CMJ test specifically remove any entirely empty columns and provide brief overview of data
data<-data |> 
  filter(test_type=='CMJ') |> 
  remove_empty("cols") |> 
  glimpse()

# Visualize missing data pattern before proceeding
data |> 
  ungroup() |> 
  summarise(across(everything(), ~mean(is.na(.)))) |> 
  pivot_longer(everything(), names_to = "column", values_to = "pct_missing") |> 
  filter(pct_missing > 0.70) |> 
  arrange(desc(pct_missing)) |> 
  ggplot(aes(pct_missing, column)) + 
  geom_col(width = .25)


#Since there is three trials per athlete, I'll combine find the mean of the trials with numeric values 
# data<-data |> 
#   group_by(date,athlete,position_group) |> 
#   summarise(
#     across(where(is.numeric), ~mean(., na.rm = TRUE)),
#     across(where(is.character), ~first(.))  # Keep first value for character columns
#   ) |> 
#   ungroup() 


# Selects only columns containing any of these strings
data<-data |> select(contains(c('athlete','date','peak_power','position_group','weight','jump_height','peak_force','concentric_impulse')))

#Peak power = peak_power_w
#Mean propulsive force = mean_concentric_peak_power
#Propulsive impulse (product of force and time during ascent) = concentric_impulse_100ms_ns
#Jump height = which to use?


# Calculate the mean for position groups 
# data |> 
#   group_by(position_group) |> 
#   summarise(
#     across(where(is.numeric), ~mean(., na.rm = TRUE)),
#     across(where(is.character), ~first(.))  # Keep first value for character columns



#Find individuals z-scores








