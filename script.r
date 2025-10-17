library(tidyverse)
library(janitor)

#Import data and begin initial exploration

data <- read_csv(file = 'data-raw/Soccer Force Plate Data.csv') |> 
  #formats column names to a more R friendly format
  clean_names() 


data |> 
  #filter test type to CMJ
  filter(test_type == 'CMJ') |> view()
