#data processing

#packages
library(janitor)
library(tidyverse)


#data
df <- read.csv("bgg_dataset.csv", sep = ";")
df_clean <- as_tibble(df) %>%
  clean_names()

#changing rating scores to numbers
convert_string <- function(x){
  as.numeric(str_replace_all(x,",","."))
}
df_clean[c("rating_average","complexity_average")] <- lapply(df_clean[c("rating_average","complexity_average")], convert_string)

head(df_clean)
