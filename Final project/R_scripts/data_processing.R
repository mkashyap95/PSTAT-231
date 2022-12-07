#data processing

#packages
library(janitor)
library(tidyverse)
library(tidyselect)


#reading in data
df <- read.csv("bgg_dataset.csv", sep = ";")
df_clean <- as_tibble(df) %>%
  clean_names()

#data overview
summary(df_clean)

#changing rating scores to numbers
convert_string <- function(x){
  as.numeric(str_replace_all(x,",","."))
}
df_clean[c("rating_average","complexity_average")] <- lapply(df_clean[c("rating_average","complexity_average")], convert_string)

head(df_clean)
summary(df_clean)


#dropping the NA values - mostly in Owned users
df_comp <- na.omit(df_clean)

#changing year published to a time scale by converting it to game age
convert_year <- function(y){
  2022 - y
}
df_comp$game_age <- sapply(df_comp$year_published, convert_year)

#summary of new df
summary(df_comp)

#checking nas in domain and mechanics
df_comp[19000,"domains"]
df_comp[df_comp == ""] <- NA

#counting NAs
dim(df_comp[is.na(df_comp$domains),])
dim(df_comp[!is.na(df_comp$domains),])

dim(df_comp[is.na(df_comp$mechanics),])
dim(df_comp[!is.na(df_comp$mechanics),])


#some special characters in mechanics -- change to commas
clean_string <- function(z){
  str_replace_all(z, "/",",")
}
df_comp$mechanics <- lapply(df_comp$mechanics, clean_string)

df_comp$domains
df_comp$mechanics


