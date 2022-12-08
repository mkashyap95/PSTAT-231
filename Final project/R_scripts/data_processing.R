#data processing

#packages
library(janitor)
library(tidyverse)
library(tidyselect)
library(keras)
library(dplyr)


#reading in data
setwd("C:\\Users\\kashyap\\Desktop\\PSTAT-231\\Final project\\data\\unprocessed")
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
df_comp <- drop_na(df_clean)
summary(df_comp)


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


#replacing NA values
df_comp$domains <- replace(df_comp$domains, is.na(df_comp$domains), "Basic")
df_comp$mechanics <- replace(df_comp$mechanics, is.na(df_comp$mechanics), "Not mentioned")


#some special characters in mechanics -- change to commas
clean_string <- function(z){
  str_replace_all(z, "/",",")
}
df_comp$mechanics <- lapply(df_comp$mechanics, clean_string)

df_comp$domains
df_comp$mechanics


#converting to string
df_comp$mechanics <- as.character(df_comp$mechanics)
df_comp$mechanics[10]

df_comp <- as.data.frame(df_comp)

#dropping unused variables
df_comp <- subset(df_comp, select=-c(id,year_published))

#adding stars -- binned column for rating
df_comp$Stars = cut(df_comp$rating_average, breaks = seq(0,10,2), labels = c(1,2,3,4,5))


#tokenizing mechanics and domains
# tokenizer_mech <- text_tokenizer(split = ",") %>%
#   fit_text_tokenizer(df_comp$domains)


#saving to csv
write.csv(df_comp, "C:\\Users\\kashyap\\Desktop\\PSTAT-231\\Final project\\data\\processed\\cleaned.csv", row.names=FALSE)
