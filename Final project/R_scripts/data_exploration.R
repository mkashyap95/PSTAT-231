#data exploration

#packages
library(dplyr)
library(tidyselect)

#reading in data
setwd("C:\\Users\\kashyap\\Desktop\\PSTAT-231\\Final project\\data\\processed")
df_comp <- read.csv("cleaned.csv")


#pair plot
df_numeric <- select_if(df_comp, is.numeric)
pairs(df_numeric)
