#data exploration

#packages
library(dplyr)
library(tidyselect)
library(corrplot)
library(ggplot2)
library(GGally)
library(caret)

#reading in data
setwd("C:\\Users\\kashyap\\Desktop\\PSTAT-231\\Final project\\data\\processed")
df_comp <- read.csv("cleaned.csv")

#heatmap
df_numeric <- select_if(df_comp, is.numeric)
df_numeric %>%
  cor() %>%
  corrplot(type = "lower", diag = F, method = "color")

#dropping owned users
df_comp <- subset(df_comp, select=-owned_users)


###OHE

#dummy variables

