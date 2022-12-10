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
# one hot encode the domains and mechanics columns
# Step 1: Convert strings to lists
convert_str_to_list <- function(x){
  c(strsplit(x, ", "))
}
df_comp$mechanics <- lapply(df_comp$mechanics, convert_str_to_list)
df_comp$domains <- lapply(df_comp$domains, convert_str_to_list)

# Step 2: Get all unique values
unique_mechanics = c()
for (row in 1:nrow(df_comp)) {
  unique_mechanics = union(unique_mechanics,unlist(df_comp[row, "mechanics"]))
}

unique_domains = c()
for (row in 1:nrow(df_comp)) {
  unique_domains = union(unique_domains,unlist(df_comp[row, "domains"]))
}

# Step 3: Create dummy columns
prep_column_name <- function(x){
  paste0("Domain ", x)
}
unique_domain_cols <- lapply(unique_domains, prep_column_name)
prep_column_name <- function(x){
  paste0("Mechanic ", x)
}
unique_mechanic_cols <- lapply(unique_mechanics, prep_column_name)

df_comp[unlist(unique_domain_cols)] <- 0
df_comp[unlist(unique_mechanic_cols)] <- 0

# Step 4: Populate dummy columns
for (row in 1:nrow(df_comp)) {
  dom_vec = unlist(df_comp[row, "domains"])
  for (dom_idx in 1:length(dom_vec)){
    dom_col = paste0("Domain ", dom_vec[dom_idx])
    df_comp[row, dom_col] = 1
  }

  mech_vec = unlist(df_comp[row, "mechanics"])
  for (mech_idx in 1:length(mech_vec)){
    mech_col = paste0("Mechanic ", mech_vec[mech_idx])
    df_comp[row, mech_col] = 1
  }
}

# Step 5: Drop one column to avoid dummy variable trap
# Drop the least populated column in each
min_sum_mech = nrow(df_comp)
min_idx_mech = 0
for (col_idx in 1:length(unique_mechanic_cols)){
  sum_ = sum(df_comp[, unique_mechanic_cols[col_idx][[1]]])
  if (sum_<min_sum_mech){
    min_idx_mech = col_idx
    min_sum_mech = sum_
  }
}

min_sum_dom = nrow(df_comp)
min_idx_dom = 0
for (col_idx in 1:length(unique_domain_cols)){
  sum_ = sum(df_comp[, unique_domain_cols[col_idx][[1]]])
  if (sum_<min_sum_dom){
    min_idx_dom = col_idx
    min_sum_dom = sum_
  }
}

drop <- c(unique_domain_cols[min_idx_dom][[1]],unique_mechanic_cols[min_idx_mech][[1]])
df_final = df_comp[,!(names(df_comp) %in% drop)]
#dummy variables

