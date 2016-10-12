library(readr)
library(rvest)
library(jsonlite)
library(httr)


income_df = read.csv("input/ACS_14_5YR_B19013_with_ann.csv",skip =1)
colnames(income_df)[4]<-"estimate_median"
colnames(income_df)[5]<-"marginal_error"

income_df$Geography=gsub("ZCTA5 ","" ,income_df$Geography)
income_df$Geography = as.factor(income_df$Geography)
income_df$estimate_median = as.numeric(income_df$estimate_median)

df_income <- income_df[c("Geography","estimate_median")]

load("target_info.Rda")







