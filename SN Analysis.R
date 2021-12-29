#Zac Boiskin
#ServiceNow Case Study
#June 2021

library(tidyverse)
library(dplyr)
library(readxl)
library(corrplot)
library(scales)


#Importing Dataset and Initial Data Glance
SN_Cust<- read_excel("2021 Career Docs/Final ServiceNow Spreadsheet.xlsx")
glimpse(SN_Cust)
head(SN_Cust)
summary(SN_Cust)

#Subsetting Data for Correlation Matrix, keeping only numeric columns
SN_Cust_Corr_Subset <- subset(SN_Cust, select = -c(1:3,9:11,13:17,20))

#Imputing missing data
list_na <- colnames(SN_Cust_Corr_Subset)[ apply(SN_Cust_Corr_Subset, 2, anyNA) ]
median_missing <- apply(SN_Cust_Corr_Subset[,colnames(SN_Cust_Corr_Subset) %in% list_na],
      2,
      median,
      na.rm = TRUE)

SN_Cust_Corr_Subset_Impute <- SN_Cust_Corr_Subset %>% 
  mutate(replace_median_upgrades = ifelse(is.na(`Number of Version Upgrades by the Customer since they started`),median_missing[1], 
                                          `Number of Version Upgrades by the Customer since they started`),
         replace_median_adoption = ifelse(is.na(`Product Adoption`), median_missing[2], `Product Adoption`))

SN_Cust_Corr_Subset_Impute_Final <- subset(SN_Cust_Corr_Subset_Impute, select = -c(7:8))



#Creating correlation plot to discover significant factors
SN_Corr_Matrix <- cor(SN_Cust_Corr_Subset_Impute_Final)


#Creating Correlation Plot
corrplot(SN_Corr_Matrix,method = "number", tl.cex = .5)

#Significant factors from correlation plot:
#Annual change in Revenue~Current Yearly Sub = .95
#Upgrades~Customer Start Quarter = -.64
#Upgrades~Years Active = .64
