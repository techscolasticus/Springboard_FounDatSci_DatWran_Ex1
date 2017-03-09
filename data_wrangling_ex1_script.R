#load required packages
library(readr)
library(tidyr)
library(plyr)
library(dplyr)

#read csv file
refine_original_df <- read_csv("~/Foundations of Data Science/Data Wrangling Exercise 1/refine_original.csv")

#fix company names
for (i in 1:length(refine_original_df$company)){
  if (grepl("^p|^P|^f", refine_original_df[i, 1])) {
    refine_original_df$company[grepl("^p|^P|^f", refine_original_df$company)] <- 'phillips'
  } else if (grepl("^a|^A", refine_original_df[i, 1])) {
    refine_original_df$company[grepl("^a|^A", refine_original_df$company)]  <- 'akzo'
  } else if (grepl("^u|^U", refine_original_df[i, 1])) {
    refine_original_df$company[grepl("^u|^U", refine_original_df$company)] <- 'unilever'
  } else if (grepl("^v|^V", refine_original_df[i, 1])) {
    refine_original_df$company[grepl("^v|^V", refine_original_df$company)] <- 'van houten'
  }
}
  
#separate Product code / number column and create new data frame
refine_original_df2 <- separate(refine_original_df, `Product code / number`, c("product_code", "product_number"), sep = "-")

#add product_category column
refine_original_df2["product_category"] <- NA

#move product_category after product_number column
refine_original_df2 <- refine_original_df2[c(1,2,3,8,4,5,6,7)]

#add values in product_category columns
refine_original_df2$product_category[refine_original_df2$product_code=="p"] <- "Smartphone"
refine_original_df2$product_category[refine_original_df2$product_code=="v"] <- "TV"
refine_original_df2$product_category[refine_original_df2$product_code=="x"] <- "Laptop"
refine_original_df2$product_category[refine_original_df2$product_code=="q"] <- "Tablet"

#concatenate address fields into one column
refine_original_df2 <- unite(refine_original_df2, "full_address", address, city, country, sep = ", ")

#create dummy binary variables for company and product_category columns
refine_original_df2$company_phillips <- as.numeric(refine_original_df2$company == 'phillips')
refine_original_df2$company_akzo <- as.numeric(refine_original_df2$company == 'akzo')
refine_original_df2$company_van_houten <- as.numeric(refine_original_df2$company == 'van houten')
refine_original_df2$company_unilever <- as.numeric(refine_original_df2$company == 'unilever')

refine_original_df2$product_smartphone <- as.numeric(refine_original_df2$product_category == 'Smartphone')
refine_original_df2$product_tv <- as.numeric(refine_original_df2$product_category == 'TV')
refine_original_df2$product_laptop <- as.numeric(refine_original_df2$product_category == 'Laptop')
refine_original_df2$product_tablet <- as.numeric(refine_original_df2$product_category == 'Tablet')

#save new data frame as refine_clean.csv
write.csv(refine_original_df2, "~/Foundations of Data Science/Data Wrangling Exercise 1/refine_clean.csv")