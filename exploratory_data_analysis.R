#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Loading packages~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse)
library(forecast)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Loading data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Train data (90 Mb file)
sales_train <- read_csv("Data/sales_train.csv",
                        col_types = cols(date = col_date(format = "%d.%m.%Y")))

# Test data
sales_test <- read_csv("Data/test.csv")

# Shops data (useless)
shops <- read_csv("Data/shops.csv")

# Items data
items <- read_csv("Data/items.csv")


# Example of submission
# ID is on test data
sample_submission <- read_csv("Data/sample_submission.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Data pre-processing~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sales_cleaned <- sales_train %>% 
  left_join(sales_test, by = c("shop_id", "item_id")) %>% 
  select(-shop_id, -item_id) %>% 
  filter(!is.na(ID))

items_per_month <- sales_cleaned %>% 
  group_by(date_block_num, ID) %>% 
  summarize(item_cnt_month = sum(item_cnt_day)) 

# Saving useful datasets  
write_csv(sales_cleaned, "Data/sales_cleaned.csv")
write_csv(items_per_month, "Data/items_per_month.csv")
