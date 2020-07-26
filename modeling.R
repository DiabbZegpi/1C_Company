#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Packages~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse) # Data manipulation and ploting
library(tictoc) # Measuring running time of code

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sales_joined <- read_csv("Data/sales_joined.csv") 
sales_test <- read_csv("Data/test.csv")%>% 
  mutate(date_block_num = 34)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Modeling~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Fitting n models: the brute force way
n <- 10000
best_sellers_items <- items_per_month %>% 
  group_by(ID) %>% 
  summarize(total_items = sum(item_cnt_month)) %>% 
  top_n(n, total_items) %>% 
  pull(ID)

tic(glue::glue("Fiting {n} models"))
items_per_month %>% 
  filter(ID %in% best_sellers_items) %>% 
  nest(data = c(date_block_num, item_cnt_month)) %>% 
  mutate(model = map(data, ~lm(item_cnt_month ~ date_block_num, data = .)),
         summary = map(model, broom::glance),
         r2 = map_dbl(summary, "r.squared")) %>% 
  select(ID, r2)
toc()


# Conclusion: r-squared too low. Not a recommended path.

# Fitting item_sales ~ shop_id + item_id + month

raw_fit_data <- sales_joined %>% 
  select(date_block_num, shop_id, item_id, item_cnt_day) %>% 
  group_by(date_block_num, shop_id, item_id) %>% 
  summarize(item_cnt_month = sum(item_cnt_day)) %>% 
  ungroup()

raw_fit <- lm(item_cnt_month ~ date_block_num + shop_id + item_id, 
              data = raw_fit_data)  

summary(raw_fit)

raw_fit_prediction <- sales_test %>% 
  bind_cols(item_cnt_month = predict(raw_fit, sales_test)) %>% 
  select(ID, item_cnt_month)

write_csv(raw_fit_prediction, "submission1.csv")
