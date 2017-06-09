library(readr)
library(dplyr)
library(ggplot2)
data_products_purchase <- read_delim("~/Documentos/workspace/chaordic-challenge/data/data_products.csv", 
                                     ";", escape_double = FALSE, trim_ws = TRUE)
catalog <- read_csv("~/Documentos/workspace/chaordic-challenge/data/catalog")


data_products_purchase$is_purchase <- TRUE
summary(data_products_purchase)
summary(catalog)


data_products_purchase <- left_join(data_products_purchase, catalog, by="pid")
summary(data_products_purchase)

levels(as.factor(data_products_purchase$category))
levels(as.factor(data_products_purchase$sub_category))
levels(as.factor(data_products_purchase$sub_sub_category))


data_products_purchase %>% ggplot(aes(x=category, fill=gender)) + geom_histogram(stat = "count") + coord_flip() +ggtitle("Category")
data_products_purchase %>% ggplot(aes(x=sub_category, fill=gender)) + geom_histogram(stat = "count") + coord_flip() + ggtitle("Subcategory")
data_products_purchase %>% ggplot(aes(x=sub_sub_category, fill=gender)) + geom_histogram(stat = "count") + coord_flip() + ggtitle("SubSubcategory")


categories <- levels(as.factor(data_products_purchase$category))

for(category in categories){
  data_products_purchase[category] <- ifelse(data_products_purchase$category == category, TRUE, FALSE)
  #data_products_purchase[category] <- as.factor(data_products_purchase[category])
}

subcategories <- levels(as.factor(data_products_purchase$sub_category))

for(sub_category in subcategories){
  data_products_purchase[sub_category] <- ifelse(data_products_purchase$sub_category == sub_category, TRUE, FALSE)
  #data_products_purchase[sub_category] <- as.factor(data_products_purchase[sub_category])
}


subsubcategories <- levels(as.factor(data_products_purchase$sub_sub_category))

for(sub_sub_category in subsubcategories){
  data_products_purchase[sub_sub_category] <- ifelse(data_products_purchase$sub_sub_category == sub_sub_category, TRUE, FALSE)
  #data_products_purchase[sub_sub_category] <- as.factor(data_products_purchase[sub_sub_category])
}


data_products_purchase$promocao <- ifelse(data_products_purchase$current_price < data_products_purchase$original_price, TRUE, FALSE)
summary(data_products_purchase$promocao)


data_products_purchase$id <- paste0(data_products_purchase$uid, data_products_purchase$pid, data_products_purchase$date)




df <- data_products_purchase[,6:length(data_products_purchase)] 
df$quantity <- data_products_purchase$quantity
summary(df)
glimpse(df)


df_factors <- df %>% select(-id, -quantity, -gender, -current_price, -original_price, -category, -sub_category, -sub_sub_category)
df_factors$promocao <- ifelse(is.na(df_factors$promocao), FALSE, df_factors$promocao)
df_factors$gender <- df$gender
glimpse(df_factors)


df_factors[] <- lapply(df_factors, factor)
glimpse(df_factors)

df_not_factors <- df %>% select(id, quantity, current_price, original_price)
summary(df_not_factors)

df_not_factors$current_price <- ifelse(is.na(df_not_factors$current_price), 0, df_not_factors$current_price)
df_not_factors$original_price <- ifelse(is.na(df_not_factors$original_price), 0, df_not_factors$original_price)
df_not_factors$price_discount <- df_not_factors$current_price - df_not_factors$original_price

df_purchase <- cbind(df_not_factors, df_factors)

glimpse(df_purchase)

df_purchase <- droplevels(df_purchase)
summary(df_purchase)

write.csv(df_purchase, "df_purchase.csv", row.names = FALSE)


