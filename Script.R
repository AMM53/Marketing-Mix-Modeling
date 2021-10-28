rm(list=ls())
library(janitor)
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
## We use relative adresses in order to retrieve the data.
## In this case, W. directory is set to the same folder the script is being
## ran from, where mktmix.csv is also located.

df_mmm <- clean_names(read.csv("mktmix.csv"))

#
ncol(df_mmm)
nrow(df_mmm)
class(df_mmm$base_price)
class(df_mmm$discount)

#
df_mmm$newspaper_inserts <-
  as.numeric(as.factor(df_mmm$newspaper_inserts)) - 1
class(df_mmm$newspaper_inserts)
df_mmm$newspaper_inserts

#
unique(as.factor(df_mmm$website_campaign))
length(unique(as.factor(df_mmm$website_campaign))) - 1

#
df_mmm %>% 
  select(website_campaign) %>%
  mutate(website_campaign = replace(website_campaign, website_campaign=="Website Campaign ", "Website Campaign"),
         Facebook = as.numeric(website_campaign == "Facebook"),
         Twitter = as.numeric(website_campaign == "Twitter"),
         WebsiteCampaign = as.numeric(website_campaign == "Website Campaign"))

# for some reason the "Website Campaign" values were input incorrectly
# in the data frame, leaving a space after Campaign as in
# "Website Campaign ". We are modifying that too to prevent future
# mistakes.

#
sales_date <- c(1:length(df_mmm$new_vol_sales))
ggplot(df_mmm) +
  geom_line(aes(y = new_vol_sales, x = sales_date))

hist(df_mmm$new_vol_sales, breaks = 20)
boxplot(df_mmm$new_vol_sales)
# Based mainly on the results obtained via boxplot, we can assume
# the median for new_vol_sales will be somewhere around 20000.
median(df_mmm$new_vol_sales)

#
df_media <- df_mmm %>% 
  select(tv, radio, stout)%>%
  pivot_longer(everything())

df_media %>% 
  ggplot(aes(x=seq_len(nrow(df_media)), y=value))+
  geom_line()+
  facet_grid(name~., scales="free")

#
df_mmm %>% 
  ggplot(aes(x=in_store, y=new_vol_sales)) +
  geom_point()

# function for variable-based point color

scatter_color <- function(j){
  df_mmm %>% 
    ggplot(aes(x=in_store, y=new_vol_sales, color=j)) +
    geom_point()
}

# based on newspaper_inserts
scatter_color(as.factor(df_mmm$newspaper_inserts))

# based on tv column
scatter_color(df_mmm$tv)

#  
df_mmm <- df_mmm %>% 
  group_by(
    discount_yesno = ifelse(discount==0, FALSE, TRUE))

df_mmm_mean <- df_mmm %>% 
  select(base_price, discount_yesno) %>% 
  group_by(discount_yesno) %>% 
  summarize(average_price = mean(base_price)) %>% 
  ggplot(aes(x=discount_yesno,
             y=average_price,
             fill=discount_yesno)) +
  geom_col() +
  coord_cartesian(ylim=c(14.5,15.5))

df_mmm_mean$data
df_mmm_mean

#
regresion_columns <- function(columns){
  df_aux <- df_mmm %>% 
    select(new_vol_sales, columns, discount_yesno)
  my_model <- lm(new_vol_sales ~ ., data = df_aux)
  return(summary(my_model)$adj.r.squared)
}

#
l1 <- c("base_price", "radio", "tv", "stout")
l2 <- c("base_price", "in_store", "discount", "radio", "tv", "stout")
l3 <- c("in_store", "discount")
lista_regresion <- list(l1, l2, l3)
map <- map(lista_regresion, regresion_columns)

for (j in 1:3){
  print(c("The variables used for the model are:",
          lista_regresion[[j]],
          "And the R-Squared for the model is:"))
  print(map[[j]])
}

# Simple calculation of the maximum rsquared value
map <- unlist(map)
mapmax <- max(map)
mapmaxlista <- lista_regresion[[which.max(map)]]

print(c("Maximum R-squared value found was:",
        mapmax,
        "Variables used to estimate the regression were:",
        mapmaxlista))
