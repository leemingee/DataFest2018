library(datamodelr)
library(data.table)
library(tidyverse)
library(xgboost)
library(ggplot2)
library(DT)
library(Hmisc)
library(readr)
library(plyr)
# Model completeness, Presentation/visualization, Business insight, Best use of external data
test <- read_csv("csc_df_s18/data/datafest2018NewApril6.csv", col_names = TRUE)
names(test)
str(test)


dm_f <- dm_from_data_frames(test)
#dm_f <- dm_add_references(dm_f, login$User_ID == demographics$User_ID)
# dm_f <- dm_add_references(dm_f, login$engageable_type == login$engageable_id)
graph <- dm_create_graph(dm_f, rankdir = "BT", col_attr = c("column", "type"))
dm_render_graph(graph)

# length(state.abb)

head(test$date)
length(unique(test$companyId))

sum_missing <- function(x){
  return(sum(x = is.na(x)))
}
datatable(test[, lapply(X = .SD, FUN = "sum_missing")])


### cleaning #####
test <- test %>% subset(country == "US")
unique.states.in.test <- unique(test$stateProvince)
states.selected <- c(state.abb, "DC")
test <- test %>% 
  filter(stateProvince %in% states.selected)
unique.city <- unique(test$city)
unique.city.table <- table(test$city)
city.post.threshold <- 30
unique.city.selected <- subset(unique.city.table, unique.city.table > city.post.threshold)
test2 <- test %>% filter(city %in% names(unique.city.selected))
# test2, subset for the city

# write.csv(test2, "data_clean_1.csv")

library(lubridate)


unique.industry <- unique(test$industry)
sum(is.na(test$industry))/length(test$industry)
unique.company <- unique(test$companyId)

ca.data <- test2 %>% filter(stateProvince == 'CA')
tx.data <- test2 %>% filter(stateProvince == 'TX')
ny.data <- test2 %>% filter(stateProvince == 'NY')

# ca.data %>% group_by_(.dots = c("city", "normTitleCategory")) %>% count()
ca.by.category <- ca.data %>% group_by(normTitleCategory) %>% count()
ca.by.category <- setorder(ca.by.category, n)
ca.by.city <- setorder(ca.data %>% group_by(city) %>% count(), n)

ca.by.category.city <- ca.data %>% group_by_(.dots = c('normTitleCategory', 'city')) %>% count()
tail(setorder(ca.by.category.city, n))


### analyze for the unusual cities ###
tx.data$click.ratio <- tx.data$localClicks/tx.data$clicks
tx.by.category.city <- tx.data %>% group_by_(.dots = c('normTitleCategory', 'city')) %>% count()
tail(setorder(tx.by.category.city, n))

# Austin
names(test2)
library(lubridate)
test2$wday <- wday(test2$date)
test2$month <- month(test2$date)
test2$year_month <- substr(test2$date, 0,7)

test2$year <- year(test2$date)
Sys.time()
test2$year.month <- paste(test2$year, test2$month, sep = "-")
Sys.time

wdaycount <- test2 %>% group_by_(.dots = c('year_month','wday')) %>% count()


class(test$month)


### omit 2017
test2[test2]


#####
# remove 2017.10 and 2017.11
test <- test2[test2$year_month != "2017-10" & test2$year_month != "2017-11",]

test2$year_month

definsea <- function(df){
  if(df$month >=1  & df$month<= 3 ){
    return("2")
  }
  if(df$month >=4  & df$month<= 6 ){
    return("3")
  }
  if(df$month >=7  & df$month<= 9 ){
    return("4")
  }
  if(df$month >=10 ){
    return("1")
  }
}

definsea2 <- function(df){
  if(df >=1  & df<= 3 ){
    return("2")
  }
  if(df >=4  & df<= 6 ){
    return("3")
  }
  if(df >=7  & df<= 9 ){
    return("4")
  }
  if(df >=10 ){
    return("1")
  }
}



test$season <- sapply(test$month, definsea2)

data_season_state <- dlply(test,.(season,stateProvince))

season.state <- paste(test$season,test$stateProvince)
count.season.state <- count(season.state)

data_season_state <- data.frame(records = count.season.state$freq,
                                season = substr(count.season.state$x, 0, 1),
                                state = substr(count.season.state$x, 3, 4))
substr('1 AX',3, 4)
table(test$season)
