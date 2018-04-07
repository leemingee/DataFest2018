setwd("C:/Users/50347/Desktop")
library(readr)
library(plyr)

data <- read_csv("Datafest/data/datafest2018NewApril6.csv")

head(data)
dataus <- data[data$country=="US",c("date","stateProvince","normTitle",
                                    "estimatedSalary", "city","clicks","localClicks")]

month <- substring(dataus$date,1,7)

data_state <- dlply(dataus,.(stateProvince))

ratioclick <- function(df){
  sumclick <- sum(df$clicks)
  sumlocal <- sum(df$localClicks)
  return(sumlocal/sumClick)
}


#data_industry <- alply(dataus,.(industry))

head(data_state$AK)
