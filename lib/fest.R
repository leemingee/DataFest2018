# setwd("C:/Users/50347/Desktop")
library(readr)
library(plyr)

#data <- read_csv("Datafest/data/datafest2018NewApril6.csv")
data <- read_csv("datafest2018NewApril6.csv")

head(data)
dataus <- data[data$country=="US",c("date","stateProvince","normTitle",
                                    "estimatedSalary", "city","clicks","localClicks")]

month <- substring(dataus$date,1,7)

dataus$month <- month
data_month <- dlply(dataus,.(month))


data_state <- dlply(dataus,.(stateProvince))

state_name <- sort(unique(dataus$stateProvince))

ratioclick <- function(df){
  sumclick <- sum(df$clicks)
  sumlocal <- sum(df$localClicks)
  return(sumlocal/sumclick)
}

ratio <- laply(data_state,ratioclick) # clickratio on states
names(ratio) <- state_name
ratio_df <- as.data.frame(ratio)
num_post <- laply(data_state,nrow)
names(num_post) <- state_name
numpost_df <- as.data.frame(num_post)
#data_industry <- alply(dataus,.(industry))

unemploy <- read.csv("unemploy.txt")
unemploy <- unemploy[-c(49,nrow(unemploy)),]

unemploy$State <- as.character(unemploy$State)
setdiff(as.character(unemploy$State),state.name)

state_abb <- function(namestate){
  df <- data.frame("name"=as.character(state.name),
                   "abb"=as.character(state.abb))
  if(!(namestate %in% df$name)){
    return("WRONG")
  }
  return(df[df$name==namestate,2])
}

#state_abb(df,"Nevada")

abb<- apply(data.frame(unemploy$State),1,state_abb)

unemploy$abb <- abb

unemploy <- unemploy[order(unemploy$abb),]
unemploy_rate <- unemploy$rate
names(unemploy_rate) <- unemploy$abb

pop <- read.csv("pop.txt")
pop <- pop[pop$Geographic.Area%in%state.abb,-(3:5)]
pop <- pop[order(pop$Geographic.Area),]

#state_del <- setdiff(names(ratio), state.abb)
ratio <- ratio[order(state.abb)]
num_post <- num_post[order(state.abb)]

state_df <- data.frame("state"=state.abb[order(state.abb)],"clickratio"=ratio,
                       "numpost"=num_post,"unemploy"=unemploy_rate,
                       "population"=pop$population)
