df1 <- read.csv("data_season_state.csv")
df1 <- df1[df1$state!="DC",]
df1[df1$season==1,"records"] <- df1[df1$season==1,"records"]*1.5
df2 <- read.csv("state_season_gdp.csv")
df2$records <- df1$records
records <- as.numeric(df2$records)
record.ratio <- c(records[1:50]/sum(records[1:50]),
                  records[51:100]/sum(records[51:100]),  
                  records[101:150]/sum(records[101:150]),
                  records[151:200]/sum(records[151:200])) 

df2$record.ratio <- record.ratio

unemploydf <- read.csv("unemploy.csv")
unemploy <- unemploydf[order(unemploydf$abb),3]
names(unemploy) <- state.abb[order(state.abb)]
unemploy <- unemploy[state.abb]

library(ggplot2)


ggplot(df2[1:50,])+
  geom_point(aes(x = GDP,y = record.ratio)) +
  geom_text(aes(x = GDP,y = record.ratio+0.0025,label = abb))
