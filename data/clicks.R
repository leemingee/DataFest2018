library(dplyr)

datafest2018NewApril6 <- read_csv("./data/datafest2018NewApril6.csv")

data.USA <- datafest2018NewApril6[datafest2018NewApril6$country == 'US',]

data.USA$localClicks

category.salary.state <- data.USA %>% 
  group_by(stateProvince, normTitleCategory) %>% 
  summarise(mean.clicks = mean(clicks, na.rm = TRUE),
            mean.localClicks = mean(localClicks, na.rm = TRUE))


############## Mean clicks of Each TitleCategory #########################################################################################################
normTitleCategory_state_salary <- na.omit(read_csv("./data/normTitleCategory_state_salary.csv"))

#data.raw$INCIDENT_CLASSIFICATION[levels(data.raw$INCIDENT_CLASSIFICATION),]
# Create data
#data=data.frame(group=paste("Group_", sample(letters, 70, replace=T), sample(letters, 70, replace=T), sample(letters, 70, replace=T), sep="" ), value=sample(seq(1,70),70)) 

all_salary_mean <- tapply(category.salary.state$mean.clicks,category.salary.state$normTitleCategory,mean)

normTitleCategory <- data.frame(Category = sort(unique(category.salary.state$normTitleCategory)),
                                salary = all_salary_mean)

# normTitleCategory_state_salary$normTitleCategory[normTitleCategory_state_salary$normTitleCategory=='NA']


data <- data.frame(group = normTitleCategory$Category, value = normTitleCategory$salary)

# story <- paste(round(normTitleCategory$salary))

# Add a column with the text you want to display for each bubble:
data$text<-paste("Category: ",data$group, "\n", "mean clicks:", round(data$value))

# Generate the layout
packing <- circleProgressiveLayout(data$value, sizetype='area')
data = cbind(data, packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)

# Make the plot with a few differences compared to the static version:
p = ggplot() + 
  geom_polygon_interactive(data = dat.gg, aes(x, y, group = id, fill=id, tooltip = data$text[id], data_id = id), colour = "black", alpha = 0.5) +
  scale_fill_viridis(option = "B", begin = 0.4) +
  geom_text(data = data, aes(x, y, label = gsub("Group_", "", group)), size=4, color="black") +
  theme_void() + 
  theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
  coord_equal()+
  labs(title = "Number of Facilities in Each Borough", size = 5)+
  theme(plot.title = element_text(colour = "black", face = "bold", size = 20, vjust = 1, hjust = 0.5))




widg=ggiraph(ggobj = p, width_svg =7, height_svg =7)
widg


################# Mean clicks in each state ################################################################################################################


all_salary_mean <- tapply(normTitleCategory_state_salary$mean.estimatedSalary,normTitleCategory_state_salary$stateProvince,mean)

normTitleCategory <- data.frame(Category = sort(unique(normTitleCategory_state_salary$stateProvince)),
                                salary = all_salary_mean)

# normTitleCategory_state_salary$normTitleCategory[normTitleCategory_state_salary$normTitleCategory=='NA']


data <- data.frame(group = normTitleCategory$Category, value = normTitleCategory$salary)

# story <- paste(round(normTitleCategory$salary))

# Add a column with the text you want to display for each bubble:
data$text<-paste("States: ",data$group, "\n", "mean salary:", round(data$value))

# Generate the layout
packing <- circleProgressiveLayout(data$value, sizetype='area')
data = cbind(data, packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)

# Make the plot with a few differences compared to the static version:
p = ggplot() + 
  geom_polygon_interactive(data = dat.gg, aes(x, y, group = id, fill=id, tooltip = data$text[id], data_id = id), colour = "black", alpha = 0.5) +
  scale_fill_viridis(option = "B", begin = 0.4) +
  geom_text(data = data, aes(x, y, label = gsub("Group_", "", group)), size=4, color="black") +
  theme_void() + 
  theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
  coord_equal()+
  labs(title = "Mean salary in each state", size = 5)+
  theme(plot.title = element_text(colour = "black", face = "bold", size = 20, vjust = 1, hjust = 0.5))




widg=ggiraph(ggobj = p, width_svg =7, height_svg =7)
widg