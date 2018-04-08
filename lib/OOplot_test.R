library(ggiraph)


library(packcircles)
library(ggplot2)
library(viridisLite)
library(viridis)
library(ggiraph)

library(readr)
# normTitleCategory_state_salary <- na.omit(read_csv("./data/normTitleCategory_state_salary.csv"))

#data.raw$INCIDENT_CLASSIFICATION[levels(data.raw$INCIDENT_CLASSIFICATION),]
# Create data
#data=data.frame(group=paste("Group_", sample(letters, 70, replace=T), sample(letters, 70, replace=T), sample(letters, 70, replace=T), sep="" ), value=sample(seq(1,70),70)) 

# all_salary_mean <- tapply(normTitleCategory_state_salary$mean.estimatedSalary,normTitleCategory_state_salary$normTitleCategory,mean)
normTitleCategory_state_salary <- statedf

normTitleCategory <- data.frame(Category = sort(unique(normTitleCategory_state_salary$state)),
                                salary = normTitleCategory_state_salary$numpost)

# normTitleCategory_state_salary$normTitleCategory[normTitleCategory_state_salary$normTitleCategory=='NA']


data <- data.frame(group = normTitleCategory$Category, value = normTitleCategory$salary)

# story <- paste(round(normTitleCategory$salary))

# Add a column with the text you want to display for each bubble:
data$text<-paste("Category: ",data$group, "\n", "mean salary:", round(data$value))

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
  labs(title = "Number of Clicks in each State, USA", size = 5)+
  theme(plot.title = element_text(colour = "black", face = "bold", size = 20, vjust = 1, hjust = 0.5))


widg=ggiraph(ggobj = p, width_svg =7, height_svg =7)
widg
