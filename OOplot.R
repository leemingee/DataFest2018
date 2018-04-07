library(packcircles)
library(ggplot2)
library(viridisLite)
library(viridis)
library(ggiraph)

data.raw<-read.csv(choose.files())
save(data.raw, file = "facility.RData")
# 
# data.raw<-load(choose.files())

summary(data.raw$Borough)

#data.raw$INCIDENT_CLASSIFICATION[levels(data.raw$INCIDENT_CLASSIFICATION),]
# Create data
#data=data.frame(group=paste("Group_", sample(letters, 70, replace=T), sample(letters, 70, replace=T), sample(letters, 70, replace=T), sep="" ), value=sample(seq(1,70),70)) 

data <- data.frame(group = levels(data.raw$Borough), value = summary(data.raw$Borough))

story<-c("", "Second Most in Number of Fire Incidents ", "Far Less Fire Incidents than that in Brooklyn and Queen", "Most Fire Incidents Among All", "Least in both Facilities and Fire Incidents Number")

# Add a column with the text you want to display for each bubble:
data$text<-paste("Borough: ",data$group, "\n", "Facility Number:", data$value, "\n", story)

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