# Load libraries RCurl -> for loading data from URL, plotly -> for making plots interactive, ggrepel -> for Text Labels
library(RCurl)
library(plotly)
library(ggrepel)

# Load Data from Remote URL
rData <- getURL('http://vincentarelbundock.github.io/Rdatasets/csv/HSAUR/water.csv')

# Read CSV data in lData (local data), test connection , header by default is true
lData <- read.csv(textConnection(rData))

# View Data
knitr::kable(lData)

# Data Summary
summary(lData)


# Scatter Diagram 1
b <- ggplot(lData, aes(x = mortality , y = hardness)) 
b + geom_point(aes(color = hardness, size = mortality, shape = factor(location))) +  scale_colour_gradient(low = "purple") 
ggplotly()

# Scatter Diagram 2
ggplot(subset(lData, location %in% c("North", "South")),
       aes(x=mortality, y=hardness, color=location)) + geom_point() +  geom_smooth() + geom_text_repel(aes(label=town), size = 3)
ggplotly()

# Bar Chart Hardness vs Mortality
ggplot(lData, aes(x=mortality, y=hardness))  + geom_bar(stat = "identity") + geom_smooth()
ggplotly()


# Create Histogram for Mortality
ggplot(data=lData, aes(lData$mortality)) +
  geom_histogram(aes(y =..density.., fill=..count..), breaks=seq(1000, 2000, by = 100), alpha = .5 ) +
  geom_density(col=1) + labs(title="Histogram for Mortality", x="Mortality", y="Count")  +
  geom_vline(aes(xintercept=mean(mortality)), color="blue", linetype="dashed", size=1)
ggplotly()

# Create Histogram for Water Hardness
ggplot(data=lData, aes(lData$hardness)) +
  geom_histogram(aes(y =..density.., fill=..count..), breaks=seq(0, 150, by = 10), alpha = .5 ) +
  geom_density(col=2) + labs(title="Histogram for Water Hardness", x="Water Hardness", y="Count") +
  geom_vline(aes(xintercept=mean(hardness)), color="blue", linetype="dashed", size=1)
ggplotly()


# North vs South Mortality Histogram
ggplot(lData, aes(x=mortality, color=location, fill=location)) +
  geom_histogram(position="identity",breaks=seq(1000, 2000, by = 100), alpha=0.5)+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  labs(title="Mortality histogram plot",x="Mortality", y = "Count")
ggplotly()

# North vs South Water Hardness Histogram
ggplot(lData, aes(x=hardness, color=location, fill=location)) +
  geom_histogram(position="identity",breaks=seq(0, 150, by = 10), alpha=0.5)+
  scale_color_manual(values=c("#999999", "#BEE181", "#5B6BAA"))+
  scale_fill_manual(values=c("#999999", "#BEE181", "#5B6BAA"))+
  labs(title="Water Hardness histogram plot",x="hardness", y = "Count")
ggplotly()


# Dotplot: Grouped Sorted and Colored
# Sort by mortality, group and color by location
xm <- lData[order(lData$mortality),] # sort by mortality
xm$loc <- factor(xm$location) # it must be a factor
xm$color[xm$location=="North"] <- "red"
xm$color[xm$location=="South"] <- "blue"
dotchart(xm$mortality,labels=row.names(xm),cex=.50,groups= xm$location,
         main="Mortality rates grouped by location",
         xlab="Mortality rate", gcolor="green", color=xm$color)

# Dotplot: Grouped Sorted and Colored
# Sort by hardness, group and color by location
wHard <- lData[order(lData$hardness),] # sort by hardness
wHard$loc <- factor(wHard$location) # it must be a factor
wHard$color[wHard$location=="North"] <- "red"
wHard$color[wHard$location=="South"] <- "blue"
dotchart(wHard$hardness,labels=row.names(xm),cex=.50,groups= wHard$location,
         main="Water Hardness grouped by location",
         xlab="Water Hardness", gcolor="Gray", color=wHard$color)

#Barplot for location(North, South)
counts <- table(lData$location)
barplot(counts, main="Location", 
        xlab="Location")

#Mortality Boxplot based on Location
boxplot(lData$mortality~lData$location,data=lData, main="Mortality data", 
        xlab="Location", ylab="Mortality data")

# Water Hardness based on Location
boxplot(lData$hardness~lData$location,data=lData, main="Water Hardness", 
        xlab="Location", ylab="Water Hardness")

#  Hardness vs Mortality Boxplot based on Location
ggplot(lData, aes(x=mortality, y=hardness, color=location)) + geom_boxplot()




































