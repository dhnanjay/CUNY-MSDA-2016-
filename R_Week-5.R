library(hflights)

# Select columns from hflight and create a new datset
newData <- subset(hflights, 
                  select=c("Month", "DayOfWeek", "UniqueCarrier", "ArrDelay","DepDelay", "Dest", "Distance"),
                  na.rm = TRUE)

# Rename Month
newData$Month <- replace(newData$Month, newData$Month=="1", "January")
newData$Month <- replace(newData$Month, newData$Month=="2", "February")
newData$Month <- replace(newData$Month, newData$Month=="3", "March")
newData$Month <- replace(newData$Month, newData$Month=="4", "April")
newData$Month <- replace(newData$Month, newData$Month=="5", "May")
newData$Month <- replace(newData$Month, newData$Month=="6", "June")
newData$Month <- replace(newData$Month, newData$Month=="7", "July")
newData$Month <- replace(newData$Month, newData$Month=="8", "August")
newData$Month <- replace(newData$Month, newData$Month=="9", "September")
newData$Month <- replace(newData$Month, newData$Month=="10", "October")
newData$Month <- replace(newData$Month, newData$Month=="11", "November")
newData$Month <- replace(newData$Month, newData$Month=="12", "December")

# Get Mean of Distance
summary(newData$Distance)


# create new subsets of Delayed Arrival based on Short and Long distance and additional Column FlightType
sDistDelay <- subset(newData, ArrDelay > 0 & Distance < 810)
sDistDelay$FlightType <- "Short Distance";
nrow(sDistDelay)
lDistDelay <- subset(newData, ArrDelay > 0 & Distance > 809)
lDistDelay$FlightType <- "Long Distance";
nrow(lDistDelay)
# Merge the above two dataset to create one dataset which has both Arrival & Departure Delay with additonal column FlightType
#  which signify whether Flight is Long Distance or Short Distance
totalDelay <- rbind(sDistDelay, lDistDelay)

# Bar chart to display which Short Distance Carrier is most delayed 
ggplot(sDistDelay, aes(x=UniqueCarrier, y=frequency(UniqueCarrier)))  + geom_bar(stat = "identity")  +
  labs(title="Arrival Delay of Short Distance Flights based on different Carriers", x="Carrier", y="Count") 

# Bar chart to display which Long Distance Carrier is most delayed 
ggplot(lDistDelay, aes(x=UniqueCarrier, y=frequency(UniqueCarrier))) + geom_bar(stat = "identity")  +
  labs(title="Arrival Delay of Long Distance Flights based on different Carriers", x="Carrier", y="Count") 

# Comparison of Long Distance vs Short Distance Delay
qplot(ArrDelay, data = totalDelay, geom = "freqpoly", colour = FlightType, xlim =c(-20, 200)) +
  labs(title="Long Distance vs Short Distance", x="Arrival Delay", y="Count") 

sDistDelay$Month <- factor(sDistDelay$Month,levels=month.name)
sDistDelay = sDistDelay[order(sDistDelay$Month,decreasing=FALSE),]
# Which month has most delayed flights for Short Distance
ggplot(sDistDelay, aes(x=Month, y=frequency(Month)))  + geom_bar(stat = "identity") 

lDistDelay$Month <- factor(lDistDelay$Month,levels=month.name)
lDistDelay = lDistDelay[order(lDistDelay$Month,decreasing=FALSE),]
# Which month has most delayed flights for Long Distance
ggplot(lDistDelay, aes(x=Month, y=frequency(Month)))  + geom_bar(stat = "identity") 

# Scatter Chart to show Arrival delayed vs Long distance
b <- ggplot(lDistDelay, aes(x = Distance , y = ArrDelay)) 
b + geom_point(aes(color = UniqueCarrier)) +
  labs(title="Scatter chart for Long Distance vs Arrival Delay", x="Distance", y="Arrival Delay") 

# Scatter Chart to show Arrival delayed vs Short distance
c <- ggplot(sDistDelay, aes(x = Distance , y = ArrDelay)) 
c + geom_point(aes(color = UniqueCarrier)) +
  labs(title="Scatter chart for Short Distance vs Arrival Delay", x="Distance", y="Arrival Delay") 

# Histograms to compare Arrival Delay by Unique Carrier
qplot(ArrDelay, data=totalDelay, geom='histogram', binwidth = 5, xlim =c(-25, 200), main='Arrival Delays by Airlines') +
  facet_wrap(~UniqueCarrier) + labs(x = 'Arrival Delay', y = "Count")

totalDelay$Month <- factor(totalDelay$Month,levels=month.name)
totalDelay = totalDelay[order(totalDelay$Month,decreasing=FALSE),]
# Histograms to compare Arrival Delay by Month
qplot(ArrDelay, data=totalDelay, geom='histogram', binwidth = 5, xlim =c(-25, 200), main='Arrival Delays by Month') +
  facet_wrap(~Month) + labs(x = 'Arrival Delay', y = "Count")

# Get Top 5 Destination
topDest <- data.frame(table(newData$Dest))
topDest <- subset(topDest, Freq > 6000)
head(topDest)

# Filter Data by Top 5 Destination
topdest <- subset(newData, Dest == 'ATL' | Dest == 'DAL' | Dest == 'DFW' | Dest == 'LAX' | 
                    Dest == 'MSY' )

# Density chart comparison between top 5 destination showing Arrival Delay of differnt carriers
qplot(ArrDelay, data=topdest, geom='density', color=UniqueCarrier, xlim =c(-25, 150)) + facet_grid(Dest ~.)
ggplotly()
# Histograms to compare Arrival Delay by Top 5 Destination
topdest2 <- subset(totalDelay, Dest == 'ATL' | Dest == 'DAL' | Dest == 'DFW' | Dest == 'LAX' | 
                     Dest == 'MSY' )
qplot(ArrDelay, data=topdest2, geom='histogram', binwidth = 5, xlim =c(0, 150), main='Arrival Delays by Destination') +
  facet_wrap(~Dest) + labs(x = 'Arrival Delay', y = "Count")


# Scatter Chart to show relationship between Arrival Delay vs Departure Delay
b2 <- ggplot(newData, aes(x = DepDelay , y = ArrDelay)) 
b2 + geom_point(aes(color = UniqueCarrier)) + geom_smooth() + 
  labs(title="Scatter chart for Departure vs Arrival Delay", x="Departure Delay", y="Arrival Delay")