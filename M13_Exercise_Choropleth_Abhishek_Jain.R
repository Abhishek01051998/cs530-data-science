# Loading the libraries
library(ggplot2)
library(maps)
library(mapdata)
usa <- map_data("usa")
states <- map_data("state")
counties <- map_data("county")
names(states)[names(states) == "region"] <- "State"
names(counties)[names(counties) == "region"] <- "State"
state_base <- ggplot(data = states, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) + geom_polygon(col = "black", fill = "gray")

milesDriven <- read.csv("milesdrive.csv")
milesDriven$State <- tolower(milesDriven$State)
stateMileMerged <- merge(states, milesDriven, by = "State")
choropleth1 <- state_base +
  geom_polygon(data = stateMileMerged, aes(fill = VMT), color = "white") +
  geom_polygon(col = "black", fill = NA) + theme_bw() +
  ggtitle("Miles driven in different States") +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen")
choropleth1


lifeExpectancy <- read.csv("lifeexp.csv")
lifeExpectancy$State <- tolower(lifeExpectancy$State)
stateLifeMerged <- merge(states, lifeExpectancy, by = "State")
choropleth2 <- state_base +
  geom_polygon(data = stateLifeMerged, aes(fill = Life.Expectancy), color = "white") +
  geom_polygon(col = "black", fill = NA) +
  theme_bw() + ggtitle("Life Expectancy in different States") +
  scale_fill_gradient(low = "white", high = "darkblue")
choropleth2


stateLifeMergedQual <- merge(counties, lifeExpectancy, by = "State")
subsetLife <- subset(stateLifeMergedQual, stateLifeMergedQual$Life.Expectancy >= 79)
choropleth3 <- state_base +
  geom_polygon(data = counties, fill = NA, color = "gray") +
  geom_polygon(col = "black", fill = NA) +
  geom_point(inherit.aes = F, aes(x = long, y = lat),
             color ="yellow", size = 0.5, data = subsetLife) +
  ggtitle("Counties in States with high life expectancy ( >= 79)")
choropleth3
