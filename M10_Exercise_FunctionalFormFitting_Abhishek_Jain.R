a<-read.csv("penguin.csv")
year <- a$Year
numPenguins <- a$Penguins
scatter1 <- plot(x = year, y = numPenguins,
                 
                 main = "Number of Penguins through the years (1981-2003)",
                 xlab = "Year", ylab = "Number of Penguins", pch = 20,
                 xlim = c(1980,2005), ylim = c(10000, 55000))

yearSquare <- year^2
quadCurve <- lm(numPenguins ~ year + yearSquare)
yearSeq <- seq(1981, 2005, 0.1)
penguinPred <- predict(quadCurve, list(year = yearSeq, yearSquare = yearSeq^2))
lines(yearSeq, penguinPred, col="red", lwd=4)


beerfrothData<-read.csv("beerfroth.csv")
time <- beerfrothData$Time
foam <- beerfrothData$Foam
scatter2 <- plot(x = time, y = foam,
                 
                 main = "Beer Froth/Foam height as time passes for 5 min",
                 xlab = "Time passed (in seconds)",
                 ylab = "Foam Height (in cm)",
                 pch = 20, ylim = c(0, 20))

expCurve<-lm(log(foam)~time)
beerPred<-exp(predict(expCurve, list(time)))
lines(time, beerPred, col="green", lwd=4)