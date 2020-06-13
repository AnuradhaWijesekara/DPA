paddyData=read.csv("C:/Users/MISCO/Desktop/New folder (2)/Paddy data.csv", header = TRUE)
paddyData

mean(paddyData$Sown.000.Ha)
mean(paddyData$Harvested.000.Ha.)
mean(paddyData$Production.000.Mt.)

productivity= (paddyData$Harvested.000.Ha./paddyData$Sown.000.Ha)*100
productivity


par(mfrow=c(1,4))
barplot(paddyData$Sown.000.Ha, xlab="Season", main="Sown Area", ylab="Area in Ha.", names.arg = c("Maha","Yala"), cex.main = 1)
barplot(paddyData$Harvested.000.Ha.,main="Harvested Area", xlab="Season", ylab="Area in Ha.", names.arg = c("Maha","Yala"), cex.main = 1)
barplot(paddyData$Average.Yield.Kg.Ha, xlab="Season", main="Average yield kg/Ha", ylab="Average yield kg/Ha.", names.arg = c("Maha","Yala"), cex.main = 1)
barplot(paddyData$Sown.000.Ha, xlab="Season", main="Production", ylab="Production in Mt.", names.arg = c("Maha","Yala"), cex.main = 1)
