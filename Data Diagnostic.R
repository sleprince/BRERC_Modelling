setwd("D:/BRERC")
library(sparta)
MyData<-read.csv("BRERC.csv")


#Needs to be Y-M-D
MyData$dateofrecord <- as.Date(MyData$dateofrecord)
class(MyData$dateofrecord)


# Run some data diagnostics on our data
results <- dataDiagnostics(taxa = MyData$Species,
                           site = MyData$Site,
                           time_period = MyData$dateofrecord,
                           progress_bar = FALSE)
