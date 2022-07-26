setwd("C:/BRERC")
library(sparta)
MyData<-read.csv("BRERC2_dataset_vsmall2.csv") #this is the CSV made in run first?


#Needs to be Y-M-D
MyData$dateofrecord <- as.Date(MyData$dateofrecord)
class(MyData$dateofrecord)


# Run some data diagnostics on our data
results <- dataDiagnostics(taxa = MyData$Species,
                           site = MyData$Site,
                           time_period = MyData$dateofrecord,
                           progress_bar = FALSE)
print(A)

trace("dataDiagnostics", edit=TRUE)


print("Hello")

debug(dataDiagnostics)
undebug(dataDiagnostics)