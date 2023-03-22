#install.packages("ggpubr")

library(ggpubr)

setwd("C:/BRERC")



my_data <- read.csv("telfer.csv")

results <- t.test(my_data$Telfer_1_2, mu = 25)

print(results)
