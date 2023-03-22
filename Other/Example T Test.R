
library(ggpubr)

set.seed(1234)
my_data <- data.frame(
  name = paste0(rep("M_", 10), 1:10),
  weight = round(rnorm(10, 20, 2), 1)
)


res <- t.test(my_data$weight, mu = 25)

print(res)