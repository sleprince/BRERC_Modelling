#Install Sparta

install.package('lme4')
install.packages('Rcpp')
library(Rcpp)
library(lme4)

write('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', file = "~/.Renviron", append = TRUE)
Sys.which("make")

install.packages("jsonlite", type = "source")
library(devtools)

# Some users have reported issues with devtools not correctly installing
# dependencies. Run the following lines to avoid these issues
list.of.packages <- c("minqa", "lme4", "gtools", "gtable", "scales",
                      "assertthat", "magrittr", "tibble", "stringr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Now install sparta
install_github('BiologicalRecordsCentre/sparta')

# Load sparta
library(sparta)

?sparta
