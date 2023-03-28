install.packages("devtools")
library(devtools)
install_github("vqv/ggbiplot")
require(ggbiplot)
library(ggplot)
library(dplyr)
library(lubridate)
library(ggplot2)
library(depmixS4) 

### Import data ###
getwd()
setwd("C:/Users/ricks/Desktop/CMPT 318/FinalProject/Data")
df <- read.table("Term_Project_Dataset.txt", header = TRUE, sep = ",")
df <- na.omit(df)

scaled_data <- cbind(df["Date"], df["Time"], scale(df[, c(3:9)]))

pcs <- prcomp(scaled_data[, c(3:9)])

plot(pcs)
summary(pcs)
print(pcs)

ggbiplot(pcs)

