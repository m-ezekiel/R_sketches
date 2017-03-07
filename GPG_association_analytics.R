# GamePad Gaussians - Association Analytics
# Created: 2017-03-07

# Initialize libraries
setwd("~/Documents/R_sketches/")
library(arules)
library(arulesViz)

# Import data
gpg_data <- read.table("2017-3-4-16-52-8_gamepadKeys.txt", sep = "\t", header = TRUE)
leftJoy <- abs(joy1_int) > 10
rightJoy <- abs(joy2_int) > 10
attach(gpg_data)
names(gpg_data)

# Convert keypresses to transactions
buttons <- data.frame(blue = as.logical(A1), green = as.logical(A2), red = as.logical(A3), opacity = as.logical(A4), disp = as.logical(L1), size = as.logical(R1), random = as.logical(L2), mute = as.logical(R2), select1 = as.logical(S1), select2 = as.logical(S2), up = as.logical(up), down = as.logical(down), left = as.logical(left), right = as.logical(right), leftJoy = as.logical(leftJoy), rightJoy = as.logical(rightJoy))
trans <- as(buttons, "transactions")
summary(trans)

# Color value frequencies
hist(red, col = "red")
hist(green, col = "green")
hist(blue, col = "blue")

#support
itemFrequency(trans)

# frequent n-itemsets
itemsets2 <- apriori(trans, parameter = list(minlen = 1, maxlen = 5, support = 0.002, target="frequent itemsets"))
summary(itemsets2)
inspect(head(sort(itemsets2, by = "support"), 20))
