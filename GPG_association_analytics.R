# GamePad Gaussians - Association Analytics
# Created: 2017-03-07

# Initialize libraries
library(arules)
library(arulesViz)

# Import data
gpg_data <- read.table("Data/AF-7958_17.3.5_19-38-32.txt", sep = "\t", header = TRUE)
# Set threshold above 10 because leftJoy idles between 0 and ~6-10 (device error)
leftJoy <- abs(joy1_int) > 6
rightJoy <- abs(joy2_int) > 0
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

# Support
itemFrequency(trans)

# Frequent n-itemsets
itemsets2 <- apriori(trans, parameter = list(minlen = 2, maxlen = 5, support = 0.00001, target="frequent itemsets"))
summary(itemsets2)
inspect(head(sort(itemsets2, by = "support"), 20))
