# GamePad Gaussians - Association Analytics
# Created: 2017-03-07

#setwd("Documents/R_sketches/")

# Initialize libraries
#library(arules)
#library(arulesViz)

# Import data
#gpg_data <- read.table("Data/gameplay_data_V/2017-04-01-16-21-58_gamepadKeys.txt", sep = "\t", header = TRUE)

gpg_data <- read.table("Data/gameplay_data_J/2017-04-02-15-13-35_gamepadKeys.txt", sep = "\t", header = TRUE)

leftJoy <- abs(joy1_int) > 0
rightJoy <- abs(joy2_int) > 0
attach(gpg_data)
names(gpg_data)

# Convert keypresses to transactions
buttons <- data.frame(red = as.logical(A1), 
                      green = as.logical(A2), 
                      blue = as.logical(A3), 
                      opacity = as.logical(A4),
                      dispersion = as.logical(L1),
                      position = as.logical(R2),
                      size = as.logical(R1),
                      random = as.logical(L2),
                      reset = as.logical(S1),
                      saveIMG = as.logical(S2),
                      up = as.logical(up),
                      down = as.logical(down),
                      left = as.logical(left),
                      right = as.logical(right),
                      leftJoy = as.logical(joy1_int),
                      rightJoy = as.logical(joy2_int))

trans <- as(buttons, "transactions")
summary(trans)

# Color value frequencies
hist(red, col = "red")
hist(green, col = "green")
hist(blue, col = "blue")

# Support
itemFrequency(trans)

# Frequent n-itemsets
itemsets <- apriori(trans, parameter = list(minlen = 1, maxlen = 5, support = 0.00001, target="frequent itemsets"))
summary(itemsets)

global_associations <- inspect(head(sort(itemsets, by = "support"), 100))
global_associations

# Write data
write.table(x = global_associations, file = "J4_global_associations.tsv", sep = "\t")


# Interval subsets
interval_itemsets <- apriori(trans[2237:2972], parameter = list(minlen = 1, maxlen = 5, support = 0.00001, target="frequent itemsets"))
summary(interval_itemsets)

inspect(head(sort(interval_itemsets, by = "support"), 100))

