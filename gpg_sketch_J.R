
setwd("Documents/R_sketches/")

# Import data
txt_files <- list.files("Data/gameplay_data_J/", pattern = "txt")
png_files <- list.files("Data/gameplay_data_J/", pattern = "png")

# 
file_index <- 4
file_path <- paste0("Data/gameplay_data_J/", txt_files[file_index])
gpg_data <- read.table(file_path, sep = "\t", header = TRUE)

attach(gpg_data)

# Row = observation; Column = variable
obs <- dim(gpg_data)[1]
str(gpg_data)


# Convert to pauses
secs <- millis/1000 - millis[1]/1000
pause <- NULL

for (i in 1:length(secs))
  pause[i] <- secs[i+1] - secs[i]

pause.ts <- ts(pause)


# Display interactive time series chart
library(dygraphs)
dygraph(pause.ts, xlab = "Keypress index", ylab = "Pause length", 
        main = paste0("J ", file_index, " ", txt_files[file_index], " ", obs)) %>%
  dySeries("V1", label = "Seconds") %>%
  dyLegend(show = "always", hideOnMouseOut = FALSE) %>%
  dyRangeSelector()


# Length of play session before reset (seconds)
# The initial session will have a longer pause due application startup
(millis[dim(gpg_data)[1]] - millis[1]) / 1000

# Number of action frames captured
dim(gpg_data)[1]

# Average pause length during session (seconds)
((millis[dim(gpg_data)[1]] - millis[1]) / 1000) / obs


p_threshold <- 8
which(pause > p_threshold)

hist(which(gpg_data$L2 == TRUE))
hist(which(gpg_data$R2 == TRUE))

hist(which(gpg_data$A1 == TRUE))
hist(which(gpg_data$A2 == TRUE))
hist(which(gpg_data$A3 == TRUE))
