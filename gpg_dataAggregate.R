
setwd("Documents/R_sketches/")

# Import data
txt_files <- list.files("Data/gameplay_data_N/", pattern = "txt")
png_files <- list.files("Data/gameplay_data_N/", pattern = "png")

file_path <- paste0("Data/gameplay_data_N/", txt_files[1])
gpg_data <- read.table(file_path, sep = "\t", header = TRUE)



## Player summary data transformations
actionFrames <- NULL
totalTime <- NULL
imgExports <- NULL

for (i in 1:length(txt_files)) {
  file_path <- paste0("Data/gameplay_data_N/", txt_files[i])
  gpg_data <- read.table(file_path, sep = "\t", header = TRUE)
  actionFrames[i] <- dim(gpg_data)[1]
  totalTime[i] <- (gpg_data$millis[dim(gpg_data)[1]] - gpg_data$millis[1]) / 1000
  imgExports[i] <- length(which(gpg_data$S2 == 1))
}



## Pause transformations
secs <- gpg_data$millis/1000 - gpg_data$millis[1]/1000
pause <- NULL

for (i in 1:length(secs))
  pause[i] <- secs[i+1] - secs[i]

p_min <- min(na.omit(pause))
p_max <- max(na.omit(pause))
p_median <- median(na.omit(pause))
p_mean <- mean(na.omit(pause))

ps16 <- NULL
ps8 <- NULL
ps4 <- NULL
ps2 <- NULL
ps1 <- NULL

for (i in 1:length(txt_files)) {
  file_path <- paste0("Data/gameplay_data_N/", txt_files[i])
  gpg_data <- read.table(file_path, sep = "\t", header = TRUE)
  
  secs <- gpg_data$millis/1000 - gpg_data$millis[1]/1000
  pause <- NULL
  
  for (k in 1:length(secs))
    pause[k] <- secs[k+1] - secs[k]
  
  p_min[i] <- min(na.omit(pause))
  p_max[i] <- max(na.omit(pause))
  p_median[i] <- median(na.omit(pause))
  p_mean[i] <- mean(na.omit(pause))
  
  ps16[i] <- length(which(pause >= 16))
  ps8[i] <- length(which(pause >= 8))
  ps4[i] <- length(which(pause >= 4))
  ps2[i] <- length(which(pause >= 2))
  ps1[i] <- length(which(pause >= 1))
}



## Associations




# Aggregate data frame by player
player_data <- data.frame(sessionLog = txt_files, actionFrames, totalTime_secs = totalTime, imgExports, p_16sec = ps16, p_8sec = ps8, p_4sec = ps4, p_2sec = ps2, p_1sec = ps1, pauseMin = p_min, pauseMax = p_max, pauseMedian = p_median, pauseMean = p_mean)

head(player_data)

# Write data
write.table(x = player_data, file = "player_data_N.tsv", sep = "\t")
