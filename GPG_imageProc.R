# Gamepad Gaussians - Image Processing Sketch
# Created: 2017-03-31

library(EBImage)

# Read data, display img, view histogram, object summary
img <- readImage("Data/gamePad_sketch_01.jpg", type = "jpeg")
display(img)
hist(img); img

# - - -

# Separate RGB layers, view mean color value
# Maybe consider median color value too

redFrame <- imageData(img)[, , 1]
sum(redFrame) / (1280*800) * 255

greenFrame <- imageData(img)[, , 2]
sum(greenFrame) / (1280*800) * 255

blueFrame <- imageData(img)[, , 3]
sum(blueFrame) / (1280*800) * 255

unique(imageData(img)[, , 3])

median(imageData(img)[, , 1]) * 255
median(imageData(img)[, , 2]) * 255
median(imageData(img)[, , 3]) * 255

# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}



x <- c(1,2,3)
y <- c(1,2,3)
x == y

imageData(img)[,,1]
imageData(img)[,,2]
imageData(img)[,,3]

gMode <- getmode(imageData(img)[,,2])

gMode_index <- which(imageData(img)[,,2] == gMode)
head(gMode_index)

imageData(img)[130,131,2]


bool <- NULL
for (i in 1:1280) {
  
}