# Image Processing
# Created: 2017-03-31

# Install package
# source("http://bioconductor.org/biocLite.R")
# biocLite("EBImage")

library(EBImage)

## Greyscale images
# Intensity scale from 0 to 1 for amount of white per pixel coordinate

black <- readImage("Data/Color_swatches/black_0_0_0.jpg")
hist(black); black

white <- readImage("Data/Color_swatches/white_255_255_255.jpg")
hist(white); white

grey <- readImage("Data/Color_swatches/grey_127_127_127.jpg")
hist(grey); grey

## Color images

# The green bar cannot be seen, but is located at 0.0
red <- readImage("Data/Color_swatches/red_255_0_0.jpg")
hist(red); red

# RGB scale 0 to 255  and Intensity scale from 0 to 1.0, so Blue = 128 -> Intensity = 0.5
dblue <- readImage("Data/Color_swatches/blue_0_74_128.jpg")
hist(dblue); dblue

# There is NOT a 1 to 1 translation in intensity from RGB to Grayscale
dblue_asGray <- readImage("Data/Color_swatches/dblue_asGrayscale.jpg")
hist(dblue_asGray); dblue_asGray
