# Example code for exam 

## subtitle: how to import external data 

The packages needed in the script are the following:

```r
library(terra) #managing raster and vector data
library(imageRy) #analysing RS data 

```
set the working directory:

backtick to (`)insert code

```r
library(terra)
setwd("~/Downloads/")
```

## data visulazation
to check of the folder you can use R
```r
getwd()
```
Import of the data: 
```r
rast("") #for raster data
```
In order to plot the image we will use the RGB scheme

```r
im.plotRGB(name, r=1, g=2, b=3)
#layer 1 =red wavelength, layer2= green, layer3=blue

#picture flipped
picture <- flip(name)

```


The plot can be exported
```r
png("Group photo.png")
im.plotRGB(name, r=1, g=2, b=3)
dev.off()
```

the output image looks like (drag and drop):

![bologna-con-2-torri-scaled-8f71b5b8](https://github.com/user-attachments/assets/46808466-ecf8-4d65-8ae4-21e7e1e67164)

Let's invert the bands 



# Others 

```r
im.list() #Listing files 
