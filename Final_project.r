#############################################################################################################
### Spatial Distribution of Bird Observations in Urban and Natural Areas of Eilenriede City Park, Hanover ###
#############################################################################################################

#Research question:
  #Do bird observations differ in density between urban and natural land-cover types in Eilenriede City Park,
  #and how is this associated with vegetation cover (NDVI)?

#Assumption: 
  #Initially assumption is that bird observations would be more frequent in urbanised areas
  #This expectation is based on the idea that higher human presence in these areas increases the likelihood of birds being seen and recorded
  #Since the data comes from human observations (GBIF), areas with more visitors are more likely to have observations uploaded


######################
### The Study area ###
######################

#The aim was to choose a study area with around 50% natural areas and 50% urban areas 
#Why the City park Eilenriede in Hanover:
  #Because it is one of the biggest forests included in a city in Europe 
  #Provides a gradient from highly vegetated areas to areas with human influence
  #Easily accessible to humans, leading to rich citizen science data (e.g., GBIF observations)


# Longitude (°E) | Latitude (°N)
#--------------------------------
# 9.744528       | 52.39625
# 9.818168       | 52.39625
# 9.818168       | 52.35133
# 9.744528       | 52.35133
# 9.744528       | 52.39625

#Area of 5km x 5km = 25km^2

###########################
### The ecological data ###
###########################

#The ecological data was downloaded from: https://www.gbif.org/occurrence/download?basis_of_record=HUMAN_OBSERVATION&has_coordinate=true&has_geospatial_issue=false&taxon_key=212&year=2016,2016&geometry=POLYGON((9.74453%2052.39625,9.74453%2052.35133,9.81817%2052.35133,9.81817%2052.39625,9.74453%2052.39625))
#The analysis focuses on bird observations from the GBIF database collected in the year 2019

#About the data:
  #Data about bird observations in the study area 
  #Human observations 
  #Geometry: POLYGON((9.74453 52.39625,9.74453 52.35133,9.81817 52.35133,9.81817 52.39625,9.74453 52.39625))
  #Has coordinates: true, has geopspatial issues: false 
  #Species analysed (Scientific name): Aves (898 occurrences)
  #Date between the start of 2019 and the end of 2019
  #GBIF.org (21 January 2026) GBIF Occurrence Download
  #https://doi.org/10.15468/dl.8zukfu

##########################
### The Satellite data ###
##########################

#Satellite imagery for the study area was sourced from Copernicus
  #Providing high-resolution data for land cover and vegetation analysis
  #Website: https://browser.dataspace.copernicus.eu/
  #Geometry: POLYGON((9.74453 52.39625,9.74453 52.35133,9.81817 52.35133,9.81817 52.39625,9.74453 52.39625))
  #Download options: Labels off, Captions off, image resolution HIGH (820x819px), TIFF (32-bit float)
  #Date in 2019 without clouds to have a clear image (date chosen 2019-06-29)
  #Layers: Visualised: True colour
  #Layers Raw: B04 and B08
  #Coordinate system: WGS 84 (EPSG:4326)
  #Resolution: lat.: 0.0000548 deg/px (0.2sec/px); long.: 0.0000898 deg/px (0.3sec/px)
  #Pixel size in meters is around 6 x 6 m

##################################################
### Starting the spatial ecology analysis in R ###
##################################################

#The working directory was set to the folder containing all downloaded data for the analysis
setwd("~/Desktop/Spatial_R")

#Installing the packages needed for the analysis 
install.packages("viridis")
install.packages("terra")    
install.packages("spatstat")
install.packages("ggridges")
install.packages("ggplot2")

#Adding the previously installed packages to the library so they can be used 
library(viridis) # Colour palette easier to read by those with colourblindness
library(terra) #Needed for rastering (rast) as well as classifying (classify) and respampling
library(spatstat)# Spatial windows and point patterns (ppp, owin), Density maps
library(ggridges) #Ridge density plots 
library(ggplot2) #for making high-quality plots with the possibility of adding multiple features


#####################################################
### Loading the data set of the bird observations ###
#####################################################

occ <- read.table("occurrence.txt", header=TRUE, sep="\t", fill = TRUE)
#Using \t to have tabs as a separation between values 
#fill = TRUE adds NA  into empty fields to make the table even 

#Check if there is a column for latitude and longitude --> result TRUE
"decimalLatitude" %in% names(occ)
"decimalLongitude" %in% names(occ)

#To have a look at the names of my columns 
names(occ) 

#Making the coordinates numeric to remove any signs (in this case, the quotation marks) 
occ$decimalLatitude  <- as.numeric(occ$decimalLatitude)
occ$decimalLongitude <- as.numeric(occ$decimalLongitude)

#To check if all values are numeric now 
occ[[98]] #Latitude 
occ[[99]] #Longitude

#Removing the empty values in either or both columns
occ.clean <- occ[!is.na(occ[[98]]) & !is.na(occ[[99]]), ]
#is.na() detects missing values and the expression mark negates --> keeps non-NA
#Only for the column of the coordinates --> so now the data has only the coordinates together with the occurrences

#Setting the limit of my study area to only have the values in the wanted range
occ.clean2 <- occ.clean[occ.clean[[98]] >= 52.35133 & occ.clean[[98]] <= 52.39625 &
                        occ.clean[[99]] >= 9.744528 & occ.clean[[99]] <= 9.818168, ]

#Creating a data frame with only the spatial information and the occurrences 
occ.new <- data.frame(lat = occ.clean2[[98]], long = occ.clean2[[99]])


#First visualization of the data to detect obvious errors 
plot(occ.new$long, occ.new$lat, main = "Map of Bird Observation Points", xlab = "Longitude (DD)", ylab = "Latitude (DD)")

############################
### Density map analysis ###
############################

#Visualisation of bird observation density in the study area
  #Analysis of the density map to identify areas with higher concentrations of occurrences

#Define observation spatial window for planar point pattern (ppp)
win <- owin(xrange = c(9.744528, 9.818168),   # Longitude
            yrange = c(52.35133, 52.39625))   # Latitude

#Create ppp object = 2D space (x, y), points and pattern (spatial structure)
#Combines coordinates and study area 
occ_points <- ppp(x = occ.new$long, y = occ.new$lat, window = win)
#After running this line: warning of duplicates 
#This is normal for this kind of ecological data 

#Smoothing point location to develop a density map
dmap_bird <- density(occ_points)


#Defining the colour palette for graphs using Viridis, which is easily readable for colorblind individuals
#The number 100 specifies that the palette will contain 100 distinct colour levels
cl <- viridis(100)

#To create a panel with two plots in one row and defining the bottom, left, top and right margins
#par(mfrow= c(2,1), mar = c(1, 2, 1, 2))

#Plotting the density together with the points 
plot(dmap_bird, main = "Density-Map of Bird Observations", col = cl, ribargs = list(las = 1,cex.axis = 0.8))
plot(occ_points, add = TRUE, pch = 20, cex = 0.4, col = "white" )

#Normalized density map (0= no-occurrence and 1= occurrence) -->scale is more intuitive 
dm_norm <- eval.im(dmap_bird / max(dmap_bird))
plot(dm_norm,main = "Normalized Density-Map of Bird Observations",col = cl, ribargs = list(las = 1,cex.axis = 0.8))
plot(occ_points, add = TRUE, pch = 20, cex = 0.4, col = "white") 

#To stop the formation of a panel in the next plots 
#dev.off()

#Visualising bird observation density: first as raw density, then normalised (0–1) for easier interpretation
#The normalised scale is more intuitive to understand and read

###############################################
### Loading the satellite images as rasters ###
###############################################

#First, loading the downloaded bands as rasters and giving them a name 
rastB8 <- rast("Eilenriede_B08.tiff") # This is the NIR band
rastB4 <- rast("Eilenriede_B04.tiff") # This is the red band

#To have a look at the information regarding the two rasters to check if everything worked smoothly 
rastB4
rastB8

#######################################
### Calculation of the DVI and NDVI ###
#######################################

#Calculation of the Difference Vegetation Index (DVI)
dvi <- rastB8 - rastB4 #DVI = NIR band - Red band 

#Calculation of the Normalised Difference Vegetation Index (NDVI) --> ranging from -1 to 1 
ndvi <- dvi / (rastB8 + rastB4) #NDVI = DVI / (NIR band + red band)

#Plotting of the NDVI values of my study area 
plot(ndvi, main = "Map of the NDVI-Value", col =cl, 
     plg = list(title = "NDVI-value", cex = 1),
     xlab = " Longitude (DD)", ylab = "Latitude (DD)")

#Interpretation of the calculation:
  #Values ranging from -1 to 0.5 are classified as water, soil, or most buildings  
  #Values ranging from 0.5 to 1.0 moderate or up to dense vegetation 

##########################################
### Clustering of the land-cover types ###
##########################################

#Using two clusters to seperated between natural and urban areas
ndvi_class <- matrix(c(-1.0, 0.5, 1,              #Cluster 1
                        0.5, 1.0, 2),             #Cluster 2
                        ncol = 3, byrow = TRUE)   #three columns and fill row by row

#The result is a matrix with two columns for the range and one for the associated class 

#Classifying the NDVI map into those 2 clusters = applying the classification to every pixel of the NDVI raster
raster_class <- classify(ndvi, ndvi_class)

# Plotting the clustered raster 

plot(raster_class, main = "Clustered NDVI-Map of Urban (1) and Natural (2) Areas", col = cl,
     xlab = " Longitude (DD)", ylab = "Latitude (DD)")



###########################
###Statistical analysis ###
###########################

#Using the density map to analyse the distribution of observations 
#rast() is converting it into a terra raster 
raster_birds <- rast(dmap_bird)

#Same data structure of bird density and NDVI-based land cover 
#Bird density is continuous; Bilinear interpolation computes a weighted average of nearby pixels
raster_birds_new <- resample(raster_birds,raster_class,method = "bilinear")

#Extraction of all raster cell values of the Bird density
birds <- values(raster_birds_new)
#Converts it into a vector 
birds <- as.vector(birds)

#Extraction of all raster cell values of the landcover classes 
landcover <- values(raster_class)
landcover <- as.vector(landcover)

#Data frame (pixel-wise) with estimated bird density in a cell and the landcover type 
df_BU <- data.frame(bird_density = birds,landcover = landcover)

#Subset bird density values by the landcover category (urban pixel and natural pixels)
#Two independent samples that can be compared 
urban_density   <- df_BU$bird_density[df_BU$landcover == 1]
natural_density <- df_BU$bird_density[df_BU$landcover == 2]

#First step: testing for normality 
  #BUT with large samples, even tiny deviations are still statistically significant 
  #For example Shapiro-Wilk test will almost always reject normality because the test is too sensitive 


#Choosing the Wilcoxon test because it doesn't assume normality and works for large samples 
  #Standard in ecology for NDVI-type of data 

#H0:The distribution of bird observation density is the same in urban and natural areas. 
#H1:Bird observation density differs between urban and natural land-cover areas.

#Wilcoxon rank sum test with continuity correction
wilcox.test(urban_density, natural_density)

#Result: Wilcoxon rank sum test with continuity correction
  #data:  urban_density and natural_density
  #W = 5.1769e+10, 
  #p-value < 2.2e-16
  #alternative hypothesis: true location shift is not equal to 0

#Interpretation: 
  #W value (rank of the sums): large W usually with large samples, and if ranks are strongly separated 
  #p-value is extremely significant so H0 can be rejected  and H1 is considered 
  #Bird observation density differs significantly between urban and natural land-cover classes
  #Land cover is strongly associated with bird density patterns.



################################################################
### Association of bird observation and urban/ natural areas ###
################################################################

#Convert the cleaned data frame with observations and coordinates (occ.new) into a vector
#Use long and lat as the coordinates, and the reference system is WGS84
occ_vect <- vect(occ.new, geom = c("long", "lat"), crs = "EPSG:4326")


#Extract NDVI values at bird locations, which allows spatial overlay points and a  raster layer
#Extract raster cell value of ndvi of each occurrence (occ_vect) --> points-based extraction 
occ_ndvi<- extract(ndvi, occ_vect)

#Combination of original occurrence data with extracted NDVI values
occ_env <- cbind(occ.new, ndvi = occ_ndvi [,2]) #data is placed into rows and two columns (NDVI and occurrence)

#Label classes --> new categorical variable of land-cover, either threshold of 0.5
occ_env$landcover <- ifelse(occ_env$ndvi < 0.5, "Urban", "Natural")


#Plot of the vector with the categorical values of land-cover
barplot(table(occ_env$landcover),           #Counts how often category occurs 
        col = c("darkgreen", "grey40"),
        main = "Number of Bird Observations divied by land-cover",
        ylab = "Number of observations", ylim = c(0, 600))

#Result: Bird observations are more frequent in natural areas
  #with ~350 observations in urban areas and ~550 in forested/natural areas


#Plot of the classified NDVI raster
plot(raster_class,
    col = cl,
    legend = FALSE,
    main = "NDVI-Map of Urban and Natural Areas",
    xlab = "Longitude (DD)",
    ylab = "Latitude (DD)")

#Adding the bird observation points to the graph of the land-cover cluster 
points(occ_env$long,occ_env$lat,pch = 20,
       col = ifelse(occ_env$landcover == "Urban", "white", "red"),cex = 0.8) #Urban white if not than red (so natural)

#Adding a legend to the plot 
legend("topright", legend = c("Urban area", "Natural area", "Urban birds", "Forest birds"),
        pt.bg = c("purple4", "yellow", NA, NA),              #Background colour (only needed for squares)
        pch = c(22, 22, 1, 20),
        col = c("black", "black", "black", "red"),
        pt.cex = c(1.5, 1.5, 0.8, 0.8),                      #Smaller symbols
        cex = 0.8,                                           #Smaller text
        y.intersp = 0.7,                                     #Tighter vertical spacing
        x.intersp = 0.5,                                     #Tighter horizontal spacing
        bg = "white",                                        #White background
        box.col = "grey50")                                  #Subtle border


#Next step: to analyse what kind of land-cover is associated with more or fewer bird observations?

######################
### Ridge analysis ### 
######################

ggplot(occ_env,aes(x = ndvi,y = landcover,fill = landcover)
) + geom_density_ridges(
    alpha = 0.7,                                            #Set transparency
    scale = 1,                                              #Vertical scaling ridge 
    rel_min_height = 0.01,                                  #Removes density values below 1% of max
    color = "white",
    linewidth = 0.3
) + coord_cartesian(xlim = c(-0.5, 1)) +                    #Limits x-axis range 
    scale_fill_manual(values = c("darkgreen", "grey60")
) + theme_minimal(base_size = 12) +
    labs(title = "Bird Observation across NDVI-Values",
    x = "NDVI-Value",
    y = "Type of land-cover",
    fill = "Land-cover"
)+ theme(legend.position = "none")


#Interpretation:
  #The NDVI distribution shows multiple peaks, indicating the presence of different habitat types within each land-cover class
  #For urban areas:
      #Most bird observations occur at low NDVI values (0 to 0.3), corresponding to built-up areas, bare soil, or sparse vegetation
      #A smaller peak at negative NDVI values likely represents water or highly impervious surfaces
      #This suggests that birds in urban areas are associated with built structures, open grounds, urban parks, and possibly water bodies
      #Indicating that observations are influenced by both water features and human-modified green spaces
  #In natural areas:
      #Bird observations are concentrated at high NDVI values (0.6 to 0.9), reflecting dense and healthy vegetation
      #Two distinct peaks may represent different forest types or variation in vegetation density (e.g., forest core vs. edges/clearings)
      #Overall, birds in natural areas are strongly associated with dense vegetation
  #There is very little overlap between the two distributions, indicating that bird occurrences are not random but strongly influenced by land cover

#Further analysis could explore why certain areas have higher observations
#Potential drivers include dense vegetation creating forest hot-spots and urban features like parks or water bodies, attracting birds in city areas


################################################################
### Density analysis together with the land-cover background ### 
################################################################

#Subset the point pattern by land cover class 
urban_ppp  <- occ_points[occ_env$landcover == "Urban"]
forest_ppp <- occ_points[occ_env$landcover == "Natural"]


#Kernel density surface for each subset
dens_urban  <- density(urban_ppp)
dens_forest <- density(forest_ppp)

#To create a panel with two plots in one row and defining the bottom, left, top and right margins
#par(mfrow= c(2,1), mar = c(1, 2, 1, 2))

#Simple density maps, one for urban and one for forest observations 
plot(density(urban_ppp), main="Density map of observations in urban areas",ribargs = list(las = 1,cex.axis = 0.8))
plot(density(forest_ppp), main="Density map of observations in natural areas", ribargs = list(las = 1,cex.axis = 0.8))

#To stop the formation of a panel in the next plots 
#dev.off()

#First plotting of the raster class to later integrate the kernel density 
plot(raster_class,col = c("grey75", "darkgreen"),legend = FALSE,main = "Density of Bird observation within the urban and natrual space",
     xlab = " Longitude (DD)", ylab = "Latitude (DD)")

#Adding the density contours to the plot of NDVI classification

#Urban – core hot-spot only (the top 10% of the density value (v=value))
#Add= TRUE so the raster below is not removed 
contour(dens_urban,add = TRUE,col = "red",lwd = 2,levels = quantile(dens_urban$v, probs = 0.9))

# Forest – core hot-spot only (the top 10% of the density value (v=value))
contour(dens_forest, add = TRUE, col = "blue", lwd = 2, levels = quantile(dens_forest$v, probs = 0.9))

#Adding the bird observation points to the map
points(occ_points, pch = 20, cex = 0.3, col = "black")

#Adding a legend to the plot, including all added features 
legend("topright", 
       legend = c("Urban area",
                  "Natural area",
                  "Bird obsv.",
                  "Urban density",
                  "Natural density"),
        pch = c(22, 22, 20, NA, NA),
        pt.bg = c("grey75", "darkgreen", NA, NA, NA),
        pt.cex = c(1.5, 1.5, 0.8, NA, NA),                  #Smaller symbols
        lty = c(NA, NA, NA, 1, 1),                          #Smaller text
        lwd = c(NA, NA, NA, 2, 2),
        seg.len = 0.8,
        col = c("black", "black", "black", "red", "blue"),
        cex = 0.8,
        y.intersp = 0.7,                                    #Tighter vertical spacing
        x.intersp = 0.5,                                    #Tighter horizontal spacing
        bg = "white",                                       #White background
        box.col = "grey50")                                 #Subtle border


#Interpretation:
  #Several hot-spots of bird observations are visible across different habitats, consistent with the ridge analysis results
  #Urban hot-spots are found in densely built-up areas as well as in open spaces, such as parks
  #Forest hot-spots occur within the forest interior, but also in urban areas with abundant green patches
  # Recognisable hot-spot where urban and forest analysis overlaps 
#Further analysis is needed to understand the factors driving high bird activity in this mixed landscape


#######################################################################################
### Analysis of the areas with the highest observation rate (both natural and urban ###
#######################################################################################

#First step: visual analysis of the area loading the true colour satellite image  

#Loading the true colour satellite image as a raster
truecolor <- rast("Eilenriede_True.tiff")

#Checking the raster --> has nlyr =3 which means three band raster 
#1 = Red, 2 = Green, 3 = blue 
truecolor

summary(truecolor) 
#The maximum is almost 1 --> setting the scale to 1 for the visualisation 

#Changing the outer margin to create space for the title
#par(oma = c(0, 0, 2, 0))    

#Plot using Red, Green, Blue bands
plotRGB(truecolor,r = 1, g = 2, b = 3,
        scale = 1, stretch = "lin")        #To make differences more visible 

#Adding the points to the graph to distinguish the exact areas of the clustered observation 
plot(occ_points, add = TRUE, pch = 20, cex = 0.5, col = "white") 

#Adding the legend
legend( "topright", inset = c(0.03, 0),
        legend = "Bird Observations",
        pch = 1,          
        col = "black",
        pt.cex = 0.5,
        bg = "white",
        cex = 0.5,
        y.intersp = 1,
        x.intersp = 0.5)

#Adding a title on top of the plot because in plotRGB, the function main is not run 
title("True-color satellite image of Eilenriede", outer = TRUE, cex = 1)


#To stop the formation of a panel in the next plots 
#dev.off()

#Area with many observations appears to be a lake 
#Next step: verify this observation using NDVI analysis


#NDVI raster is already named as 'ndvi'
#Create a water raster so the original NDVI data set remains unchanged 
water_raster <- ndvi

#Choosing the right NDVI value for small lakes, as well as the pixel resolution, can become a problem in this method: 
  #Usually, the NDVI value of water bodies is in the range of 0 to -0.5
  #In small lakes, the outer vegetation can become a problem (Can change the NDVI value and make it no longer distinguishable as a water body)
  #Also, classification error due to highly impervious surfaces that are wrongly identified as water bodies 


#Threshold-based on classification: everything slightly negative or more is classified as water 
water_raster[water_raster > -0.009] <- NA   #Non-water (now NA)
water_raster[water_raster <= -0.009] <- 1   #Water

#Creating a base raster for the background to highlight the distribution of water 
background_raster <- ndvi
background_raster[] <- 1  # all cells = 1 to have a clear background 
background_raster[water_raster == 1] <- NA  #removes the cells that are classified water 


#Plotting grey background first
plot(background_raster, col = "grey80", legend = FALSE, main = "NDVI based analyis of water bodies",
     xlab = " Longitude (DD)", ylab = "Latitude (DD)")

#Adding the water raster in blue as an overlay (add = TRUE)
plot(water_raster, col = "blue", add = TRUE, legend = FALSE)

#Adding a legend 
legend("topright", legend = "Water", fill = "blue", cex = 0.7,  
       bg = "white", box.col = "grey50")


#Same plot but with the addition of the points of occurrence
plot(background_raster, col = "grey80", legend = FALSE, main = "Analysis of bird observations and water bodies (NDVI based)",
     xlab = " Longitude (DD)", ylab = "Latitude (DD)")

# Add water in blue as an overlay (add = TRUE)
plot(water_raster, col = "blue", add = TRUE, legend = FALSE)

#Adding the points can be useful to check if the points of observation are concentrated at water bodies 
plot(occ_points, add = TRUE, pch = 20, cex = 0.4, col = "black")

#Adding the legend to the plot, integrating all features shown in the graph
legend("topright", 
       legend = c("Water bodies", "Bird obsv."),
       pch = c(22, 20),
       pt.bg = c("blue", NA),
       pt.cex = c(1.5, 0.8),             #Smaller symbols
       col = c("black", "black"),
       cex = 0.8,
       y.intersp = 0.7,                  #Tighter vertical spacing
       x.intersp = 0.5,                  #Tighter horizontal spacing
       bg = "white",                     #White background
       box.col = "grey50")               #Subtle border


#Interpretation:
  #This NDVI analysis of water remains partly inconclusive due to the small spatial extent of the water body relative to the raster resolution
  #However, the RGB true-colour imagery clearly shows that the area with a high concentration of bird observations contains a lake
  #surrounded by small park and green spaces
  #Several areas are also incorrectly classified as water, while some existing water bodies are not clearly detected
  #The combination of open water, vegetation, and recreational park infrastructure likely provides favourable habitat conditions for birds,
  #while simultaneously attracting high numbers of human visitors
  #This overlap of suitable bird habitat and human activity offers a plausible explanation for the unusually high number of observations
  #recorded in this location


##################
### Conclusion ###
##################

#The initial assumption that bird observations would be more frequent in highly urbanised areas was not supported by the analysis
#Instead, a higher number of observations was recorded in vegetated areas
#This pattern is likely due to the presence of trees, shrubs, and other vegetation between buildings, providing natural habitat within the city frame 
#Even within urban areas, these vegetated patches attract birds and are also accessible to humans, 
#increasing the likelihood of observations being recorded in natural areas 
#NDVI-based analysis confirmed that bird observations are strongly associated with areas of higher vegetation density, 
#even when these areas are interspersed within urban structures
#This indicates that bird density is influenced more by habitat quality (vegetation cover) than by urbanisation alone
#Overall, the spatial distribution of bird observations in Eilenriede City Park demonstrates that natural features within urban environments 
#play a critical role in supporting wildlife and influencing observation patterns 
#These findings directly address the research question, showing that bird densities differ significantly between urban and natural land-cover types 
#and that vegetation cover (NDVI) is a key explanatory factor

#Possible bias of the data/analysis: 
  #Location accuracy bias:
      #Some observations may use exact GPS coordinates, while others may only use a general location or proxy
      #This can lead to spatial uncertainty in associating birds with urban vs. natural areas
  #Observer effort bias:
      #Areas with higher human activity (paths, urban areas, parks) are more likely to have birds observed and reported.
      #This does not necessarily reflect true bird abundance, only where people are present to record them.
  #Spatial resolution bias:
      #The resolution of satellite data (NDVI) might be too coarse, with pixel size larger than fine-scale habitat features
      #Small parks, water bodies, or vegetation patches might be misrepresented, affecting the classification of urban vs. natural areas
