#Final project Spatial in R
# Obersvations of bird in the city park Eilenriede 
#to analyse the spatial distrubution of birds between urban and nature areas

#Study area around 50/50
# Longitude (°E) | Latitude (°N)
#--------------------------------
# 9.744528       | 52.39625
# 9.818168       | 52.39625
# 9.818168       | 52.35133
# 9.744528       | 52.35133
# 9.744528       | 52.39625

#Area of 5km x 5km = 25km^2

#The analysis is abput GBIF date in the year 2019
#https://www.gbif.org/occurrence/download?basis_of_record=HUMAN_OBSERVATION&has_coordinate=true&has_geospatial_issue=false&taxon_key=212&year=2016,2016&geometry=POLYGON((9.74453%2052.39625,9.74453%2052.35133,9.81817%2052.35133,9.81817%2052.39625,9.74453%2052.39625))

#About the data:

#Data about brd oservatiosn in the study area 
#Human observations 
#Geometry: POLYGON((9.74453 52.39625,9.74453 52.35133,9.81817 52.35133,9.81817 52.39625,9.74453 52.39625))
#Has coordinates: true, has geopspatial issues: false 
#Species analyzed (Scientific name): Aves (898 occurences)
#Date between the start of 2016 and the end of 2019
#GBIF.org (21 January 2026) GBIF Occurrence Download
#https://doi.org/10.15468/dl.8zukfu

#Satellite image dataset sources from Copericus:
#website: https://browser.dataspace.copernicus.eu/
#Location: same Polygon as chosen for the GBIF.org data 
#Download options: Labels off, Captions off, image resolution HIGH (820x819px), TIFF (32-bit float)
#Date in 2019 without clouds to have a clear image (date chosen 2019-06-29)
# Layers: Vizualized: True color
# Layers Raw: B04 and B08
# Coordinate system: WGS 84 (EPSG:4326)
#Resolution: lat.: 0.0000548 deg/px (0.2sec/px); long.: 0.0000898 deg/px (0.3sec/px)

setwd("~/Desktop/Spatial_R")

#packages
install.packages("viridis")
install.packages("terra")    
install.packages("spatstat")
install.packages("ggridges")
install.packages("ggplot2")


library(viridis) #Color palette to develop graphs for colorblind people 
library(terra) #Needed for rastering (rast) as well as classfying (classify) and respampling
library(spatstat)# Spatial windows and point patterns (ppp, owin), Density maps, nearest-neighbor analysis
library(ggridges) #Ridge density plots 
library(ggplot2) #for plotting (very detailed and many functions)


###Loading the data set of the bird observations

occ <- read.table("occurrence.txt", header=TRUE, sep="\t", fill = TRUE)
# "\t" for tabs as separators
# fill = TRUE adds blank fields if rows aren't equal (NA)

#check if coordinates are integrated into my data --> result TRUE
"decimalLatitude" %in% names(occ)
"decimalLongitude" %in% names(occ)

names(occ) #have a look at the names of my columns 

#make the coordinates numeric to remove and kind of signs (in this case the quotation marks) 
occ$decimalLatitude  <- as.numeric(occ$decimalLatitude)
occ$decimalLongitude <- as.numeric(occ$decimalLongitude)

#to check if it worked 
occ[[98]] #Latitude 
occ[[99]] #Longitude

#removing the empty values in either or both 
occ.clean <- occ[!is.na(occ[[98]]) & !is.na(occ[[99]]), ]
#is.na() detects missing values and the expression mark negates --> keeps non-NA
#only for the rows with the cooridnates --> so now only data that have both cooridnates


#setting the range of my study area so only the values in the wanted range are left
occ.clean2 <- occ.clean[occ.clean[[98]] >= 52.35133 & occ.clean[[98]] <= 52.39625 &
                        occ.clean[[99]] >= 9.744528 & occ.clean[[99]] <= 9.818168, ]

#only the spatial information together with the occurences 
occ.new <- data.frame(lat = occ.clean2[[98]], long = occ.clean2[[99]])

head(occ.new)

# first visualisation of the data to detect obvious errors 
plot(occ.new$long, occ.new$lat, main = "Bird Observation Map", xlab = "Longitude (DD)", ylab = "Latitude (DD)")

#################################
###Start analysis of the data ###
#################################


###Density map###
#################

#visualization of the cluster --> statistical interpretation
#Analysis of a density map to see if there is an area with more occurences compared to others 
#defining the range for the density map

#Define observation spatial window for planar point pattern (ppp)
win <- owin(
  xrange = c(9.744528, 9.818168),   # Longitude
  yrange = c(52.35133, 52.39625))   # Latitude


#Create ppp object = 2D space (x, y), points and pattern (spatial structure)
#combines cooridnates and study area 
occ_points <- ppp(x = occ.new$long, y = occ.new$lat, window = win)
#warning of duplicates abut for occurrence data normal 


#Smoothes point location to develop density map
dmap_bird <- density(occ_points)

cl <- viridis(100)

#plotting the density together with the points 
plot(dmap_bird, main = "Density Map of Observations", col = cl, ribargs = list(las = 1,cex.axis = 0.8))
plot(occ_points, add = TRUE, pch = 20, cex = 0.4, col = "white" )

#Normalized density map (0=no occurence and 1=occurence) -->this better it is more intuitive 
dm_norm <- eval.im(dmap_bird / max(dmap_bird))
plot(dm_norm,main = "Normalized Density (0–1)",col = cl, ribargs = list(las = 1,cex.axis = 0.8))
plot(occ_points, add = TRUE, pch = 20, cex = 0.4, col = "white") 


#############################################
###Loading the satellite images as rasters###
#############################################

rastB8 <- rast("Eilenriede_B08.tiff") # This is the NIR band
rastB4 <- rast("Eilenriede_B04.tiff") # This is the red band

rastB4
rastB8

###calculation of the Difference Vegetation Index (DVI)
dvi <- rastB8 - rastB4 #DVI = NIR band - Red band 

#Calculation of the Normalized difference vegetation index (NDVI)
ndvi <- dvi / (rastB8 + rastB4) #NDVI = DVI / (NIR band + red band)

plot(ndvi, main = "NDVI Map of the Eilenriede in Hannover", col =cl, 
     plg = list(title = "NDVI value", cex = 1),
     xlab = " Longitude (DD)", ylab = "Latitude (DD)")

#Interpretation of the calculation:
#Values ranging from -1 to 0.5 are classified as water, soil, most buildings  
#Values ranging from 0.5 to 1.0 moderate or up to dense vegetation 
#The analysis of presence of vegetation in the study area
#Using two clusters
ndvi_class <- matrix(c(-1.0, 0.5, 1, #Cluster 1
                        0.5, 1.0, 2), #Cluster 2
                        ncol = 3, byrow = TRUE) #three columns and fill row by row
#two for the range and one for the class 

# Classifying the NDVI map into those 2 clusters = appliying the classification to every pixel of the NDVI raster
raster_class <- classify(ndvi, ndvi_class)

# Removing -1 values as they will appear as a third cluster
# and assigning them to NA 
raster_class[raster_class == -1] <- NA


# Plotting the clustered raster
###############################

plot(raster_class, main = "NDVI map of Urban (1) and Natural (2) Areas", col = cl,
     xlab = " Longitude (DD)", ylab = "Latitude (DD)")



##########################
###Statistcial analysis###
##########################

# Convert the cleaned data frame with observations and coordinates (occ.new)
#use long and lat as the coordinates and the reference system is WGS84
occ_vect <- vect(occ.new, geom = c("long", "lat"), crs = "EPSG:4326")


# Extract NDVI values at bird locations allows spatial overlay points and raster layer
#Extract raster cell value of ndvi of each occurrence (occ_vect)
occ_ndvi<- extract(ndvi, occ_vect)

#combination original occurrence data with extracted ndvi values
occ_env <- cbind(occ.new, ndvi = occ_ndvi [,2]) #all rows but only two columns

# Label classes --> new categorical variable of landcover eith thresholf of 0.5
occ_env$landcover <- ifelse(occ_env$ndvi < 0.5, "Urban", "Natural")

# Remove rows with missing NDVI to continue with statistcal analysis 
occ_env_stat <- na.omit(occ_env)

names(occ_env_stat)
#Usually first step: testing for normality 
#BUT with large samples even tiny deviations are still statistically significant 
#so for example Shapiro-Wilk test will almost always reject normality because the test is too sensitive 

# Visual normality check (large sample)

par(mfrow = c(1, 2))

# Histogram: frequnecy distribution NDVI (check symmetry, skewness and spread)
hist(occ_env_stat$ndvi,breaks = 30,col = "grey80",
  main = "NDVI distribution at bird locations",
  xlab = "NDVI")

# Q–Q plot: NDVI value over quantiles (to check normal distribution)
#devuation line = non-normality
qqnorm(occ_env_stat$ndvi,main = "Q–Q plot of NDVI")
qqline(occ_env_stat$ndvi, col = "red")

dev.off()

#Histogram: shows not symmetric behavior 
#Normal for NDVI data because certain values associated with charscteristcs e.g forest 
#Reflects ecologcial structures 
#Q-Q Plot: s-shaped curve --> again: Plot shows data is not normally distributed 
#Visual inspection of histograms and Q–Q plots indicated a non-normal NDVI distribution
#Therefore, differences between urban and forest bird observation locations were assessed using a non-parametric Wilcoxon rank-sum test.

#Choosing Wilcoxon test because: doesn't assume normality, works for large samples 
#robust to skewed distributions 
#Standard in ecology for NDVI type data 

# Main statistical test (robust, large n)
#Wilcoxon rank sum test with continuity correction

#compares ndvi distribution between two landcover groups 
wilcox.test(ndvi ~ landcover,data = occ_env_stat)


#Result: 
#data:  ndvi_cont by landcover
#W = 0, p-value < 2.2e-16
#alternative hypothesis: true location shift is not equal to 0
#Interpretation: extremely strong differences between groups
#do not overlap much 

###so further analysis of the distribution between the two group (vegetated and non-vegetated)



####################################################################
###Plotting the association observation with urban vs. vegetation###
####################################################################

#Plot of the vector with the categorical values of landcover
barplot(
  table(occ_env$landcover),                              #counts how often category occurs 
  col = c("darkgreen", "grey40"),
  main = "Number of Bird Observations by Land Cover",
  ylab = "Number of observations", ylim = c(0, 600))


# Plot classifed NDVI raster
plot(raster_class,
    col = cl,
    legend = FALSE,
    main = "NDVI map of Urban and Natural Areas",
    xlab = "Longitude (DD)",
    ylab = "Latitude (DD)")

# Plot bird points with coodinates (long/lat)
points(occ_env$long,occ_env$lat,pch = 20,
       col = ifelse(occ_env$landcover == "Urban", "white", "red"),cex = 0.8) #Urban white if not than red (so natural)

legend("topright",legend = c("Urban area","Natural area","Urban birds","Forest birds"),
        pt.bg = c("purple4", "yellow", NA, NA),  #background color (only needed for squares)
        pch = c(22, 22, 1, 20),
        col = c("black", "black", "black", "red"),
        pt.cex = c(1.5, 1.5, 0.8, 0.8), # smaller symbols
        cex = 0.8,                      # smaller text
        y.intersp = 0.7,                # tighter vertical spacing
        x.intersp = 0.5,                # tighter horizontal spacing
        bg = "white",                   # white background
        box.col = "grey50")             # subtle border


###Ridge analysis### 
####################


ggplot(occ_env,aes(x = ndvi,y = landcover,fill = landcover)
) + geom_density_ridges(
    alpha = 0.7,                              #set transparency
    scale = 1,                                #vertical scaling ridge 
    rel_min_height = 0.01,                    #Removes density values below 1% of max
    color = "white",
    linewidth = 0.3
) + coord_cartesian(xlim = c(-0.5, 1)) +       #Limits x-axis range 
    scale_fill_manual(values = c("grey60", "darkgreen")
) + theme_minimal(base_size = 12) +
    labs(title = "NDVI Distribution at Bird Observation Locations",
    x = "NDVI",
    y = "Land-cover type",
    fill = "Land-cover")

#Interpretation: 
#Multiple peaks are visible so different habitat types within the classes 
#Most observations of bird in low NDVI values (0 to 0.3) for the urban category = built-up areas, soil, sparse vegetation
#small peak in the negative = water or very impervious surfaces 
#Bird observations associated with built structures, open ground, urban parks, possibly water bodies 
#Observations influenced by water bodies and human-modified green spaces?
#In the natural class observations in high NDVI values (0.6 to 0.9) = dense/healthy vegetation
#two distinct peaks: different forest types or variation density (forest core vs. edges/clearings)
#Birds in natural areas are strongly associated with dense vegetation 
#Very little overlap between the two distributions 
#No random distributions of birds but land cover strongly influences bird occurences
#Wilcoxon p-value is visually confirmed 
#Further analysis: Why are there certain areas with more observations? What in the land cover could be the reason?
#Forest hotspots due to dense vegetation?
#Urban hotspots due to water bodies, parks and human activity?




###Kernel density with landcover bakcground### 
##############################################

#Subset the point pattern by land cover class 
urban_ppp  <- occ_points[occ_env$landcover == "Urban"]
forest_ppp <- occ_points[occ_env$landcover == "Natural"]


#Kernel density surface for each subset
dens_urban  <- density(urban_ppp)
dens_forest <- density(forest_ppp)


par(mfrow= c(1,2))


#simple density maps one for urban and one for forest observations 
plot(density(urban_ppp), main="Urban bird density",ribargs = list(las = 1,cex.axis = 0.8))

plot(density(forest_ppp), main="Forest bird density", ribargs = list(las = 1,cex.axis = 0.8))


#First plotting of the raster class to later integrate the kernel density 
plot(raster_class,col = c("grey75", "darkgreen"),legend = FALSE,main = "Urban vs Natural Bird Observation Hotspots",
     xlab = " Longitude (DD)", ylab = "Latitude (DD)")


# Urban – core hotspot only (the top 10% of the density value (v=value))
#add= TRUE so the raster below is not removed 
contour(dens_urban,add = TRUE,col = "red",lwd = 2,levels = quantile(dens_urban$v, probs = 0.9))

# Forest – core hotspot only
contour(dens_forest, add = TRUE, col = "blue", lwd = 2, levels = quantile(dens_forest$v, probs = 0.9))


# Optional: add bird observation points
points(occ_points, pch = 20, cex = 0.3, col = "black")

legend("topright", 
       legend = c("Urban area",
                  "Natural area",
                  "Bird obsv.",
                  "Urban density",
                  "Natural density"),
       pch = c(22, 22, 20, NA, NA),
       pt.bg = c("grey75", "darkgreen", NA, NA, NA),
       pt.cex = c(1.5, 1.5, 0.8, NA, NA),              # smaller symbols
       lty = c(NA, NA, NA, 1, 1),                      # smaller text
       lwd = c(NA, NA, NA, 2, 2),
       seg.len = 0.8,
       col = c("black", "black", "black", "red", "blue"),
       cex = 0.8,
       y.intersp = 0.7,      # tighter vertical spacing
       x.intersp = 0.5,      # tighter horizontal spacing
       bg = "white",         # white background
       box.col = "grey50")   # subtle border


#Interpretation: 
#Multiple hotspots in different habitats --> aligns with the results of the ridge analysis 
#Urban Hotspot in densly built up area but also open grounds with possibly park
#Forest hotspot in the forest interior but also in an urban area with many green patches 
#Further analysis of the area where many bird observations in both urban and natural areas 
#Why? Further anaylsis needed 


#Further analysis of hotspots: WHY?###
######################################

#There is a certain hotspot with many observations of birds? Analysis of what the possible reason could be:

#First load the True color image to see at possible solutions 
# Load the true color raster
truecolor <- rast("Eilenriede_True.tiff")

# Check the raster --> has nlyr =3 which means three band raster 
#1 = Red, 2= Green, 3= blue 
truecolor

summary(truecolor) #max is almost 1 so scale set to 1 as well 

# Plot using Red, Green, Blue bands (usually bands 1,2,3)
plotRGB(truecolor,r = 1, g = 2, b = 3,
        scale = 1,            # if values are 0–1
        main = "True Color Image of Eilenriede")

plot(occ_points, add = TRUE, pch = 20, cex = 0.4, col = "white") 

#Looks like a lake --> check with NDVI analysis if true 


# NDVI raster is already loaded as 'ndvi'
# Create water raster: NDVI < 0 (copy so orgignal unchanged)
water_raster <- ndvi

#choosing the right ndvi value for lakes if difficult 
#usually  in close to 0 or a big egative (0 until -0.5)
#but do to vegetation on the outer parts of the lake or already vegetaion within the lake (NDVI around 0.2 until 0.8)
#especially small water bodies re hard to identify with the ndvi

#Threshold-based on classification: everything slightly negative or more = water 
water_raster[water_raster > -0.009] <- NA   # Non-water (not plotted)
water_raster[water_raster <= -0.009] <- 1   # Water

# Create a base raster for background (everything else grey)
background_raster <- ndvi
background_raster[] <- 1  # all cells = 1 to have a clear background 
background_raster[water_raster == 1] <- NA  #removes the cells that are classified water 


#par(mfrow= c(1,2))


# Plot grey background first
plot(background_raster, col = "grey80", legend = FALSE, main = "NDVI based water Bodies in Study Area",
     xlab = " Longitude (DD)", ylab = "Latitude (DD)")
# Add water in blue as an overlay (add = TRUE)
plot(water_raster, col = "blue", add = TRUE, legend = FALSE)

legend("topright", legend = "Water", fill = "blue", cex = 0.7,  
       bg = "white", box.col = "grey50")


# same with the points of occurence
plot(background_raster, col = "grey80", legend = FALSE, main = "NDVI based water Bodies in Study Area",
     xlab = " Longitude (DD)", ylab = "Latitude (DD)")
# Add water in blue as an overlay (add = TRUE)
plot(water_raster, col = "blue", add = TRUE, legend = FALSE)

#check if the point are concentrated at water bodies 
plot(occ_points, add = TRUE, pch = 20, cex = 0.2, col = "black")

legend("topright", 
       legend = c("Water bodies", "Bird obsv."),
       pch = c(22, 20),
       pt.bg = c("blue", NA),
       pt.cex = c(1.5, 0.8),              # smaller symbols
       col = c("black", "black"),
       cex = 0.8,
       y.intersp = 0.7,      # tighter vertical spacing
       x.intersp = 0.5,      # tighter horizontal spacing
       bg = "white",         # white background
       box.col = "grey50")   # subtle border



#This visual analysis is a bit inconclusive due to the small size of the water body
#But und the RGB visula it is clear that in the area with many occurences a lake with a small park area is visible 
#also some spots that are wrongly idenfifed as water bodies 
#Possibly a park with many humans and a water body so optimal for birds but also a place of leisure of humans
#makes the high observations in this place very plausible 


################
###Conclusion###
################

#Assumption from the beginning was not found during the anaylsis 
#More observations in vegated than in urban areas 
#might be due to a lot aof vegetation between building 
#there are the animals naturally more found due to trees and other vegeation which is the preferred habitat 
#But it is still in urban areas so many humans present to do the obervations
#due to NDVI analysis are these obersavtiosn still assoiciated with the vegetated areas wventhough between buildings
#this might be the reason for more obersavtions in vegated areas and not urban because so interconnected 

