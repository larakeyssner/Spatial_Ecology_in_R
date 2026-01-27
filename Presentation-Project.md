# Spatial Distribution of Bird Observations in Urban and Natural Areas of Eilenriede City Park, Hanover (Germany)

Do bird observations differ in density between urban and natural land-cover types, and how is this associated with vegetation cover?

### Initial Assumption
Initially assumption is that bird observations would be more frequent in urbanised areas due to higher human presence in these areas, which increases the likelihood of birds being seen and recorded

## The Study Area

The aim was to choose a study area with around 50% natural areas and 50% urban areas

**Why the City park Eilenriede in Hanover:**
- It is one of the biggest forests in a city in Europe
- Provides a gradient from highly vegetated areas to areas with human influence
- Easily accessible to humans, leading to rich citizen science data (e.g., GBIF observations)

Area of 5km x 5km = 25km² with the park in the center 

## The Ecological Data

Bird observations from the GBIF database collected in the year 2019

**About the data:**
- Human observations
- Species analysed: Aves (898 occurrences)

## The Satellite Data

Satellite imagery for the study area was sourced from Copernicus

- Date in 2019 without clouds to have a clear image (date chosen 2019-06-29)
- Layers - Visualised: True colour
- Layers Raw: B04 and B08
- Pixel size in meters is around 6 x 6 m

## Starting the Spatial Ecology Analysis in R

```{r setup-analysis}
library(viridis)    #Colour palette easier to read by those with colourblindness
library(terra)      #Needed for rastering (rast), classifying (classify), resampling, plotRBG
library(spatstat)   #Spatial windows and point patterns (ppp, owin), Density maps
library(ggridges)   #Ridge density plots 
library(ggplot2)    #For making high-quality plots with the possibility of adding multiple features
```

## Loading the Data Set of the Bird Observations

```{r load-bird-data}
#Loading the occurrence data
occ <- read.table("occurrence.txt", header = TRUE, sep = "\t", fill = TRUE)

#Making the coordinates numeric to remove any signs (in this case, the quotation marks) 
occ$decimalLatitude  <- as.numeric(occ$decimalLatitude)
occ$decimalLongitude <- as.numeric(occ$decimalLongitude)

#Removing the empty values in either or both columns
occ.clean <- occ[!is.na(occ[[98]]) & !is.na(occ[[99]]), ]

#Setting the limit of my study area to only have the values in the wanted range
occ.clean2 <- occ.clean[occ.clean[[98]] >= 52.35133 & occ.clean[[98]] <= 52.39625 &
                        occ.clean[[99]] >= 9.744528 & occ.clean[[99]] <= 9.818168, ]

#Creating a data frame with only the spatial information and the occurrences 
occ.new <- data.frame(lat = occ.clean2[[98]], long = occ.clean2[[99]])
```

## Density Map Analysis
Analysis of the density map to identify areas with higher concentrations of occurrences

```{r density-analysis}
#Define observation spatial window for planar point pattern (ppp)
win <- owin(xrange = c(9.744528, 9.818168),   # Longitude
            yrange = c(52.35133, 52.39625))   # Latitude

#Create ppp object = 2D space (x, y), points and pattern (spatial structure)
occ_points <- ppp(x = occ.new$long, y = occ.new$lat, window = win)

#Smoothing point location to develop a density map
dmap_bird <- density(occ_points)

#Plotting the density together with the points 
plot(dmap_bird, main = "Density-Map of Bird Observations", col = cl, ribargs = list(las = 1, cex.axis = 0.8))
plot(occ_points, add = TRUE, pch = 20, cex = 0.4, col = "white")

#Normalized density map (scale is more intuitive)
dm_norm <- eval.im(dmap_bird / max(dmap_bird))
plot(dm_norm, main = "Normalized Density-Map of Bird Observations", col = cl, ribargs = list(las = 1, cex.axis = 0.8))
plot(occ_points, add = TRUE, pch = 20, cex = 0.4, col = "white") 
```
<p align="center">
<img width="514" height="513" src = https://github.com/user-attachments/assets/43a569aa-5b56-485d-94a4-23f0a7c9322e>


**Interpretation:** 
- Visualising bird observation density
- The normalised scale is more intuitive to understand and read
- The density maps show clear hotspots of bird observations, particularly in certain areas

## Loading the Satellite Images as Rasters

```{r load-rasters}
rastB8 <- rast("Eilenriede_B08.tiff") # This is the NIR band
rastB4 <- rast("Eilenriede_B04.tiff") # This is the red band
```

## Calculation of the DVI and NDVI

```{r calculate-indices}
#Calculation of the Difference Vegetation Index (DVI)
dvi <- rastB8 - rastB4 # DVI = NIR band - Red band 

#Calculation of the Normalised Difference Vegetation Index (NDVI) --> ranging from -1 to 1 
ndvi <- dvi / (rastB8 + rastB4) # NDVI = DVI / (NIR band + red band)

#Plotting of the NDVI values of my study area 
plot(ndvi, main = "Map of the NDVI-Value", col = cl, plg = list(title = "NDVI-value", cex = 1),
     xlab = "Longitude (DD)", ylab = "Latitude (DD)")
```
<p align="center">
<img width="514" height="513" src =https://github.com/user-attachments/assets/1c30ad03-cd03-4145-a1b3-e5bdebfdfbf2>

**Interpretation of the calculation:**
- Values ranging from -1 to 0.5 are classified as water, soil, or most buildings
- Values ranging from 0.5 to 1.0 moderate or up to dense vegetation

## Clustering of the Land-Cover Types

```{r classify-landcover}
#Using two clusters to separate natural and urban areas
ndvi_class <- matrix(c(-1.0, 0.5, 1,              # Cluster 1
                        0.5, 1.0, 2),             # Cluster 2
                        ncol = 3, byrow = TRUE)   # three columns and fill row by row

#Classifying the NDVI map into those 2 clusters
raster_class <- classify(ndvi, ndvi_class)

plot(raster_class, main = "Clustered NDVI-Map of Urban (1) and Natural (2) Areas", col = cl,
     xlab = "Longitude (DD)", ylab = "Latitude (DD)")
```

<p align="center">
<img width="514" height="513" src = https://github.com/user-attachments/assets/ec6ed43f-1246-497a-8011-276e294fbd24>


**Interpretation:** 
- The NDVI map has been classified into two land-cover types: Urban (class 1, NDVI ≤ 0.5) and Natural (class 2, NDVI > 0.5)
- This binary classification allows for direct comparison of bird observations between urban and natural areas.

## Statistical Analysis

**Hypothesis of the statistical analysis**
- **H0:** The distribution of bird observation density is the same in urban and natural areas
- **H1:** Bird observation density differs between urban and natural land-cover areas

```{r statistical-analysis}
#Using the density map to analyse the distribution of observations 
raster_birds <- rast(dmap_bird)

#Bird density is continuous; Bilinear interpolation computes a weighted average of nearby pixels
raster_birds_new <- resample(raster_birds, raster_class, method = "bilinear")

# Extract and convert to vectors
birds <- as.vector(values(raster_birds_new))
landcover <- as.vector(values(raster_class))

#Data frame (pixel-wise) with estimated bird density in a cell and the landcover type 
df <- data.frame(bird_density = birds, landcover = landcover)

#Removing the rows with NA
df_clean <- na.omit(df)

#Subset bird density values by the landcover category (urban pixel and natural pixels)
#Two independent samples that can be compared 
urban_density <- df_clean$bird_density[df_clean$landcover == 1]
natural_density <- df_clean$bird_density[df_clean$landcover == 2]

#Choosing the Wilcoxon test because it doesn't assume normality and works for large samples 
#Wilcoxon rank sum test with continuity correction
wilcox.test(urban_density, natural_density)
```

**Interpretation of Wilcoxon test results:**
- W value (rank of the sums) is 5.18e+10: large W usually with large samples, and if ranks are strongly separated
- p-value is significant (p < 2.2e-16), so H0 can be rejected and H1 is considered
- Bird observation density differs significantly between urban and natural land-cover classes
- Land cover is strongly associated with bird density patterns

## Association of Bird Observation and Urban/Natural Areas

```{r association-analysis}
#Convert the cleaned data frame with observations and coordinates (occ.new) into a vector
occ_vect <- vect(occ.new, geom = c("long", "lat"), crs = "EPSG:4326")

#Extract raster cell value of NDVI of each occurrence (occ_vect) --> points-based extraction 
occ_ndvi <- extract(ndvi, occ_vect)

#Combination of original occurrence data with extracted NDVI values
occ_env <- cbind(occ.new, ndvi = occ_ndvi[, 2]) 

#Label classes --> new categorical variable of land-cover, either threshold of 0.5
occ_env$landcover <- ifelse(occ_env$ndvi < 0.5, "Urban", "Natural")

#Plot of the vector with the categorical values of land-cover
barplot(table(occ_env$landcover),col = c("darkgreen", "grey40"), main = "Number of Bird Observations divided by land-cover",
        ylab = "Number of observations", ylim = c(0, 600))
```
<p align="center">
<img width="514" height="513" src = https://github.com/user-attachments/assets/e5da4367-e74d-46ce-9101-62db6b7b3c98>


**Interpretation:** Bird observations are more frequent in natural areas with approximately 350 observations in urban areas and 550 in forested/natural areas. This contradicts the initial assumption that urban areas would have more observations.

```{r ndvi-map-points}
#Plot of the classified NDVI raster
plot(raster_class, col = cl, legend = FALSE, main = "NDVI-Map of Urban and Natural Areas with Bird Observations",
    xlab = "Longitude (DD)", ylab = "Latitude (DD)")

#Adding the bird observation points to the graph of the land-cover cluster 
points(occ_env$long, occ_env$lat, pch = 20,
       col = ifelse(occ_env$landcover == "Urban", "white", "blue"), cex = 1)

#Adding a legend to the plot 
```
<p align="center">
<img width="514" height="513" src = https://github.com/user-attachments/assets/ef15e6d3-ad8c-4bc2-91fb-8be57322d8f6>


  
**Interpretation:** The overlay of bird observations on the classified NDVI map clearly shows that most observations occur in natural areas, with relatively fewer observations in urban areas.

## Ridge Analysis

```{r ridge-analysis}
#In ggplot2, the + chains together different layers/modifications to build the plot step by step
ggplot(occ_env,aes(x = ndvi,y = landcover,fill = landcover)
) + geom_density_ridges(                                    #Adding the Ridge plot geometry
    alpha = 0.7,                                            #Set transparency
    scale = 1,                                              #Vertical scaling ridge 
    color = "white",
    linewidth = 0.3
) + coord_cartesian(xlim = c(-0.5, 1)) +                    #Limits x-axis range 
    scale_fill_manual(values = c("darkgreen", "grey60")     #Choosing colours
) + theme_minimal(base_size = 12) +                         #Clean theme 
    labs(title = "Bird Observation across NDVI-Values",     #Adding all the labels
    x = "NDVI-Value",
    y = "Type of land-cover",
)+ theme(legend.position = "none")                          #Removing the legend
```
<p align="center">
<img width="514" height="513" src = https://github.com/user-attachments/assets/a1e2223f-2316-4107-889f-19ebd4d86060>


**Interpretation:**
- The NDVI distribution shows multiple peaks, indicating the presence of different habitat types within each land-cover class
- For urban areas:
  - Most bird observations occur at low NDVI values (0 to 0.3), corresponding to built-up areas, bare soil, or sparse vegetation
  - A smaller peak at negative NDVI values likely represents water or highly impervious surfaces
- In natural areas:
  - Bird observations are concentrated at high NDVI values (0.6 to 0.9), reflecting dense and healthy vegetation
  - Two distinct peaks may represent different forest types or variation in vegetation density (e.g., forest core vs. edges/clearings)
- There is minimal overlap between the two distributions, indicating that bird occurrences are not random but strongly influenced by land cover

## Density Analysis Together with the Land-Cover Background

```{r density-landcover}
#Subset the point pattern by land cover class 
urban_ppp  <- occ_points[occ_env$landcover == "Urban"]
forest_ppp <- occ_points[occ_env$landcover == "Natural"]

#Kernel density surface for each subset
dens_urban  <- density(urban_ppp)
dens_forest <- density(forest_ppp)

par(mfrow = c(1, 2), mar = c(4, 4, 3, 2))

#Simple density maps, one for urban and one for forest observations 
plot(density(urban_ppp), main = "Density map of observations in urban areas", ribargs = list(las = 1, cex.axis = 0.8))
plot(density(forest_ppp), main = "Density map of observations in natural areas", ribargs = list(las = 1, cex.axis = 0.8))
```
<p align="center">
<img width="514" height="513" src =https://github.com/user-attachments/assets/73ef98e6-a056-40a9-a06e-0538626c0e34>

```{r density-landcover}
#First plotting of the raster class to later integrate the kernel density 
plot(raster_class, col = c("azure4", "lightgrey"), legend = FALSE, main = "Density of Bird observation within the urban and natural space",
     xlab = "Longitude (DD)", ylab = "Latitude (DD)")

#Urban – core hot-spot only (the top 10% of the density value (v=value))
contour(dens_urban, add = TRUE, col = "red", lwd = 2, levels = quantile(dens_urban$v, probs = 0.9))
#Forest – core hot-spot only (the top 10% of the density value (v=value))
contour(dens_forest, add = TRUE, col = "blue", lwd = 2, levels = quantile(dens_forest$v, probs = 0.9))

points(occ_points, pch = 20, cex = 0.3, col = "black")

#Adding a legend to the plot, including all added features 
```

<p align="center">
<img width="514" height="513" src =https://github.com/user-attachments/assets/c226981b-ef33-4340-bf35-d1707630ab97>

  
**Interpretation:**
- Several hot-spots of bird observations are visible across different habitats, consistent with the ridge analysis results
- Urban hot-spots are found in densely built-up areas as well as in open spaces
- Forest hot-spots occur within the forest interior, but also in urban areas with abundant green patches
- Recognisable hot-spot where urban and forest analysis overlap

## Analysis of Areas with the Highest Observation Rate

```{r true-color-analysis}
# First step: visual analysis of the area loading the true colour satellite image  
truecolor <- rast("Eilenriede_True.tiff")

plotRGB(truecolor, r = 1, g = 2, b = 3, scale = 1, stretch = "lin")      
plot(occ_points, add = TRUE, pch = 20, cex = 0.5, col = "white") 
```
<p align="center">
<img width="514" height="513" src =https://github.com/user-attachments/assets/06eadf24-8bc5-4600-baf2-afa7e8008257>


**Interpretation:** Area with many observations appears to be a lake -> Next step: verify this observation using NDVI analysis
- Usually  the NDVI value of water bodies in the range of 0 to -0.5
- In small lakes the outer vegetation can become a disrupt this range (NDVI value and make it no longer distinguishable as a water body)

```{r water-analysis}
water_raster <- ndvi

#Threshold-based on classification: everything slightly negative or more is classified as water 
water_raster[water_raster > -0.009] <- NA   # Non-water (now NA)
water_raster[water_raster <= -0.009] <- 1   # Water

#Creating a base raster for the background to highlight the distribution of water 
background_raster <- ndvi
background_raster[] <- 1  # all cells = 1 to have a clear background 
background_raster[water_raster == 1] <- NA  # removes the cells that are classified water 

plot(background_raster, col = "grey80", legend = FALSE, 
     main = "Analysis of bird observations and water bodies (NDVI based)",
     xlab = "Longitude (DD)", ylab = "Latitude (DD)")

#Add water in blue as an overlay (add = TRUE)
plot(water_raster, col = "blue", add = TRUE, legend = FALSE)

#Adding the points can be useful to check if the points of observation are concentrated at water bodies 
plot(occ_points, add = TRUE, pch = 20, cex = 0.4, col = "black")

#Adding the legend to the plot, integrating all features shown in the graph
```
<p align="center">
<img width="514" height="513" src =https://github.com/user-attachments/assets/1a31972a-ebc3-4492-b97e-6eb33ac9c5b3>

**Interpretation:**
- This NDVI analysis of water remains partly inconclusive due to the small spatial extent of the water body relative to the raster resolution
- Several areas are also incorrectly classified as water, while some existing water bodies are not clearly detected
- However, the RGB true-colour imagery clearly shows that the area with a high concentration of bird observations contains a lake surrounded by a small park and green spaces
- The combination of open water, vegetation, and recreational park infrastructure likely provides favourable habitat conditions for birds, while simultaneously attracting high numbers of human visitors
- This overlap of suitable bird habitat and human activity offers a plausible explanation for the unusually high number of observations recorded in this location

## Conclusion

- **❌ Initial assumption disproved: Bird observations were not more frequent in urbanised areas**
- **✅ Higher observations in vegetated areas: More bird records occurred in natural/vegetated zones**
- **📊 Significant difference confirmed: Bird densities differ substantially between urban and natural land-cover types**
- **🌳 Vegetation drives patterns: Green spaces provide essential habitat that is attracting birds and is also accessible to humans**
- **📍 Natural features are critical: Green spaces within cities play vital roles in supporting urban wildlife**
- **🛰️ NDVI as key explanatory factor: Vegetation cover successfully explains observation patterns**

**Possible bias of the data/analysis:**
1. **Location accuracy bias:**
   - Some observations may use exact GPS coordinates, while others may only use a general location or proxy
2. **Observer effort bias:**
   - Areas with higher human activity (paths, urban areas, parks) are more likely to have birds observed and reported
3. **Spatial resolution bias:**
   - The resolution of satellite data (NDVI) might be too coarse, with a pixel size larger than fine-scale habitat features
   - Small parks, water bodies, or vegetation patches might be misrepresented, affecting the classification of urban vs. natural areas
