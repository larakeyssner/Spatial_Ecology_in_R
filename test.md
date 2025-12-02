#***********************************************************************************#
##### DESERTIFICATION IN SICILY, FOCUS ON PARCO DELLE MADONIE AND FUTURE TRENDS #####
#***********************************************************************************#

### INDEX

# 1. INTRODUCTION
# 2. HYPOTHESIS
# 3. SOURCE
# 4. INSTALLATION AND UPLOAD OF THE NECESSARY PACKAGES
# 5. CREATION OF DATAFRAME WITH TEMPERATURES AND PRECIPITATION  OF 2010-2015-2020-2024 SUMMER
# 6. DOWNLOAD AND VISUALIZATION OF EACH MEAN NDVI OF EVERY SUMMER
# 7. DIFF NDVI 2024 - 2010
# 8. CORRELATION BETWEEN TOTAL PRECIPITATION AND AVERAGE TEMPERATURE WITH NDVI
# 9. CALCULATION OF 2024 NDVI FROM SENTINEL-2
# 10. CONSISTENCY BETWEEN MODIS AND SENTINEL-2 MODIS
# 11. CALCULATION OF COEFFICIENT OF DETERMINATION
# 12. PREDICTION OF NDVI MAP FOR 2030
# 13. STATISTIC NDVI VALUE PREDICTION FOR 2030
# 14. CONCLUSIONS


## 1. INTRODUCTION
# It is known that Sicily is one of the most dry and with extreme hot temperature italian regions, 
# and every year the situation is aggravating. I decided to study a specific region of Sicily,
# which is Parco delle Madonie, a naturalistic protected park.

## 2. HYPOTESIS
# Specifically, I wanted to see if the is a correlation between the increase of temperature and decrease
# of precipitation with the decrease of vegetation. 

## 3. SOURCE
# https://appeears.earthdatacloud.nasa.gov/explore # Used to download the images + NDVI of Parco delle Madonie
# https://browser.dataspace.copernicus.eu/ # Used to download the image of Parco delle Madonie 2024 - B04 and B08
# https://www.protectedplanet.net/32713 # Used to cut the region of Parco delle Madonie
# https://meteostat.net/it/place/it/petralia-sottana # Used to download the data of temperature and precipitation


## 4. INSTALLATION AND UPLOAD OF THE NECESSARY PACKAGES
# I install the packages I am going to use 
install.packages("terra") # To handle raster data (e.g., satellite images in .tif format).
install.packages("readr") # To read tabular data (CSV or TXT files) efficiently.
install.packages("ggplot2") # To create high-quality plots.
install.packages("dplyr") # To manipulate and clean data frames.
install.packages("viridis") # To apply colorblind-friendly palettes to maps and plots.
install.packages("reshape2") # To reshape data frames (especially from wide to long format).

## I upload the packages
library(terra) 
library(readr)
library(ggplot2)
library(dplyr)
library(viridis)
library(reshape2)


## 5. CREATION OF DATAFRAME WITH TEMPERATURES AND PRECIPITATION  OF 2010-2015-2020-2024 SUMMER

# Creation of dataframe: temperature and precipitation in Sicily for each year analysed (2010, 2015, 2020, 2025)
# To create a dataframe I use can not use the package meteostat because is too old for R and Rstudio, so I have to download them manually
# I download the data of temperature and precipitation for the period 01/06 - 31/08 of each year in Parco delle Madonie, located in Petralia Sottana
# from the website of meteostat.

# I upload the data of the meteo station from my file of each year
meteo2010 <- read_csv("C:/SpatialEcology_Progetto/ParcoMadonie_2010.csv")

# I check the columns
head(meteo2010)

meteo2015<- read_csv("C:/SpatialEcology_Progetto/ParcoMadonie_2015.csv")
head(meteo2015)

meteo2020<- read_csv("C:/SpatialEcology_Progetto/ParcoMadonie_2020.csv")
head(meteo2020)

meteo2024<- read_csv("C:/SpatialEcology_Progetto/ParcoMadonie_2024.csv")
head(meteo2024)

# I bind all the data in one file
meteo_all <- bind_rows(meteo2010, meteo2015, meteo2020, meteo2024)

# I make sure that the date are under the format date
meteo_all$date <- as.Date(meteo_all$date)

# I extract the year
meteo_all$year <- format(meteo_all$date, "%Y")

# I calculate the mean of temperature and precipitation for each year
meteo_summary <- meteo_all %>%
  group_by(year) %>%
  summarise(
    average_temperature = mean(tavg, na.rm = TRUE),
    total_precipitation = sum(prcp, na.rm = TRUE)
  )

# I print the result, I will need the information later to do the graphs to 
# visualize the trend of precipitation and temperatures
print(meteo_summary)


# 6. DOWNLOAD AND VISUALIZATION OF EACH MEAN NDVI OF EVERY SUMMER
# to download the images of the park for 2010, 2015, 2020, 2024 I have to use appEEARS, another 
# NASA website with geodata, beacuse even though is not extreme good quality (use of MODIS, which has a
# resolution of one pixel per 250 m^2) it is better for me to use 
# because there is no data of the years 2010 and 2015 with Sentinel-2, so I use always the same kind of 
# map to be more coherent and scientific. 

# For the year 2010 I downloaded the images from 1/06/2010 to 31/08/2010 in .tiff format, with NDVI included
# then I made the mean of the NDVI to get just one image to see

# I decided to use these colors - blue, grey, green - to have an higher visible impact on the plot
# so I created the function with the new palette
palette_ndvi <- colorRampPalette(c("#313695", "#f7f7f7", "#006837"))  

# Year 2010
tif_folder2010 <- "C:/SpatialEcology_Progetto/Madonie_2010"

# I created a list of files .tif
tif_files2010 <- list.files(path = tif_folder2010, pattern = "\\.tif$", full.names = TRUE)

# I check that every file is uploaded
print(tif_files2010)

# I put all the raster in one stack
ndvi_stack2010 <- rast(tif_files2010)

 # I calculated the average doing it pixel-per-pixel
 ndvi_mean2010 <- mean(ndvi_stack2010, na.rm = TRUE)

 # I visualize the NDVI mean on the map with a variation of the colorblind friendly palette "viridis"
  plot(ndvi_mean2010, 
            main = "Mean NDVI - Summer 2010 (Parco delle Madonie)",
            col = palette_ndvi(100))   ## Generate 100 color values from the custom color palette
                                        ## This creates a smooth transition for the NDVI map

# I repeted the process for each interested year
# Year 2015

tif_folder2015 <- "C:/SpatialEcology_Progetto/Madonie_2015"

tif_files2015 <- list.files(path = tif_folder2015, pattern = "\\.tif$", full.names = TRUE)

print(tif_files2015)

ndvi_stack2015 <- rast(tif_files2015)

ndvi_mean2015 <- mean(ndvi_stack2015, na.rm = TRUE)

  plot(ndvi_mean2015, 
          main = "Mean NDVI - Summer 2015 (Parco delle Madonie)",
           col = palette_ndvi(100)) 

# Year 2020

tif_folder2020 <- "C:/SpatialEcology_Progetto/Madonie_2020"

tif_files2020 <- list.files(path = tif_folder2020, pattern = "\\.tif$", full.names = TRUE)

print(tif_files2020)

ndvi_stack2020 <- rast(tif_files2020)

ndvi_mean2020 <- mean(ndvi_stack2020, na.rm = TRUE)

  plot(ndvi_mean2020, 
          main = "Mean NDVI - Summer 2020 (Parco delle Madonie)",
           col = palette_ndvi(100))

# Year 2024

tif_folder2024 <- "C:/SpatialEcology_Progetto/Madonie_2024"

tif_files2024 <- list.files(path = tif_folder2024, pattern = "\\.tif$", full.names = TRUE)

print(tif_files2024)

ndvi_stack2024 <- rast(tif_files2024)

ndvi_mean2024 <- mean(ndvi_stack2024, na.rm = TRUE)

  plot(ndvi_mean2024, 
          main = "Mean NDVI - Summer 2024 (Parco delle Madonie)",
           col = palette_ndvi(100))


# 7. DIFF NDVI 2024 - 2010
# Now I calculate the NDVI diff between 2024 to 2010 to see if the vegetation has changed in 14 years

# I calculate the NDVI diff 
ndvi_diff_2010_2024 <- ndvi_mean2024 - ndvi_mean2010

# I upload the library I need for the plot

library(terra)
library(viridis)


# I do the raster with the new palette
plot(ndvi_diff_2010_2024,
     col = palette_ndvi(100),
     main = "NDVI Change (2024 - 2010)"
    )

# 8. CORRELATION BETWEEN TOTAL PRECIPITATION AND AVERAGE TEMPERATURE WITH NDVI
# Then I create a dataframe with all my data to compare vegetation, precipitation and temperature between all four years

# I find the mean NDVI of each year with the function global of the package terra
# The global() function calculates summary statistics (e.g., mean, min, max)
# for the entire raster layer. It is used to extract a single value that 
# represents the whole dataset, such as the average NDVI of a given year.
# it is different from ndvi_mean, which is a raster and not a number, where every pixel has a NDVI value

ndvi_2010_val <- global(ndvi_mean2010, fun = mean, na.rm = TRUE)[[1]] # 'na.rm = TRUE' it is used to remove
                                                                      # the missing values (NA) of those pixel
                                                                      # that don't have data (becaus of clouds...)
ndvi_2015_val <- global(ndvi_mean2015, fun = mean, na.rm = TRUE)[[1]]
ndvi_2020_val <- global(ndvi_mean2020, fun = mean, na.rm = TRUE)[[1]]
ndvi_2024_val <- global(ndvi_mean2024, fun = mean, na.rm = TRUE)[[1]]

# I put everything in a dataframe 
dati_ndvi <- data.frame(
  year = c(2010, 2015, 2020, 2024),
  mean_ndvi = c(ndvi_2010_val, ndvi_2015_val, ndvi_2020_val, ndvi_2024_val)
)

# I print the mean NDVI
print(dati_ndvi)

# I print again my meteo data
print(meteo_summary)

# Now I can get a dataframe with all the necessary data to create a plot
complete_data <- data.frame(
  year = c(2010, 2015, 2020, 2024),
  mean_ndvi = c(0.563, 0.579, 0.557, 0.527),
  mean_temp = c(24.0, 21.9, 25.0, 25.1),
  total_precip = c(5, 72.9, 9.2, 14.1)
)

# Now I do a NDVI plot

ggplot(complete_data, aes(x = year, y = mean_ndvi)) +
  geom_line(color = "#1b9e77", size = 1.2) +
  geom_point(color = "#1b9e77", size = 3) +
  labs(title = "Mean Summer NDVI", x = "Year", y = "NDVI") +
  theme_minimal()

# Temperature plot
ggplot(complete_data, aes(x = year, y = mean_temp)) +
  geom_line(color = "#d95f02", size = 1.2) +
  geom_point(color = "#d95f02", size = 3) +
  labs(title = "Mean Summer Temperature", x = "Year", y = "°C") +
  theme_minimal()

# Precipitation plot
ggplot(complete_data, aes(x = year, y = total_precip)) +
  geom_line(color = "#7570b3", size = 1.2) +
  geom_point(color = "#7570b3", size = 3) +
  labs(title = "Total Summer Precipitation", x = "Year", y = "mm") +
  theme_minimal()


# Now I want to see if there is a correlation between the variance of NDVI and temperature/precipitation
# I correlate the data

cor(complete_data$mean_ndvi, complete_data$mean_temp)
cor(complete_data$mean_ndvi, complete_data$total_precip)

# I make the plots

# Correlation NDVI and precipitation
ggplot(complete_data, aes(x = total_precip, y = mean_ndvi)) +
  geom_point(size = 3, color = "darkgreen") +
  geom_smooth(method = "lm", se = TRUE, color = "orange") + # I added the trend line that follows a linear model, 
                                                            # and the confidence interval (se = TRUE) 
  labs(
    title = "NDVI vs Summer Precipitation",
    x = "Total Summer Precipitation (mm)",
    y = "Mean NDVI"
  ) +
  theme_minimal()


# Correlation NDVI and temperature
ggplot(complete_data, aes(x = mean_temp, y = mean_ndvi)) +
  geom_point(size = 3, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  labs(
    title = "NDVI vs Summer Temperature",
    x = "Mean Summer Temperature (°C)",
    y = "Mean NDVI"
  ) +
  theme_minimal()

## CALCULATION OF 2024 NDVI FROM SENTINEL-2
# Now I want to calculate the NDVI with a image taken with sentinel-2 on Copernicus Browser, which is more accurate 
# This is necessary to see if the MODIS NDVI is really rapresentative of the vegetation, to verify that 
# MODIS has valid data

library(terra)

# I upload the raster (B04) - the red band is telling me how much red light the plants are absorbing (and 
# using for photosynteshis), so if I have a low value of B04, it means that plants are absorbing a lot 
# of light and are alive and healthy


# I upload the .shp file that let me cut the park (Using the percorso I have saved it in)
parco <- vect("C:/SpatialEcology_Progetto/ParcoMadonie/ParcoMadonie.shp")

# I upload the band b04
b04 <- rast("C:/SpatialEcology_Progetto/B04.tiff")

# I cut the raster in the extension of the park, to be more precise

b04_crop <- crop(b04, parco)
b04_masked <- mask(b04_crop, parco)

library(viridis)

plot(b04_masked,
    col = viridis(100),
     main = "B04 - Clipped to Parco delle Madonie",
     zlim = c(0, max(values(b04_masked), na.rm = TRUE)))

# I upload the raster b08 - the NIR (NearInfraRed) is the invisible to human eye light that plants reflect
# B08 has high value when plants reflect more, and it means that they are alive and not stressed
b08 <- rast("C:/SpatialEcology_Progetto/B08.tiff")

# I cut B08 on the park extension too
b08_crop <- crop(b08, parco)
b08_masked <- mask(b08_crop, parco)


plot(b08_masked,
    col = viridis(100),
     main = "B08 - Clipped to Parco delle Madonie",
     zlim = c(0, max(values(b04_masked), na.rm = TRUE)))

# Now I can calculate the NDVI Sentinel-2
# to get the NDVI I have to:
ndvi_s2_2024 <- (b08_masked - b04_masked) / (b08_masked + b04_masked)


## 10. CONSISTENCY BETWEEN MODIS AND SENTINEL-2 MODIS
# I recall the library I need
library(terra)
library(ggplot2)

# I reproject the Sentinel-2 NDVI raster to match the coordinate reference system of the MODIS NDVI raster.
# This ensures spatial alignment between the two datasets. Bilinear interpolation is used for continuous data.

ndvi_s2_projected <- project(ndvi_s2_2024, ndvi_mean2024, method = "bilinear")

# I resample the Sentinel NDVI raster to match the resolution and extent of the MODIS NDVI raster.
# This is necessary to allow pixel-by-pixel comparison between the two datasets.

ndvi_s2_resampled <- resample(ndvi_s2_projected, ndvi_mean2024)

# I extract the pixel values from both the MODIS and resampled Sentinel-2 NDVI rasters
ndvi_compare <- cbind(values(ndvi_mean2024), values(ndvi_s2_resampled))

# I convert the combined pixel values into a data frame for easier manipulation and plotting
ndvi_df <- as.data.frame(ndvi_compare)

# I assign appropriate column names to the data frame
colnames(ndvi_df) <- c("NDVI_MODIS", "NDVI_SENTINEL")

# I remove any rows containing NA values to ensure accurate analysis
ndvi_df <- na.omit(ndvi_df)

# Now I cane create a plot where I can visualize if the rappresentation accurate

library(ggplot2)
ggplot(ndvi_df, aes(x = NDVI_MODIS, y = NDVI_SENTINEL)) +
  geom_point(alpha = 0.3, color = "darkviolet") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "NDVI MODIS vs Sentinel-2 – Summer 2024",
    x = "NDVI MODIS (2024)",
    y = "NDVI Sentinel-2 (2024)"
  ) +
  theme_minimal()

## 11. CALCULATION OF COEFFICIENT OF DETERMINATION
# To understan if the sentinel-2 data can be explained well with MODIS, I calculate the 
# coefficient of determination R^2, where if R^2 > 0.8 it means that the two sensor are
# coherent to measure vegetation

# I calculate the regression 
mod <- lm(NDVI_SENTINEL ~ NDVI_MODIS, data = ndvi_df)

# I calculate R^2
r2 <- summary(mod)$r.squared
print(paste("R-squared:", round(r2, 3)))


## 12. PREDICTION NDVI MAP FOR 2030
# Now I want to do a prediction of NDVI based on the previously years, assuming that it is a constat 
# and linear variation. In this way I'm doing a raster, resulting in a map of 2030 NDVI where I have 
# the spatial variation of NDVI for each pixel

# Linear model
ndvi_model <- lm(mean_ndvi ~ year, data = complete_data)

# Prevision for 2030
predicted_ndvi <- predict(ndvi_model, newdata = data.frame(year = 2030))
print(predicted_ndvi)

# NDVI trend: mean variation every 14 years
ndvi_diff <- ndvi_mean2024 - ndvi_mean2010   # Both the ndvi are raster, I'm getting the difference 
                                             # between the two images pixel per pixel
ndvi_trend <- ndvi_diff / 14  # mean yearly variation for each pixel

# I project it to the year 2030 (so i multiply the mean NDVI of each pixel x 6 and add the the 2024 ndvi) 
ndvi_2030 <- ndvi_mean2024 + ndvi_trend * 6

# I visualise it in a map
plot(ndvi_2030,
     col = palette_ndvi(100),
     main = "Estimated NDVI - Summer 2030")

## 13. STATISTIC NDVI VALUE PREDICTION FOR 2030
# Now I do a statistic linear regression to understand how NDVI will be in 2030, 
# which will be even more statistical significative

# I create a dataframe with the information of every NDVI of each year, which I'm going to use 
# to understend the trend in the years
ndvi_years <- data.frame(
  year = c(2010, 2015, 2020, 2024),
  mean_ndvi = c(0.5631159, 0.5792164, 0.5578672, 0.5273455)
)

# I do a linear regression to calculate the NDVI in the year 2030, this is done by understanding
# the correlation between year and mean NDVI
model <- lm(mean_ndvi ~ year, data = ndvi_years)

# Assuming that the trend is linear, I predict NDVI for 2030
predicted_2030 <- predict(model, newdata = data.frame(year = 2030))

# Add 2030 prediction to the dataframe for plotting
ndvi_years <- rbind(ndvi_years, data.frame(year = 2030, mean_ndvi = predicted_2030))

# Plot
ggplot(ndvi_years, aes(x = year, y = mean_ndvi)) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "red", size = 3) +
  ylim(0.50, 0.60) +    # I know that my NDVI are in a range between 0.50 and 0.60, so a put this limit on the y aes
  labs(
    title = "Mean Summer NDVI Trend (2010–2030)",
    x = "Year",
    y = "Mean NDVI") +
  theme_minimal()


#### 14. CONCLUSIONS ####

# Variation of temperature and precipitation are correlated to the variation of vegetation, meaning that
# when there is a higher temperature and lower precipitation there is less vegetation too

# Due to the increase of extreme variables the vegetation is decreasing, the previosion for 2030
# shows a lower NDVI if it follows a linear trend
