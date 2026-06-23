library(dplyr)
library(tidyr)
library(tidyverse)

getwd()

popden_0913 <- read.csv("C:/Users/mdickson/OneDrive - Metro Nashville Government/Desktop/CVS_Files/Census tract profile/popden_0913.csv")
popden_1418 <- read.csv("C:/Users/mdickson/OneDrive - Metro Nashville Government/Desktop/CVS_Files/Census tract profile/popden_1418.csv")
popden_1923 <- read.csv("C:/Users/mdickson/OneDrive - Metro Nashville Government/Desktop/CVS_Files/Census tract profile/popden_1923.csv")


uninsured_0913 <- read.csv("C:/Users/mdickson/OneDrive - Metro Nashville Government/Desktop/CVS_Files/Census tract profile/uninsured_0913.csv")
uninsured_1418 <- read.csv("C:/Users/mdickson/OneDrive - Metro Nashville Government/Desktop/CVS_Files/Census tract profile/uninsured_1418.csv")
uninsured_1923 <- read.csv("C:/Users/mdickson/OneDrive - Metro Nashville Government/Desktop/CVS_Files/Census tract profile/uninsured_1923.csv")


popden_0913 <- popden_0913 %>%
  select(GeoID, TimeFrame, rpopden, GeoVintage, Source, Location)


popden_1418 <- popden_1418 %>%
  select(GeoID, TimeFrame, rpopden, GeoVintage, Source, Location)


popden_1923 <- popden_1923 %>%
  select(GeoID, TimeFrame, rpopden, GeoVintage, Source, Location)



uninsured_0913 <- uninsured_0913 %>%
  select(GeoID, TimeFrame,ppopwoins,GeoVintage, Source, Location)
uninsured_0913$ppopwoins <- as.numeric(uninsured_0913$ppopwoins)

uninsured_1418 <- uninsured_1418 %>%
  select(GeoID, TimeFrame,ppopwoins,GeoVintage, Source, Location)
uninsured_1418$ppopwoins <- as.numeric(uninsured_1418$ppopwoins)


uninsured_1923 <- uninsured_1923 %>%
  select(GeoID, TimeFrame,ppopwoins,GeoVintage, Source, Location)
uninsured_1923$ppopwoins <- as.numeric(uninsured_1923$ppopwoins)

##combine all sets into one larger dataset for analysis - population density
popden_0923 <- list(popden_0913, popden_1418, popden_1923)
popden_0923 <- popden_0923 %>%
  reduce(full_join, by="GeoID")

popden_0923 <- na.omit(popden_0923)

popden_0923 <- popden_0923 %>%
  mutate(
    rc_1 = round((rpopden.y - rpopden.x)/rpopden.x * 100, 1),
    rc_2 = round((rpopden - rpopden.y)/rpopden.y * 100, 1),
    rc_3 = round((rpopden - rpopden.x)/rpopden.x * 100, 1)
  )

##combine all sets into one larger dataset for analysis - rate of uninsured
uninsured_0923 <- list(uninsured_0913, uninsured_1418, uninsured_1923)
uninsured_0923 <- uninsured_0923 %>%
  reduce(full_join, by="GeoID")

uninsured_0923 <- na.omit(uninsured_0923)

uninsured_0923 <- uninsured_0923 %>%
  mutate(
    rc_1 = round((ppopwoins.y - ppopwoins.x)/ppopwoins.x * 100, 1),
    rc_2 = round((ppopwoins - ppopwoins.y)/ppopwoins.y * 100, 1),
    rc_3 = round((ppopwoins - ppopwoins.x)/ppopwoins.x * 100, 1)
  )

##write csv files for the combined data to be used in ArcGIS for visualizations
write.csv(uninsured_0923, "C:/Users/mdickson/OneDrive - Metro Nashville Government/Desktop/CVS_Files/Census tract profile/uninsured_0923.csv")

write.csv(popden_0923, "C:/Users/mdickson/OneDrive - Metro Nashville Government/Desktop/CVS_Files/Census tract profile/popden_0923.csv")

