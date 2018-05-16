loadAndProcessAgeMap <- function(sim = sim){
  ageMap <- raster::raster(file.path(dataPath(sim),"can_age04_1km.tif")) %>%
    postProcess(targetFilePath = file.path(dataPath(sim),"can_age04_1km.tif"), destinationPath = dataPath(sim), #Not sure I should use destinationPath?!
                studyArea = sim$studyArea, rasterToMatch = sim$vegMap)
  ageMap[] <- round(ageMap[], 0)
  cols <- length(which(!is.na(unique(getValues(ageMap)))))
  ageMap <- setColors(ageMap, n=cols,colorRampPalette(c("LightGreen", "DarkGreen"))(cols))

  return(ageMap)
}

