
defineModule(sim, list(
  name = "cropReprojectLCC2005",
  description = paste(
    "A translator module.",
    "It crops and/or reprojects a raster file to a smaller,",
    "cropped RasterLayer, defined by a shapefile that has iknformation on extent and projection.",
    "This was based on the cropReprojectLccAge SpaDES module"
  ),
  keywords = c("translator", "cropping", "raster crop", "shapefile", "crop to shapefile"),
  authors = c(person(c("Eliot", "J","B"), "McIntire", email = "eliot.mcintire@canada.ca", role = c("aut", "cre")),
              person("Tati", "Micheletti", email = "tati.micheletti@gmail.com", role = c("aut", "cre")),
              person("Alex", "Chubaty", email = "alexander.chubaty@canada.ca", role = c("aut", "cre"))),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.1.9011", cropReprojectLCC2005 = "0.0.1", SpaDES.tools = "0.1.1.9010", reproducible = "0.1.4.9016"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "cropReprojectLCC2005.Rmd"),
  reqdPkgs = list("SpaDES","quickPlot","sp","raster","sf","rgdal","tools","reproducible","gdalUtils","rgeos","sf", "stringr","stringi","RColorBrewer","magick"),
  parameters = rbind(
    defineParameter(".useCache", "logical", FALSE, NA, NA, paste0("Should this entire module be run with caching activated? This is generally intended",
                    "for data-type modules, where stochasticity and time are not relevant")),
    defineParameter("plotCroppedRasters", "logical", TRUE, NA, NA, "Should the vegMap and ageMap be plotted?")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "vegMap", objectClass = "RasterLayer", desc = "Vegetation map class raster", sourceURL = "ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip"),
    expectsInput(objectName = "ageMap", objectClass = "RasterLayer", desc = "Age class map raster", sourceURL = "ftp://ftp.daac.ornl.gov/data/nacp/NA_TreeAge//data/can_age04_1km.tif"),
    expectsInput(objectName = "cropTemplate", objectClass = "character", desc = "Choose among 'rP' for randomPolygon, 'eR' for ecodistrict or 'pR' for province", sourceURL = NA),
    expectsInput(objectName = "polyMatrix", objectClass = "matrix", desc = "For 'cropTemplate = rP', this is the latlong center point for the random polygon", sourceURL = NA),
    expectsInput(objectName = "areaSize", objectClass = "numeric", desc = "For 'cropTemplate = rP', this is the size in hactares of the random polygon", sourceURL = NA),
    expectsInput(objectName = "areaNumber", objectClass = "character", desc = "For 'cropTemplate = eR', this is the number of the ecodistrict to be cropped to", sourceURL = NA)
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "vegMap", objectClass = "RasterLayer", desc = "Vegetation map class raster"),
    createsOutput(objectName = "ageMap", objectClass = "RasterLayer", desc = "Age class map raster"),
    createsOutput(objectName = "studyArea", objectClass = "shapefile", desc = "Shapefile to crop to"),
    createsOutput(objectName = "ageMapInit", objectClass = "RasterLayer", desc = "Age class map raster: compatible with other modules"), # Taken out when other modules are ready
    createsOutput(objectName = "vegMapLcc", objectClass = "RasterLayer", desc = "Vegetation map class raster: compatible with other modules") # Taken out when other modules are ready
  )
))
## event types
#   - type `init` is required for initialiazation

doEvent.cropReprojectLCC2005 = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event

      sim$vegMap <- prepInputs(targetFile = "LCC2005_V1_4a.tif", archive = "LandCoverOfCanada2005_V1_4.zip",
                                  destinationPath = dataPath(sim))
      if(sim$cropTemplate=="rP"){

          sim$studyArea <- randomPolygon(x = sim$polyMatrix, hectares = sim$areaSize) %>%
              postProcess(targetFilePath = dataPath(sim),
                            destinationPath = dataPath(sim), rasterToMatch = sim$templateRaster)
      } else {

        if(sim$cropTemplate=="eD"){

          sim$studyArea <- prepInputs(url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/district/ecodistrict_shp.zip",
                                 destinationPath = dataPath(sim), rasterToMatch = sim$templateRaster)
            sim$studyArea <- sim$studyArea[sim$studyArea$ECODISTRIC == sim$areaNumber,]# %>%
#            sp::spTransform(CRSobj = proj4string(templateRaster))
        }
      }

        sim$vegMap <- postProcess(x = sim$vegMap, targetFilePath = file.path(dataPath(sim), "LCC2005_V1_4a.tif"),
                                  studyArea = sim$studyArea, destinationPath = dataPath(sim))

        sim$ageMap <- loadAndProcessAgeMap(sim = sim)

    ### To ensure compatibility with other LCC05 modules: (if other modules are updated to only vegMap and ageMap, these can be taken out)
        sim$vegMapLcc <- sim$vegMap
        sim$ageMapInit <- sim$ageMap

        if (sum(!is.na(getValues(sim$ageMapInit))) == 0) {
          stop("There are no age data provided with input age map")
        }
        if (sum(!is.na(getValues(sim$vegMapLcc))) == 0) {
          stop("There are no vegatation data provided with input vegatation map")
        }

      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim), "cropReprojectLCC2005", "plot")

    },
    plot = {

          if(P(sim)$plotCroppedRasters==TRUE){
            Plot(sim$vegMapLcc, title = "Vegetation Map")
            Plot(sim$ageMapInit, title = "Age Map")
          }

    },

    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {

  if(is.null(sim$cropTemplate)|length(sim$cropTemplate)==0){
    sim$cropTemplate <- "rP"
    warning(paste0("cropTemplate not provided. Using 'rP' (random polygon) as template for cropping."), call. = FALSE)}

  if(!suppliedElsewhere("polyMatrix", sim)&sim$cropTemplate=="rP") {sim$polyMatrix <- matrix(c(-122.85, 52.04), ncol = 2)
   warning(paste0("Random polygon coordinates not provided. Using random forested area in British Columbia."), call. = FALSE)}

  if(!suppliedElsewhere("areaSize", sim)&sim$cropTemplate=="rP") {sim$areaSize <- 500000
   warning(paste0("Random polygon area not provided. Using ", format(sim$areaSize, scientific=F), " hectares."), call. = FALSE)}

  if(!suppliedElsewhere("areaNumber", sim)&sim$cropTemplate=="eD") {sim$areaNumber <- 339
  warning(paste0("areaNumber for ecodistrict cropping was not provided. Using area '339'(Athabasca Plain, Boreal Shield - Saskatchewan)."), call. = FALSE)}

  return(invisible(sim))
}
