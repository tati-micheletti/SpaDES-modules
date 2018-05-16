ecodistrictMap <- function(){

    library('magick')
  browseURL("http://sis.agr.gc.ca/cansis/publications/maps/eco/all/districts/eco_all_districts_4m_west.jpg")

  return(warning(paste0("To see ecodistricts in regions other than British Columbia, Alberta, and Manitoba, please refer to maps on \n",
                        "http://sis.agr.gc.ca/cansis/publications/maps/eco/all/districts/index.html"), call. = FALSE))

}
