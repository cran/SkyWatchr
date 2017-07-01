querySW <- function(api_key = NULL, time_period, longitude_latitude, instrument_satellite = NULL, data_level = NULL, 
                     max_resolution = NULL, max_cloudcover = NULL, wavelength_band = NULL, output = "data.frame"){
  
  if (is.null(api_key)) {
    api_key <- getOption("SkyWatchr.apikey")
    
    if(is.null(api_key)) {
      message("You need to set an API key via options(SkyWatchr.apikey = 'your_api_key')")
    }
  }
  
  if (is.null(time_period)) time_period <- Sys.Date()
  
  if(is(longitude_latitude, "Spatial")) longitude_latitude <- paste0(bbox(longitude_latitude), collapse = ",")
  
  URL <- paste0("https://api.skywatch.co/data", "/time/", time_period, "/location/", longitude_latitude)
  
  if(!is.null(instrument_satellite)) URL <- paste0(URL, "/source/", instrument_satellite)
  if(!is.null(data_level)) URL <- paste0(URL, "/level/", data_level)
  if(!is.null(max_resolution)) URL <- paste0(URL, "/resolution/", max_resolution)
  if(!is.null(max_cloudcover)) URL <- paste0(URL, "/cloudcover/", max_cloudcover)
  if(!is.null(wavelength_band)) URL <- paste0(URL, "/band/", wavelength_band)
  
  query <- GET(paste0(URL, "?"), add_headers('Accept' = 'application/json', 'x-api-key' = api_key))
  
  res <- content(query)
  res <- lapply(res, unlist)
  res <- as.data.frame(do.call(rbind, lapply(res, function(x){x[!grepl("area.coord", names(x))]})))
  res[] <- lapply(res, as.character)
  res$size_kb <- round(as.numeric(res$size)/1e3, 1)
  res$cloud_cover <- as.numeric(res$cloud_cover)
  res$resolution <- as.numeric(res$resolution)

  res1 <- res[, which(colnames(res) == 'area.bbox1'):which(colnames(res) == 'area.bbox4')]
  
  area <- apply(res1, 1, function(x){
    x.lb <- paste(x[1], x[2], collapse = " ") # left bottom
    x.lt <- paste(x[1], x[4], collapse = " ") # left top
    x.rt <- paste(x[3], x[4], collapse = " ") # right top
    x.rb <- paste(x[3], x[2], collapse = " ") # right bottom 
    return(paste0("POLYGON((", paste(x.lb, x.lt, x.rt, x.rb, x.lb, sep = ","), "))"))
  })
  
  res <- res[, (max(grep("area.bbox", colnames(res))) + 1):ncol(res)]
  res$area <- area
  
  if(output == "html"){
    
    html.res <- interactiveTable(res)
    
    for (i in 1:nrow(res)){
      html.res <- gsub(res[i, "download_path"], paste0("<a href='", res[i, "download_path"], "'>", res[i, "download_path"], "</a>"),
                       html.res, fixed = TRUE)
    }
    print(html.res)
  }
  
  return(res)
}