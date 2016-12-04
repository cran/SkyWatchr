querySW <- function(api_key, time_period = NULL, longitude_latitude, instrument_satellite = NULL, data_level = NULL, 
                    max_resolution = NULL, max_cloudcover = NULL, wavelength_band = NULL, output = "data.frame"){
  
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
  
  res <- as.data.frame(do.call(rbind, lapply(res, unlist)))
  res[] <- lapply(res, as.character)
  res$size <- round(as.numeric(res$size)/1e3, 1)
  res$cloud_cover <- as.numeric(res$cloud_cover)
  res$resolution <- as.numeric(res$resolution)
  colnames(res)[which(colnames(res) == 'size')] <- 'size_kb'  

  if(output == "html"){

    html.res <- htmlTable(res)

    for (i in 1:nrow(res)){
      html.res <- gsub(res[i, "download_path"], paste0("<a href='", res[i, "download_path"], "'>", res[i, "download_path"], "</a>"),
                       html.res, fixed = TRUE)
    }
    print(html.res)
  }
  
  return(res)
}