\name{querySW}
\alias{querySW}
\title{Query the SkyWatch API for satellite imagery and climate/atmospheric datasets}
\description{
Search satellite imagery and climate/atmospheric datasets by wavelength (band), cloud cover, resolution, location, date, etc. using the SkyWatch API. Available datasets include ACOS, AIRS, CAI, FTS-SWIR, Landsat-8, MOPITT, OCO2, Sentinel-2 and TES. To learn more about the SkyWatch API, see \href{https://github.com/skywatchspaceapps/api}{https://github.com/skywatchspaceapps/api}. 
}
\usage{
querySW(api_key = NULL, time_period, longitude_latitude, instrument_satellite = NULL, 
        data_level = NULL, max_resolution = NULL, max_cloudcover = NULL, 
        wavelength_band = NULL, output = "data.frame")
}
\arguments{
  \item{api_key}{
string; personal alphanumeric API key. See Details section
}
  \item{time_period}{
string; one or two UTC timestamps in ISO format (yyyy-mm-ddThh:mm:ss.sssss+|-zzzz). See Details section
}
  \item{longitude_latitude}{
string or an Spatial object. See Details section
}
  \item{instrument_satellite}{
string;  source of the data, either the instrument on-board the satellite or the satellite itself. See Details section
}
  \item{data_level}{
numeric; data processing levels for Earth observation data. See Details section
}
  \item{max_resolution}{
numeric; maximum spatial resolution (in meters). See Details section
}
  \item{max_cloudcover}{
numeric; maximum cloud cover (in percentage). See Details section
}
  \item{wavelength_band}{
string; wavelength bands for imagery (i.e. Landsat-8) and by file type for non-imagery data (e.g. Hierarchical-Data-Format). See Details section
}
  \item{output}{
string; either "data.frame" (default) or "html". "html" returns a data.frame and prints it as html.
}
}
\details{
\code{api_key} is a personal alphanumeric API key created for and provided to a user once registered at \href{http://www.skywatch.co/request-access}{http://www.skywatch.co/request-access}. For a cleaner code, users can set the global option \code{SkyWatchr.apikey}  (recommended) once per session via: \code{options(SkyWatchr.apikey = 'your_api_key')}.

\code{time_period} corresponds to one or two UTC timestamps in ISO format (yyyy-mm-ddThh:mm:ss.sssss+|-zzzz). Partial dates can also be specified: 2009, 2009-12, 2009-12-25, 2009-12-25T13:25:00.0000+0000. If only one timestamp is passed in, a one day range is assumed. For example, if 2009-12-25 is specified, the search takes place as if 2009-12-25,2009-12-26 was specified.

\code{longitude_latitude} can be an object of class 'Spatial' (in geographic coordinates) or a string. When supplied as string, \code{longitude_latitude} corresponds to a list of longitude, latitude coordinate pairs as a flat, comma-separated list. A list of two numbers represents a point, four numbers is a square area where the coordinates are the corners, or if there are more than four numbers the coordinates represent a closed polygon, where the first point equals the last point in the list. Because this list represents a number of points, there always has to be an even number of numbers in the list. Examples:

Point: -71.1043443253,-42.3150676016

Square: -71.1043443253471,-42.3150676015829,71.1043443253471,42.3150676015829

Polygon: -71.1043443253471,-42.3150676015829,71.1043443253471,-42.3150676015829,

71.1043443253471,42.3150676015829,-71.1043443253471,42.3150676015829,

-71.1043443253471,-42.3150676015829

\code{instrument_satellite} corresponds to the source of the data, either the instrument on-board the satellite or the satellite itself. Single or multiple sources can be specified. Choice of sources are: ACOS, AIRS, CAI, FTS-SWIR, Landsat-8, MOPITT, OCO2, Sentinel-2 and TES. This field is not case-sensitive, and multiple sources can be specified (separated by commas).

\code{data_level} corresponds to the data processing levels for Earth observation data. Level 1, 2, and 3 (L1, L2, L3) datasets are available. If no data level is specified, datasets of all levels will be returned. Only a single level can be specified. Choices are: 1, 2, and 3.

\code{max_resolution} is only applicable to imagery that's available through the API (i.e. Landsat-8). Resolution is in meters (m). Resolutions less-than or equal-to this value will be returned. The resolution for Landsat-8 is 15 m. All climate/atmospheric datasets have a resolution of 0 m, because it is not applicable. The maximum resolution is 30 m. If resolution is omitted all imagery or data matching other search criteria will be returned.

\code{max_cloudcover} is only applicable to imagery that's available through the API (i.e. Landsat-8). Cloud cover is given as a percentage (%) of the image covered by cloud (0 to 100). Images less-than or equal-to this cloud cover value will be returned. All climate/atmospheric datasets have a cloud cover of 0%, because it is not applicable. If cloud cover is omitted all imagery or data matching other search criteria will be returned.

\code{wavelength_band} can be specified by the wavelength bands for imagery (i.e. Landsat-8) and by file type for non-imagery data (e.g. Hierarchical-Data-Format). Choices of bands are: Blue, Cirrus, Coastal-Aerosol, Green, Hierarchical-Data-Format, Near-Infrared, Panchromatic, Red, Short-Wave-Infrared-1, Short-Wave Infrared-2, Thermal-Infrared-1, and Thermal-Infrared-2. This field is not case-sensitive, and multiple bands can be specified (separated by commas).
}
\value{
data.frame containing query output
}
\author{
Ali Santacruz; thanks to Joshua Kunst for kindly contributing the SkyWatchr.apikey option code
}
\examples{
\dontrun{
api_key <- "your_personal_alphanumeric_api_key"

# Set the SkyWatchr.apikey option 
options(SkyWatchr.apikey = api_key)

# Query data for one of the world largest landfills "Olusosun Dump" in Nigeria
querySW(time_period = "2015-8", longitude_latitude = "6.566358,3.367358,6.586358,3.387358")

# If the SkyWatchr.apikey option is not set, then provide the API key
querySW(api_key, time_period = "2015-8", longitude_latitude = "6.566358,3.367358,6.586358,3.387358")

# An Imperial Oil refinery in Canada
res <- querySW(time_period = 2015, longitude_latitude = "36.281389,-80.060278", data_level = 3)
View(res)

# The smoggiest city on Earth Ahvaz, Iran
querySW(time_period = "2015-06", longitude_latitude = "31.321119,48.676074", data_level = 3)

# Ahvaz, Iran in September, 2015
querySW(time_period = "2015-9", longitude_latitude = "31.321119,48.676074", data_level = 1, 
        wavelength_band = "red,green,blue")

# Other examples
querySW(time_period = "2009-12-25", longitude_latitude = "-71.1043443253471,-42.3150676015829", 
        data_level = 2)
querySW(time_period = "2009-12-25", longitude_latitude = "-71.1043443253471,-42.3150676015829")

querySW(time_period = "2016-07-11,2016-07-12", 
        longitude_latitude = "-71.1,-42.3,71.1,-42.3,71.1,42.3,-71.1,42.3,-71.1,-42.3",
        instrument_satellite = "Landsat-8", data_level = 1, max_resolution = 30, 
        max_cloudcover = 100, wavelength_band = "Blue")

# Print output data.frame as html
querySW(time_period = "2015-8", longitude_latitude = "6.56635,3.36735,6.58635,3.38735", 
        output = "html")
querySW(time_period = "2016-07-11,2016-07-12", 
        longitude_latitude = "-71.1,-42.3,71.1,-42.3,71.1,42.3,-71.1,42.3,-71.1,-42.3",
        instrument_satellite = "Landsat-8", data_level = 1, max_resolution = 30, 
        max_cloudcover = 100, wavelength_band = "Blue", output = "html")

# Queries can also be performed using objects of class Spatial (as defined by the sp package) 
# projected in geographic coordinates
ex1 <- data.frame(x = -71.1043443253, y = -42.3150676016, data = "point")
coordinates(ex1) <- ~ x + y
class(ex1)
querySW(time_period = "2015-8", longitude_latitude = ex1)
}
}
\keyword{ spatial }% use one of  RShowDoc("KEYWORDS")