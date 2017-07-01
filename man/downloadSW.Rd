\name{downloadSW}
\alias{downloadSW}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Download satellite imagery and climate/atmospheric datasets using the SkyWatch API
}
\description{
Download satellite imagery and climate/atmospheric datasets using the SkyWatch API based on a query output object obtained from the querySW function.
}
\usage{
downloadSW(x, subset, api_key = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
data.frame returned by the querySW function
}
  \item{subset}{
logical expression indicating elements or rows to keep. See details
}
  \item{api_key}{
string; personal alphanumeric API key. See Details section
}
}
\details{
\code{x} must be a data.frame returned by the querySW function.

If \code{indexes} is NULL, all files in the \emph{download_path} column in \code{x} are downloaded. A numeric vector indicating rows of \code{x} can be provided to \code{indexes} to download only selected files in the \emph{download_path} column of \code{x}.

The \code{subset} argument works on the rows of the \code{x} object. Columns can be referred to (by name) as variables in the expression (see the examples).
}
\value{
Returns nothing
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
\dontrun{
api_key <- "your_personal_alphanumeric_api_key"

# Set the SkyWatchr.apikey option 
options(SkyWatchr.apikey = api_key)

res <- querySW(time_period = "2015-06", longitude_latitude = "48.676074,31.321119", 
               data_level = 3)
View(res)

# Download all files
downloadSW(res)

# Download a subset
downloadSW(res[c(1,3), ])

# Use an expresion to subset files to be downloaded
downloadSW(res, source == "MOPITT" & size_kb < 2400)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ spatial }% use one of  RShowDoc("KEYWORDS")

