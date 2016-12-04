downloadSW <- function(x, subset){
  
  if(missing(subset)){
    r <- rep_len(TRUE, nrow(x))
  } else {
    e <- substitute(subset)
    r <- eval(e, x, parent.frame())
  }
  x <- x[r,]
  
  for(i in 1:nrow(x)){
    urli <- x[i, "download_path"]
    elems <- unlist(strsplit(urli, "/", fixed = TRUE))
    file_name <- strsplit(elems[length(elems)], "?", fixed = TRUE)[[1]][1]
    cat(paste0("Downloading ", file_name, "... (", i, "/", nrow(x), ")", "\n"))
    download.file(urli, file_name)
  }
  cat(paste0("\n", "All requested files have been downloaded to: ", getwd()))
}