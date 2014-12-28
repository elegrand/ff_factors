get_FF_3factor <- function(ff_url, ff_file, local_file=ff_file, skip, 
                          remove_last_row=FALSE) {
  download.file(url=paste0(ff_url, ff_file), destfile=local_file)
  x <- unzip(zipfile=paste0("~/Documents/finance/", ff_file), list=TRUE)
  if (missing(skip)){
    lines <- readLines(unz(ff_file, x$Name))
    i <- which(sapply(lines, nchar) == 0)
    header <- unlist(strsplit(lines[i[[1]]+1], " "))
    header <- header[nchar(header) > 0]
    rng <- c(i[[1]]+2, i[[2]] - 1)
    l <- lapply(lines[seq(rng[[1]], rng[[2]], 1)],
           function(x) {
             s <- unlist(strsplit(x, " "))
             matrix(s[nchar(s) > 0], nrow=1)
           })
    m <- do.call(rbind, l)
    dates <-  as.Date(paste(
      substr(m[ ,1],1,4), substr(m[ ,1],5,6), "01", sep="-"), "%Y-%m-%d")
    ff_data <- data.frame(m[, -1], stringsAsFactors=FALSE) # make data.frame, convert to numeric later
    names(ff_data) <- header
  } else{
    ff_data <- read.table(unz(ff_file, x$Name), header=T, skip=skip,
                          stringsAsFactors=FALSE) 
    if(remove_last_row)
      ff_data <- ff_data[-nrow(ff_data), ] #  remove copyright row
    
    dates <- as.Date(ymd(rownames(ff_data)))
    assert_that(nrow(ff_data) == length(dates))
  }
  
  ff_data[] <- lapply(ff_data, as.numeric) # convert all columns to numeric
  xts(ff_data, order.by=dates) # return xts object
}

get_FF_1factor <- function(ff_url, ff_file, local_file=ff_file) {
  download.file(url=paste0(ff_url, ff_file), destfile=local_file)
  x <- unzip(zipfile=paste0("~/Documents/finance/", ff_file), list=TRUE)
  ff_data <- readLines(unz(ff_file, x$Name))
  i <- which(sapply(ff_data, nchar) == 0)
  vals <- lapply(ff_data[ seq(i[[2]]+1, i[[3]]-1) ], 
                 function(x) {
                   x <- unlist(strsplit(x, " "))
                   x[nchar(x)>0]})
  m <- do.call(rbind, vals[seq(2, length(vals))])
  dates <-  as.Date(paste(
    substr(m[ ,1],1,4), substr(m[ ,1],5,6), "01", sep="-"), "%Y-%m-%d")

  returns <- as.numeric(m[ ,2])
  retval <- xts(returns, order.by=dates)
  attr(retval, "dimnames") <- list(NULL, vals[[1]])
  retval
}
