corr <- function(directory, threshold = 0) {
	## 'directory' is a character vector of length 1 indicating
      ## the location of the CSV files

      ## 'threshold' is a numeric vector of length 1 indicating the
      ## number of completely observed observations (on all
      ## variables) required to compute the correlation between
      ## nitrate and sulfate; the default is 0
      ## Return a numeric vector of correlations

	p1=paste(getwd(),"/",directory, sep="")
	filenames <- list.files(p1, pattern="*.csv", full.names=TRUE)
	ov<-numeric()
	for (i in filenames) {
		data <- read.csv(i)
		q1=NROW(na.omit(data))
		if (q1>threshold) {
			d1=data[,c("sulfate","nitrate")]
			d2=cor(d1,use="complete.obs")
			ov<-c(ov,d2[1,2])
		}

	}
	return(ov)
}