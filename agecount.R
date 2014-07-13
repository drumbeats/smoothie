agecount <- function(age=NULL){
	if (is.null(age)){
		print(is.null(age))
		stop("Age not mentioned")
	}
	h <- readLines("homicides.txt")
	acnt=0
	for (i in 1:NROW(h)){
		v1=h[i]
		v2=regexpr("years",v1)
		if (v2!=-1){
			v3=as.integer(substring(v1,v2-3,v2-2))
			if (v3==age){acnt=acnt+1}
		}
	}
	print(acnt)
	return(acnt)
}