count <- function(cause=NULL){
	if (is.null(cause)){
		stop("Cause not mentioned")
	}
	cause=tolower(cause)
	al=c("asphyxiation", "blunt force", "other", "shooting","stabbing", "unknown")
	if (cause %in% al) {
		h <- readLines("homicides.txt")
		acnt=0
		for (i in 1:NROW(h)){
			v1=h[i]
			v2=regexpr("Cause",v1)
			#print(v1)
			#print(as.integer(v2))
			if (v2!=-1){
				v3=TRUE
				v4=v2+7
				v6=0
				while(v3){
					v5=substring(v1,v4,v4)
					#print(v5)
					if (v5==""){
						v3=FALSE
					}
					else {
						if(v5=="<"){
							v6=v4
							v3=FALSE
						}
						else {
							v4=v4+1
						}
					}
				}
				#print (v6)
				if (v6>0){
					spc=tolower(substring(v1,v2+7,v6-1))
					qc=tolower(cause)
					if (spc==qc){
						acnt=acnt+1
					}
				}
			}
		}
		print(acnt)
		#return(acnt)
	}
	else {
		stop("Invalid Cause")
	}
}