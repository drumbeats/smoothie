p22<-function(st,dis,nn){
	options(warn=-1)
	o<- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	h<- read.csv("hospital-data.csv", colClasses = "character")
	oh<- merge(o, h, by = "Provider.Number")
	a<-subset(oh,State.x==st)
	a1<-NROW(a)
	if (a1==0){
		stop("invalid state")
	}
	else {
		n<-0
		if (dis=="heart attack"){
			m<-11
			n<-1
		}
		if (dis=="heart failure"){
			m<-17
			n<-1
		}
		if (dis=="pneumonia"){
			m<-23
			n<-1
		}
		if (n==1){
			cstr=colnames(a)[m]
			b1<-a[,c("Hospital.Name.x","State.x",cstr)]
			b1["NC"]<-NA
			if (dis=="heart attack"){
				b1$NC=as.numeric(b1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
			}
			if (dis=="heart failure"){
				b1$NC=as.numeric(b1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
			}
			if (dis=="pneumonia"){
				b1$NC=as.numeric(b1$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
			}
			b2<-b1[order(b1$NC,b1$Hospital.Name.x), ]
			b3=b2[!is.na(b2$NC),]
			##print(head(b2))
			if (is.numeric(nn) | is.integer(nn)){
				if (nn <= NROW(b3)){
					##print("here")
					##print(head(b3))
					print(b3[nn,1])
				}
				else {
					print(NA)
				}
			}
			if (is.character(nn)) {
				if (nn=="best"){
					print(b3[1,1])
				}
				if (nn=="worst"){
					print(b3[NROW(b3),1])
				}	
			}
		}
		else {
			##txt3=") : invalid outcome"
			##options(useFancyQuotes = FALSE)
			##message(txt1,dQuote(st),txt2,dQuote(dis),txt3)
			stop("invalid outcome")
		}
	}
}