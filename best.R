best<-function(st,dis){
	options(warn=-1)
	o<- read.csv("f:\\bestrp\\outcome-of-care-measures.csv", colClasses = "character")
	h<- read.csv("f:\\rp\\hospital-data.csv", colClasses = "character")
	oh<- merge(o, h, by = "Provider.Number")
	## print(NROW(oh))
	a<-subset(oh,State.x==st)
	a1<-NROW(a)
	##txt1="Error in best("
	##txt2=", "
	if (a1==0){
		##txt3=") : invalid state"
		##options(useFancyQuotes = FALSE)
		##message(txt1,dQuote(st),txt2,dQuote(dis),txt3)
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
			death<-as.numeric(a[,m])
      		b<-min(death,na.rm=TRUE)
			c<-subset(a,death==b)
			if(NROW(c)>1){
				##message("Multi Min ", NROW(c))
				d<-c[order(2),]
				d[1,2]
			}
			else {
				c[1,2]
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