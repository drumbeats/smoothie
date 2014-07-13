rankall<-function(dis,nn=1){
	options(warn=-1)
	o<- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	h<- read.csv("hospital-data.csv", colClasses = "character")
	oh<- merge(o, h, by = "Provider.Number")
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
		cstr=colnames(oh)[m]
		b1<-oh[,c("Hospital.Name.x","State.x",cstr)]
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
		b3["RANK"]<-NA
		stlist<-character()
		stcnt<-integer()
		dfhosp<-character()
		dfstate<-character()
		flag=0
		if (is.numeric(nn) | is.integer(nn)){
			flag=1
		}
		if (is.character(nn)){
			if (nn=="best"){
				flag=1
				nn=1
				##print("here")
				##print(nn)
				##print(class(nn))
			}
		}
		for (i in 1:NROW(b3)){
			ist=b3[i,2]
			if (is.element(ist,stlist)) {
				v1=match(ist,stlist)
				stcnt[v1]=stcnt[v1]+1
				sn=stcnt[v1]
			}
			else{
				stlist<-c(stlist,ist)
				stcnt<-c(stcnt,1)
				sn=1
			}
			b3[i,5]=sn
			if (flag==1) {
				if (nn==sn) {
					##print(b3[i, ])
					dfhosp<-c(dfhosp,b3[i,1])
					dfstate<-c(dfstate,ist)
				}
			}
		}
		if (flag==1) {
			nst=length(stlist)
			##print(nst)
			for (i in 1:nst){
				if (stcnt[i]<nn){
					dfhosp<-c(dfhosp,NA)
					dfstate<-c(dfstate,stlist[i])
					##print (stlist[i])
				}
			}
		}
		else{
			##print(stlist)
			##print(stcnt)
			for (i in 1:NROW(b3)){
				ist=b3[i,2]
				if (is.element(ist,stlist)) {
					v1=match(ist,stlist)
					if (b3[i,5]==stcnt[v1]){
						dfhosp<-c(dfhosp,b3[i,1])
						dfstate<-c(dfstate,ist)
					}
				}
			}
		}
		mydf=data.frame(hospital=dfhosp,state=dfstate)
		mydf<-mydf[order(mydf$state), ]
		##print(stlist)
		##print(stcnt)
		return(mydf)
	}
	else {
		stop("invalid outcome")
	}
}



rankhospital<-function(st,dis,nn){
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
					##print(head(b3,n=10))
					dd=b3[nn,1]
					print(dd)
					return (dd)
				}
				else {
					return("NA")
					print("NA")
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