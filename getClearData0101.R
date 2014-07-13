##  reads from url
## makes a data table
## counts rows which have a specific field value
a="https://d396qusza40orc.cloudfront.net/getdata/data/ss06hid.csv"
#download.file(a,"aaa.csv")
at<-read.csv("aaa.csv")
bq=at[!(is.na(at$VAL)),]
bqq=bq[bq$VAL==24,]
print(NROW(bqq))

## reads from xlsx
library(xlsx)
a="https://d396qusza40orc.cloudfront.net/getdata/data/DATA.gov_NGAP.xlsx"
#download.file(a,"a4.xlsx",mode="wb")
b<-read.xlsx("a4.xlsx",sheetIndex=1,colIndex=7:15,rowIndex=18:23)
c=sum(b$Zip*b$Ext,na.rm=T) 
print(c)

## reads from XML
library(XML)
a="https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
#download.file(a,"a5.xml",mode="wb")
b<-xmlTreeParse("a5.XML",useInternalNodes=TRUE)
r<-xmlRoot(b)
d=xpathSApply(r,"//zipcode",xmlValue)
c=d[d==21231]
print(NROW(c))

## using data.table
library(data.table)
a="https://d396qusza40orc.cloudfront.net/getdata/data/ss06pid.csv"
#download.file(a,"a6.csv",mode="wb")
b<-fread("a6.csv")
q1=b[,mean(pwgtp15),by=SEX]
q2=tapply(b$pwgtp15,b$SEX,mean)
q3=tapply(b$pwgtp15,b$SEX,NROW)
print(q1)
print(q2)
print(q3)

