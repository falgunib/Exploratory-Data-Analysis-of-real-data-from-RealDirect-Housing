library(ggplot2)
require(gdata)
bk <- read.xls("rollingsales_manhattan.xls",pattern="BOROUGH")
head(bk)
summary(bk)
bk$sale.price.n <- as.numeric(gsub("[^[:digit:]]","",bk$SALE.PRICE))
bk$units <- as.numeric(bk$total.units)
count(is.na(bk$sale.price.n))
names(bk) <- tolower(names(bk))

## clean/format the data with regular expressions
bk$gross.sqft <- as.numeric(gsub("[^[:digit:]]","",bk$gross.square.feet))
bk$land.sqft <- as.numeric(gsub("[^[:digit:]]","",bk$land.square.feet))
bk$sale.date <- as.Date(bk$sale.date)
bk$year.built <- as.numeric(as.character(bk$year.built))
## do a bit of exploration to make sure there's not anything ## weird going on with sale prices

bk.SP0 <- subset(bk,`zip.code`>0)
bk <- bk.SP0
attach(bk)
#zip code with units
ggplot(bk, aes(x=zip.code, fill=units)) + geom_histogram(binwidth=1)
#
bk.lowprice.places = subset(bk,`sale.price.n`<1e7)
hist(bk.lowprice.places$`sale.price.n`,col="green",breaks=50)
rug(bk.lowprice.places$`sale.price.n`) # adds "rug" underneath with data points
abline(v=median(bk.lowprice.places$`sale.price.n`),col='magenta',lwd=2) # adds vertical line at median location



t <- unique(bk$building.class.category)
axis(side = 1, at = t)
plot(bk$units,bk$zip.code) #units with zipcode
plot(bk$zip.code, bk$sale.price.n) #sale price with zipcode

barplot(table(bk$zip.code),col='blue',main="Zip Code")
barplot(table(bk$building.class.category),col='blue',main="Building class category")

ggplot(bk, aes(x=bk$zip.code, colour=bk$sale.price.n)) + geom_density() #zip/ sale price

#
hist(sale.price.n[sale.price.n<1e6])
hist(gross.sqft[sale.price.n<1e6])
detach(bk)
## keep only the actual sales
bk.sale <- bk[bk$sale.price.n!=0,]

plot(bk.sale$gross.sqft,bk.sale$sale.price.n)
plot(log(bk.sale$gross.sqft),log(bk.sale$sale.price.n))
## for now, let's look at 1-, 2-, and 3-family homes 
#zip code
plot(log(bk.sale$zip.code), log(bk.sale$sale.price.n))


bk.homes <- bk.sale[which(grepl("FAMILY",bk.sale$building.class.category)),]
plot(log(bk.homes$gross.sqft),log(bk.homes$sale.price.n))
bk.homes[which(bk.homes$sale.price.n<100000),] [order(bk.homes[which(bk.homes$sale.price.n<100000),] $sale.price.n),]
## remove outliers that seem like they weren't actual sales
bk.homes$outliers <- (log(bk.homes$sale.price.n) <=5) + 0
bk.homes <- bk.homes[which(bk.homes$outliers==0),]
plot(log(bk.homes$gross.sqft),log(bk.homes$sale.price.n))
