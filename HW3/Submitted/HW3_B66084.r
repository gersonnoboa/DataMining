
library(ggplot2)
klient1 = read.table('klient1.txt', header = FALSE)
klient3 = read.table('klient3.txt', header = FALSE)

den1 = density(klient1$V1, kernel = "triangular", adjust = 2)
plot(den1, col="red", xlab="Time since Monday", main="Comparison of clients")

den3 = density(klient3$V1, kernel="triangular", adjust = 2)
lines(den3, col="blue")
legend('topright', c("Klient1", "Klient3") , lty=1, col=c('red', 'blue'), bty='n', cex=.75)

#ex2  
klient1friday = subset(klient1, V1 >= 96 & V1 < 120)
klient1saturday = subset(klient1, V1 >= 120 & V1 < 144)

klient3friday = subset(klient3, V1 >= 96 & V1 < 120)
klient3saturday = subset(klient3, V1 >= 120 & V1 < 144)

den1friday = density(klient1friday$V1, na.rm = TRUE)
den1saturday = density(klient1saturday$V1, na.rm = TRUE)
den3friday = density(klient3friday$V1, na.rm = TRUE)
den3saturday= density(klient3saturday$V1, na.rm = TRUE)

plot(den1friday, col="red", type='l', xlim=range(c(100, 145)), ylim = range(c(0.00, 0.35)), main = "Shopping on Friday and Saturday", xlab = "Hours since Monday")
lines(den1saturday, col="blue", type='l')
lines(den3friday, col="green")
lines(den3saturday, col="orange")
legend('topright', c("Klient1 Friday", "Klient1 Saturday", "Klient3 Friday", "Klient3 Saturday") , lty=1, col=c('red', 'blue', 'green', 'orange'), bty='n', cex=.75)

ggplot(data=klient1friday, aes(sample=V1)) +
  geom_qq(fill="blue", color="blue") +
  geom_qq(data=klient3friday, aes(sample=V1), color="red") +
  labs("title" = "Comparison between Klient1 (red) and Klient3 (blue) on a Friday", y="Hours", x = "Quantiles")
  

#ex3
product = read.table('product_time_shop.txt', header = TRUE, stringsAsFactors = F, sep = ";")
productdf = data.frame(product)

setNames(aggregate(shop_id ~ product, data = productdf, FUN = length), c("Product on all shops", "Amount"))

subset18 <- subset(product, shop_id == 18)
setNames(aggregate(shop_id ~ product, data = subset18, FUN = length), c("Product on Shop 18", "Amount"))

subset21 <- subset(product, shop_id == 21)
setNames(aggregate(shop_id ~ product, data = subset21, FUN = length), c("Product on Shop 21", "Amount"))

subset3 <- subset(product, shop_id == 3)
setNames(aggregate(shop_id ~ product, data = subset3, FUN = length), c("Product on Shop 3", "Amount"))

subset32 <- subset(product, shop_id == 32)
setNames(aggregate(shop_id ~ product, data = subset32, FUN = length), c("Product on Shop 32", "Amount"))

subset4 <- subset(product, shop_id == 4)
setNames(aggregate(shop_id ~ product, data = subset4, FUN = length), c("Product on Shop 4", "Amount"))

txt=product$date
cmb_date=paste(substr(txt,1,4),'-',substr(txt,5,6),'-',substr(txt,7,8),sep='')
mydates <- as.Date(cmb_date)
product["weekdays"] <- weekdays(mydates)
product["months"] <- months(mydates)
product["days"] <- as.numeric(format(mydates,"%d"))
product["doy"] <- as.numeric(format(mydates,"%j"))
product["year"] <- as.numeric(format(mydates,"%Y"))

print("Dates covered")
unique(mydates)

print("Years covered")
unique(product$year)

print("Months covered")
unique(product$months)

#weekdays
#time vs days
ggplot(product, aes(x=weekdays, y=time, color=weekdays)) + 
  geom_violin() + 
  geom_boxplot(width=.1) +
  labs(y="Time", x = "Days of the week", title = "Comparison between time of purchase and weekdays")

bananaSubset = subset(product, product == "Banana")
ggplot(bananaSubset, aes(x=weekdays, y=time, color=weekdays)) + 
  geom_violin() + 
  geom_boxplot(width=.1) +
  labs(y="Time", x = "Days of the week", title = "Comparison between time of purchase of bananas and weekdays")

#products
#time of day vs products on sunday
sundaySubset = subset(product, weekdays != "domingo")

ggplot(sundaySubset, aes(x=product, y=time, color=product)) + 
  geom_violin() + 
  geom_boxplot(width=.1) +
  labs(y="Time", x = "Products", title = "Comparison between time of purchase and products on Sunday") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
  

ggplot(bananaSubset, aes(x=months, y=time, color=months)) + 
  geom_violin() + 
  geom_boxplot(width=.1) +
  labs(y="Time", x = "Months", title = "Bananas through the months")

vastlakukkelSubset = subset(product, product == "Vastlakukkel")
ggplot(vastlakukkelSubset, aes(x=months, y=time, color=months)) + 
  geom_violin() + 
  geom_boxplot(width=.1) +
  labs(y="Time", x = "Months", title = "Vastlakukkel through the months")

ggplot(product, aes(x=product, y=doy, color=product)) + 
  geom_violin() + 
  #geom_boxplot(width=.1) +
  labs(y="Day of the year", x = "Months", title = "Products bought throughout the year") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

#shops
product["shop_id"] <- lapply(product["shop_id"], as.character)
ggplot(product, aes(x=shop_id, y=time, group=shop_id, color=shop_id)) + 
  geom_violin() +
  geom_boxplot(width=.1) +
  labs(y="Time", x = "Store", title = "Time of purchase in store")

ggplot(product, aes(x=shop_id, y=doy, group=shop_id, color=shop_id)) + 
  geom_violin() +
  geom_boxplot(width=.1) +
  labs(y="Day of the year", x = "Store", title = "Days of purchase in all stores")

subset18 = subset(product, year == "2014" & shop_id == 18)
ggplot(subset18, aes(x=product, y=doy, group=product, color=product)) + 
  geom_violin() +
  labs(y="Day of the year", x = "Product", title = "Products purchased in store 18, analyzed by day of the year") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

#ex 5
ggplot(subset18, aes(x=product, y=doy, group=product, color=product)) + 
  geom_violin() +
  labs(y="Day of the year", x = "Product", title = "Products purchased in store 18, analyzed by day of the year") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

print("Bananas sold")
subsetbanana2014 = subset(bananaSubset, year == 2014)
setNames(aggregate(product ~ doy, data = subsetbanana2014, FUN = length), c("Day of year", "Bananas sold"))

print("Milk type 1 sold")
subsetmilk2014 = subset(product, product == "Milk_1")
setNames(aggregate(product ~ doy, data = subsetmilk2014, FUN = length), c("Day of year", "Milk type 1 sold"))

print("Vastlakukkel sold")
subsetvastlakukkel = subset(product, product == "Vastlakukkel" & year == 2014)
setNames(aggregate(product ~ doy, data = subsetvastlakukkel, FUN = length), c("Day of year", "Vastlakukkel sold"))
