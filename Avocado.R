import <- function(){
  Avocado <- read.csv(system.file("extdata", "avocado.csv", package = "Avoprices"), sep=",")
  return(Avocado)
}
View(import)
attach(Avocado)
str(Avocado)
dim(Avocado)
sapply(Avocado,function(x)sum(is.na(x)))
Avocado<-Avocado[complete.cases(Avocado),]

library(tidyverse)
Avocado <- read_csv("avocado.csv")
as.data.frame(Avocado)
organic <- Avocado %>% select(Date, AveragePrice, type, "Total Volume") %>% filter(type == "organic")
conventional <- Avocado %>% select(Date, AveragePrice, type, "Total Volume") %>% filter(type == "conventional")

dplyr::group_by(organic, '1 month')
dplyr::group_by(conventional, '1 month')
install.packages("smooth")
library("smooth")

sma.conv <- sma(conventional$AveragePrice, h=10, silent=FALSE) + theme_economist()

sum(is.na(avocado))
sum(is.null(avocado))
colnames(Avocado)[4] <- "TotalVolume"


avocadoNY  <- select(filter(Avocado, region=='NewYork'),
                     c("Date","type","AveragePrice","TotalVolume","region"))


avoNYconv <- select(filter(avocadoNY,type=='conventional'), c("Date","type",
                                                              "AveragePrice",
                                                              "TotalVolume",
                                                              "region"))

avoNYorg <- select(filter(avocadoNY,type=='organic'), c("Date","type",
                                                        "AveragePrice",
                                                        "TotalVolume",
                                                        "region"))

ggplot(avocadoNY, aes(y=type, x='TotalVolume')) + geom_boxplot()



avoNYconv$year <- as.numeric(format(avoNYconv$Date, "%Y"))
avoNYconv$month <- as.numeric(format(avoNYconv$Date, "%m", label=TRUE))
avoNYconv15 <- avoNYconv[which(avoNYconv$year >= 2015), ]
conv <- ggplot(avoNYconv15,aes(x=month, y=AveragePrice))
conv <- conv + geom_line(aes(color=factor(year), group=year))
conv <- conv + scale_color_discrete(name="Year")
conv <- conv + scale_y_continuous(labels= comma)
conv <- conv + labs(title="Conventional-Price Changes", x="Month", y="Price")
conv

avoNYorg$year <- as.numeric(format(avoNYorg$Date, "%Y"))
avoNYorg$month <- as.numeric(format(avoNYorg$Date, "%m", label=TRUE))
avoNYorg15 <- avoNYorg[which(avoNYorg$year >= 2015), ]
org <- ggplot(avoNYorg15,aes(x=month, y=AveragePrice))
org <- org + geom_line(aes(color=factor(year), group=year))
org <- org + scale_color_discrete(name="Year")
org <- org + scale_y_continuous(labels= comma)
org <- org + labs(title="Conventional-Price Changes", x="Month", y="Price")
org


avoNYorg$year <- as.numeric(format(avoNYorg$Date, "%Y"))
avoNYorg$month <- as.numeric(format(avoNYorg$Date, "%m", label=TRUE))
avoNYorg15 <- avoNYorg[which(avoNYorg$year >= 2015), ]
org.changes <- ggplot(avoNYconv15,aes(x=month, y=TotalVolume))
org.changes <- org.changes + geom_line(aes(color=factor(year), group=year))
org.changes <- org.changes + scale_color_discrete(name="Year")
org.changes <- org.changes + scale_y_continuous(labels= comma)
org.changes <- org.changes + labs(title="Organic Quality Demand", x="Month", y="Quantity")
org.changes

avoNYconv$year <- as.numeric(format(avoNYconv$Date, "%Y"))
avoNYconv$month <- as.numeric(format(avoNYconv$Date, "%m", label=TRUE))
avoNYconv15 <- avoNYconv[which(avoNYconv$year >= 2015), ]
conv.changes <- ggplot(avoNYconv15,aes(x=month, y=TotalVolume))
conv.changes <- conv.changes + geom_line(aes(color=factor(year), group=year))
conv.changes <- conv.changes+ scale_color_discrete(name="Year")
conv.changes <- conv.changes + scale_y_continuous(labels=comma)
conv.changes<- conv.changes + labs(title="Conventional Quantity Demand", x="Month", y="Quantity")
conv.changes

avocado.org <- dplyr::select(filter(avocadoNY,type=='organic'), c(Date,type,
                                                                  AveragePrice,TotalVolume,region))
library("prophet")
colnames(avocado.org)[1] <- "ds"
colnames(avocado.org)[4] <- "y"

P1 <- prophet(avocado.org,weekly.seasonality=TRUE,daily.seasonality=TRUE)
future.org <- make_future_dataframe(P1, periods=365)
tail(future.org)
forecast.org <- predict(P1, future.org)
tail(forecast.org[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

plot(P1,forecast.org)


unique(Avocado$region)
Avocado$Month <- format(Avocado$Date, '%B') #create new variabe, month, in Avocado dataset
Avocado$Month <- factor(Avocado$Month,levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")) #rearrange Month variable
library(ggthemes)

NE.name <- c("Albany", "BaltimoreWashington", "Boston", "BuffaloRochester", "HarrisburgScranton", "HartfordSpringfield", "NewYork", "Northeast", "NorthernNewEngland", "Philadelphia", "Pittsburgh", "Syracuse")
MW.name <- c("Chicago", "CincinnatiDayton", "Columbus", "Detroit", "GrandRapids", "GreatLakes", "Indianapolis", "Plains", "StLouis")
S.name <- c("Atlanta", "Charlotte", "DallasFtWorth", "Houston", "Jacksonville", "Louisville", "MiamiFtLauderdale", "Midsouth", "Nashville", "NewOrleansMobile", "Orlando", "PhoenixTucson", "RaleighGreensboro", "RichmondNorfolk", "Roanoke", "SouthCarolina", "SouthCentral", "Southeast", "Tampa")
W.name <- c("Boise", "California", "Denver", "LasVegas", "LosAngeles", "Portland", "Sacramento", "SanDiego", "SanFrancisco", "Seattle", "Spokane", "WestTexNewMexico", "West")

Total.name <- c("TotalUS")
 region.list <- list(
     Northeast=NE.name,
     Midwest=MW.name,
     South=S.name,
     West=W.name,
     TotalUS=Total.name)

Avocado$USRegion <- lapply(Avocado$region,
                           function(x) names(region.list)[grep(x,region.list)])

Avocado$USRegion2 <- unlist(Avocado$USRegion)

names(Avocado) <- c("X1", "Date", "AveragePrice", "TotalVolume", "4046", "4225", "4770", "TotalBags", "SmallBags", "LargeBags", "XLargeBags", "type", "year", "region", "Month", "USRegion", "USRegion2")

library(tidyverse)
library(ggthemes)
ggplot(Avocado, aes(x=Date, y=AveragePrice, color=USRegion2)) + geom_smooth() + theme_economist() + scale_fill_economist() + labs(y="Average Price ($)", title="The Price of An Avocado by Region")

ggplot(filter(Avocado, USRegion2=="TotalUS"), aes(x=Month, y=AveragePrice, color=type)) + geom_boxplot() + facet_wrap(~USRegion2) + theme_economist() + scale_fill_economist() + theme(axis.text.x = element_text(angle = 90))



