################################################################################
# 서구 28260 데이터 분석
################################################################################
library(dygraphs)
library(plyr)
library(RColorBrewer)
library(lubridate)
library(ggvis)
source("helpers.R")

region = f_readLocalGugunData("t", "28260", 2006, 2015)

# 컬럼 추가
region$SALE_DATE = floor_date(region$SALE_DATE, "month")
region$UNIT_PRICE = with(region, round(TRADE_AMT/AREA))

# 거래량
volume = count(region, c("SALE_YEAR", "SALE_MONTH")) 
ts.volume = ts(data = volume$freq, start = c(2006, 1), frequency = 12)

# 매매물건의 m2당 가격변화
unitprice = ddply(region, .(SALE_DATE), summarise, price = mean(UNIT_PRICE))  
ts.unitprice = ts(data = unitprice$price, start = c(2006, 1), frequency = 12)

# 평균단가와 거래량 간의 상관관계
data = cbind(ts.volume, ts.unitprice) 
dygraph(data) %>% dyRangeSelector()

# 스케일링된 데이터를 비교
scaled.volume = transform(volume, freq = scale(freq))
scaled.unitprice = transform(unitprice, price = scale(price))
ts.scaled.volume = ts(data = scaled.volume$freq, start = c(2006, 1), frequency = 12)
ts.scaled.unitprice = ts(data = scaled.unitprice$price, start = c(2006, 1), frequency = 12)
scaled.data = cbind(ts.scaled.volume, ts.scaled.unitprice)
dygraph(scaled.data) %>% dyRangeSelector()

# 2010년도 데이터만 추출해서 unitprice 의 histogram 
region.2010 = subset(region, SALE_YEAR == 2010)
ggvis(data=region.2010, x=~UNIT_PRICE) %>%
  layer_histograms() 
ggvis(data=region.2010, x=~UNIT_PRICE) %>%
  layer_densities() 

# 2014년도 데이터만 추출해서 unitprice 의 histogram 
region.2014 = subset(region, SALE_YEAR == 2014)
ggvis(data=region.2014, x=~UNIT_PRICE) %>%
  layer_histograms() 
ggvis(data=region.2014, x=~UNIT_PRICE, fill=~DONG_CODE) %>%
  group_by(DONG_CODE) %>%
  layer_histograms(opacity := 0.4) 

################################################################################
# 전세가 대비 매매가
################################################################################
library(lubridate)
library(reshape2)
trade = f_readLocalGugunData("t", "28260", 2006, 2015)
rent = f_readLocalGugunData("r", "28260", 2006, 2015)
trade$TYPE = "TRADE"
rent$TYPE = "RENT"
trade = trade[, c("TYPE", "APT_CODE", "SALE_MONTH", "SALE_DAYS", "AREA", "DONG_CODE",
                  "SALE_YEAR",  "SALE_DATE", "REAL_AREA", "TRADE_AMT")]
rent = subset(rent, MONTHLY == 0)
rent = rent[, c("TYPE", "APT_CODE", "SALE_MONTH", "SALE_DAYS", "AREA", "DONG_CODE",
                "SALE_YEAR",  "SALE_DATE", "REAL_AREA", "TRADE_AMT")]

# 월별데이터로 변형한다.
trade$MONTH = floor_date(trade$SALE_DATE, "month")
rent$MONTH = floor_date(rent$SALE_DATE, "month")

# 동별 데이터로 바꾼다.
by.dong = count(trade, vars = c("DONG_CODE", "MONTH"))
by.dong = dcast(by.dong, MONTH ~ DONG_CODE, mean, na.rm = T, value.var = "freq")
by.dong$MONTH = NULL
ts.dong = ts(as.matrix(by.dong), start = c(2006, 1), frequency = 12)
dygraph(ts.dong) %>% dyRangeSelector() %>%
  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"))

# 아파트, 월별, 전용면적별로 평균가격을 구한다.
avg.trade = ddply(trade, .(APT_CODE, AREA, MONTH), summarise, mean(TRADE_AMT))
str(avg.trade)
head(avg.trade)

