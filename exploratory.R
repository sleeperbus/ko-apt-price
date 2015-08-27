library(dygraphs)
################################################################################
# dygraph 테스트
################################################################################
lungDeaths <- cbind(mdeaths, fdeaths)
str(lungDeaths)
dygraph(lungDeaths)
dygraph(lungDeaths) %>% dyRangeSelector()


################################################################################
# 인천서구 데이터로 놀이
################################################################################
library(plyr)
source("helpers.R")
apts = f_readLocalGugunData("t", "28260", 2010, 2015)
countByTime = count(apts, c("SALE_YEAR", "SALE_MONTH"))
z = ts(data = countByTime$freq, start = c(2006, 1), frequency = 12)
dygraph(z)
