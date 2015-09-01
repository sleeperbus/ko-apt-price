library(dygraphs)
################################################################################
# dygraph 테스트
################################################################################
lungDeaths <- cbind(mdeaths, fdeaths)
str(lungDeaths)
dygraph(lungDeaths)
dygraph(lungDeaths) %>% dyRangeSelector()


################################################################################
# 서구 28260
# 계양구 28245
# 김포시 41570
################################################################################
library(plyr)
library(RColorBrewer)
source("helpers.R")
region_1 = f_readLocalGugunData("t", "28260", 2006, 2015)
region_2 = f_readLocalGugunData("t", "28245", 2006, 2015)
region_3 = f_readLocalGugunData("t", "41570", 2006, 2015)
count_1 = count(region_1, c("SALE_YEAR", "SALE_MONTH"))
count_2 = count(region_2, c("SALE_YEAR", "SALE_MONTH"))
count_3 = count(region_3, c("SALE_YEAR", "SALE_MONTH"))
x = ts(data = count_1$freq, start = c(2006, 1), frequency = 12)
y = ts(data = count_2$freq, start = c(2006, 1), frequency = 12) 
z = ts(data = count_3$freq, start = c(2006, 1), frequency = 12) 
data = cbind(x, y, z)

dygraph(data) %>% dyRangeSelector() %>%
  dyOptions(colors = RColorBrwer::brewer.pal(3, "Set1"))

dygraph(data, main = "APT Prices") %>%
  dyHighlight(highlightCircleSize = 5, 
              highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)

################################################################################
# 전세가 대비 매매가
################################################################################
library(lubridate)
trade = f_readLocalGugunData("t", "28260", 2006, 2015)
rent = f_readLocalGugunData("r", "28260", 2006, 2015)


