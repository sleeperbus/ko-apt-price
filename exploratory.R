library(dygraphs)
################################################################################
# dygraph 테스트
################################################################################
str(mdeaths)
lungDeaths <- cbind(mdeaths, fdeaths)
str(lungDeaths)
dygraph(lungDeaths)

dygraph(lungDeaths) %>% dyRangeSelector()


