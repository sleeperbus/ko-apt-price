library(ggvis)
source("helpers.R")
sido = readRDS("data/sido.rds")
gugun = readRDS("data/gugun.rds")
dong = readRDS("data/dong.rds")

# 인천 서구 거래동향
sampleGugunCode = "28260"
sampleGugun = data.frame()
for (year in 2006:2015) {
  fileName = paste0("data/", sampleGugunCode, "_", year, ".rds")
  tmpDf = data.frame()
  tmpDf = readRDS(fileName)
  sampleGugun = rbind(sampleGugun, tmpDf) 
}
str(sampleGugun)
sampleGugun %>%
  ggvis(x=~SALE_DATE) %>% layer_histograms(fill:="red", opacity:=0.7)

# function factoring test
tradeDong = f_getTrade(dong$dongCode[1], 2015, 3)
rentDong = f_getRent(dong$dongCode[1], 2015, 3)
str(rentDong)

# 데이터 가져오기
yearData = f_dongYearData(dong$dongCode[1], 2015, 2015, f_getRent)

# 툴팁 
testDong = dong[dong$gugunCode == "45140", ]
testDong = testDong[with(testDong, order(dongCode)),]
codes = c("시도코드", "구군코드", "동코드", "동이름")
f_1 = function(x) {
  paste0(codes, ": ", format(x), collapse="<br />")
}

x = ddply(testDong, .(dongCode), f_1)
testDong$TOOLTIP = x$V1

paste0(codes, ": ", format(testDong[1,]), collapse = "<br />")
f_1(testDong[1,])

# 데이터 변환 SUM_AMT => TRADE_AMT 
testGugun = readRDS("data/11110_2006.rds")
testGugun$TRADE_AMT = testGugun$SUM_AMT
testGugun$SUM_AMT = NULL
names(testGugun)

dataPath = "data"
for (fileName in dir(dataPath)) {
  if (fileName != "sido.rds" & fileName != "gugun.rds" & fileName != "dong.rds") {
    filePath = file.path(dataPath, fileName)  
    df = readRDS(filePath)
    df$TRADE_AMT = df$SUM_AMT
    df$SUM_AMT = NULL
    saveRDS(df, filePath)
  }
}

# 데이터 뒤지기
files = dir("data", pattern = "t_11680")
fileNames = sapply(files, function(file) file.path("data", file))
result = lapply(fileNames, function(file) readRDS(file))
result = do.call("rbind", result)


