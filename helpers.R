library(jsonlite)
library(stringr)
library(log4r)

sidos = readRDS("data/sido.rds")
guguns = readRDS("data/gugun.rds")
dongs = readRDS("data/dong.rds")

logFileName = file.path(getwd(), paste0("log_", format(Sys.Date(), "%Y%m%d"), ".log"))
logger = create.logger()
logfile(logger) = logFileName
level(logger) = "INFO"

savePath = file.path(getwd(), "data")

# 국토부 실거래가 사이트에 접속해서 데이터를 가져온다.
# qryType t 매매, r 전월세
f_readUrl = function(
  qryType, dongCode, year, period,
  warning = function(w) {
    warn(logger, w)
    invokeRestart("updateTryCount")
  },
  error = function(e) {
    error(logger, e)
    invokeRestart("updateTryCount")
  } 
) {
  tryCount = 1 
  while (tryCount <= 10) {
    withRestarts(
      tryCatch(
{
  msg = paste0("tring to read, ", dongCode, "-", year, "-", period)  
  debug(logger, msg)
  url = paste0("http://rt.molit.go.kr/rtApt.do?", 
               "cmd=get", qryType, "AptLocal&dongCode=", 
               dongCode, "&danjiCode=ALL&srhYear=", year,
               "&srhPeriod=", period, "&gubunRadio2=1") 
  rawData = readLines(url, encoding="UTF-8")           
  data = fromJSON(rawData) 
  return(data)
},
warning = warning,
error = error
      ),
updateTryCount = function() {
  msg = paste(qryType, dongCode, year, period, sep = "-")
  msg = paste0(msg, ", at tryCount: ", tryCount)
  info(logger, msg)  
  tryCount <<- tryCount + 1
}
    )
  }
# tryCount 가 3보다 커지면 그냥 NULL 을 반환
return(NULL)
}

# 아파트 정보에 추가적인 정보를 생성한다.
f_addInfo = function(apts) {
  apts$SALE_MONTH = str_pad(apts$SALE_MONTH, 2, pad="0")
  apts$SALE_DAYS= str_pad(apts$SALE_DAYS, 2, pad="0")
  apts$SALE_DATE = do.call(paste0, apts[,c("SALE_YEAR", "SALE_MONTH", "SALE_DAYS")])
  apts$SALE_DATE = strptime(apts$SALE_DATE, "%Y%m%d")
  apts$SALE_DATE = as.Date(apts$SALE_DATE)
  apts$AREA = round(as.numeric(apts$AREA))
  apts = apts[with(apts, order(SALE_DATE)),]
  
  apts$REAL_AREA = -1
  apts$REAL_AREA_DESC = ""  
  apts[which(apts$AREA <= 35), c("REAL_AREA")] = 0
  apts[which(apts$AREA <= 35), c("REAL_AREA_DESC")] = "(~35)"
  apts[which(apts$AREA >= 36 & apts$AREA <= 40), c("REAL_AREA")] = 1
  apts[which(apts$AREA >= 36 & apts$AREA <= 40), c("REAL_AREA_DESC")] = "(36~40)"
  apts[which(apts$AREA >= 41 & apts$AREA <= 50), c("REAL_AREA")] = 2
  apts[which(apts$AREA >= 41 & apts$AREA <= 50), c("REAL_AREA_DESC")] = "(41~50)"
  apts[which(apts$AREA >= 51 & apts$AREA <= 60), c("REAL_AREA")] = 3
  apts[which(apts$AREA >= 51 & apts$AREA <= 60), c("REAL_AREA_DESC")] = "(51~60)"
  apts[which(apts$AREA >= 61 & apts$AREA <= 70), c("REAL_AREA")] = 4
  apts[which(apts$AREA >= 61 & apts$AREA <= 70), c("REAL_AREA_DESC")] = "(61~70)"
  apts[which(apts$AREA >= 71 & apts$AREA <= 80), c("REAL_AREA")] = 5
  apts[which(apts$AREA >= 71 & apts$AREA <= 80), c("REAL_AREA_DESC")] = "(71~80)"
  apts[which(apts$AREA >= 81 & apts$AREA <= 90), c("REAL_AREA")] = 6
  apts[which(apts$AREA >= 81 & apts$AREA <= 90), c("REAL_AREA_DESC")] = "(81~90)"
  apts[which(apts$AREA >= 91 & apts$AREA <= 100), c("REAL_AREA")]  = 7
  apts[which(apts$AREA >= 91 & apts$AREA <= 100), c("REAL_AREA_DESC")] = "(91~100)"
  apts[which(apts$AREA >= 101), c("REAL_AREA")]  = 8
  apts[which(apts$AREA >= 101), c("REAL_AREA_DESC")] = "(101~)"
  
  apts$APT_NAME = factor(apts$APT_NAME)
  apts$GROUP = do.call(paste0, list(apts$APT_NAME, apts$REAL_AREA_DESC))
  apts$GROUP = factor(apts$GROUP)
  return (apts)  
}

# 전월세 데이터를 가져온다.
f_getRent = function(dongCode, year, period) {
  data = f_readUrl("Rent", dongCode, year, period)		 
  if (is.null(data)) return(NULL)
  aptInfo = as.data.frame(data[1])
  prices = as.data.frame(data[2]) 
  apts = data.frame()
  if (nrow(prices) > 0) {
    names(aptInfo) = c("APT_NAME", "AREA_CNT", "APT_CODE", "BORM", 
                       "BUILD_YEAR", "BUBN")
    aptInfo$DONG_CODE = dongCode
    names(prices) = c("SALE_MONTH", "SALE_DAYS", "APT_CODE", "FLOOR", "MONTHLY",
                      "AREA", "TRADE_AMT")
    apts = merge(aptInfo, prices, by="APT_CODE") 
    apts$SALE_YEAR = year
  } else {
    debug(logger, paste(paste(dongCode, year, period, sep = "-"), ": no prices"))
    return(NULL)
  }          	
  
  apts = f_addInfo(apts)
  
  apts$TRADE_AMT= as.numeric(gsub(",", "", apts$TRADE_AMT))
  apts$MONTHLY = as.numeric(gsub(",", "", apts$MONTHLY))
  apts$TOOL_TIP = ""  
  apts$TOOL_TIP = with(apts, paste(APT_NAME, paste0(AREA, "m2"), TRADE_AMT,  
                                   SALE_DATE, sep="/"))  
  return(apts)  
}

# 매매 데이터를 가져온다.
f_getTrade = function(dongCode, year, period) {
  data = f_readUrl("Trade", dongCode, year, period)		 
  if (is.null(data)) return(NULL)
  
  aptInfo = as.data.frame(data[1])
  prices = as.data.frame(data[2]) 
  apts = data.frame()
  if (nrow(prices) > 0) {
    names(aptInfo) = c("APT_NAME", "AREA_CNT", "APT_CODE", "BORM", 
                       "BUILD_YEAR", "BUBN")
    aptInfo$DONG_CODE = dongCode
    names(prices) = c("SALE_MONTH", "TRADE_AMT", "SALE_DAYS", "APT_CODE", 
                      "FLOOR", "AREA")
    apts = merge(aptInfo, prices, by="APT_CODE") 
    apts$SALE_YEAR = year
  } else {
    debug(logger, paste(paste(dongCode, year, period, sep = "-"), ": no prices"))
    return(NULL)
  }          	
  apts = f_addInfo(apts)
  
  apts$TRADE_AMT = as.numeric(gsub(",", "", apts$TRADE_AMT))
  apts$TOOL_TIP = ""  
  apts$TOOL_TIP = with(apts, paste(APT_NAME, paste0(AREA, "m2"), TRADE_AMT, 
                                   SALE_DATE, sep="/")) 
  
  return (apts) 
}

# 특정 동코드의 연도별 데이터를 생성한다.
f_dongYearData = function(dongCode, from, to, f_name) {
  apts = data.frame()
  for (srhYear in from:to) {
    for (srhPeriod in 1:4) {
      tempApts = f_name(dongCode, srhYear, srhPeriod)		 
      apts = rbind(apts, tempApts)
    }
  }  
  return(apts) 
}

# 조회된 데이터를 파일로 저장
f_dongToFile = function(dongCode, from, to, f_name) {
  for (srhYear in from:to) {
    apts = data.frame()
    for (srhPeriod in 1:4) {
      tempApts = f_name(dongCode, srhYear, srhPeriod)		 
      apts = rbind(apts, tempApts) 
    }
    fileName = paste(paste(dongCode, srhYear, sep="_"), "rds", sep=".")
    saveRDS(apts, paste("data", fileName, sep="/"))
    msg = paste("successfully write to", fileName)
    debug(logger, msg)
  }  
}

# 대량의 데이터를 가져온다.
f_crawler = function(fromYear, toYear, prefix, f_name) { 
  msg = paste(fromYear, "~" , toYear, prefix, "started", sep = " ")
  info(logger, msg)
  for (curGugunCode in guguns[114:nrow(guguns),2]) { 
    startTime = Sys.time()
    dongCodes = data.frame()
    result = data.frame()
    curDongs = subset(dongs, gugunCode == curGugunCode)
    for (year in fromYear:toYear) {
      result = apply(as.data.frame(curDongs[,3]), 1, f_dongYearData, year, 
                     year, f_name)
      result = do.call("rbind", result)
      fileName = paste(paste(prefix, curGugunCode, year, sep="_"), "rds", sep=".")
      saveRDS(result, file.path(savePath, fileName))
    }
    endTime = Sys.time()
    
    gugunIdx = which(curGugunCode == guguns$gugunCode)
    pct = round((gugunIdx / nrow(guguns)) * 100, 2)
    
    timeTakes = difftime(endTime, startTime)
    remainTimes = (nrow(guguns) - gugunIdx) * timeTakes
    
    msgTimeTakes = round(as.numeric(timeTakes, units = "mins"))
    msg = paste(curGugunCode, "takes", msgTimeTakes, "mins")
    message(msg)
    info(logger, msg)
    
    msgRemainTimes = round(as.numeric(remainTimes, units = "hours"), digits = 2)
    msg = paste("Times remains:", msgRemainTimes, "hours")
    message(msg)
    info(logger, msg)
    
    message(paste0(pct, "% done.")) 
    info(logger, paste0(pct, "% done.")) 
  } 
  msg = paste(from, "~" , to, prefix, "ended", sep = " ")
  info(logger, msg)
}
