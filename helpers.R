################################################################################
# 국토교통부에 접속해서 데이터를 가져오는 함수 모음
################################################################################

library(jsonlite)
library(stringr)
library(log4r)
library(plyr)
library(httr)

sidos = readRDS("data/sido.rds")
guguns = readRDS("data/gugun.rds")
dongs = readRDS("data/dong.rds")

# 국토교통부 post request header
url = "http://rt.molit.go.kr/srh/getListAjax.do"
headers = add_headers(
  Accept="application/json, text/javascript, */*; q=0.01",
  Origin="http://rt.molit.go.kr", 
  "X-Requested-With"="XMLHttpRequest",
  "User-Agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.106 Safari/537.36",
  "Content-Type"="application/x-www-form-urlencoded; charset=UTF-8",
  "Referer"="http://rt.molit.go.kr/srh/srh.do?menuGubun=A&srhType=TOT&houseType=1&gubunCode=LAND",
  "Accept-Encoding"="gzip, deflate",
  "Accept-Language"="ko,en-US;q=0.8,en;q=0.6"
)


# 전역 로거
logFileName = file.path(getwd(), paste0("log_", format(Sys.Date(), "%Y%m%d"), ".log"))
logger = create.logger()
logfile(logger) = logFileName
level(logger) = "INFO"

savePath = file.path(getwd(), "data")

#-------------------------------------------------------------------------------
# 국토부 실거래가 사이트에 접속해서 데이터를 가져온다.
# - qryType Trade 매매, Rent 전월세
# - 가끔씩 request 에러가 발생하므로 10 정도는 retry 해준다.
# [수정사항]
# 2016.01.13  인터페이스가 변경됨
#------------------------------------------------------------------------------- 
f_readUrl = function(
  qryType, sidoCode, gugunCode, dongCode, year, period,
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
  houseType = 0
  tradeType = ""
  
  if (qryType == "t") {
    houseType = 1
    tradeType = "Trade"
  } else {
    houseType = 2
    tradeType = "Rent"
  } 
  while (tryCount <= 10) {
    withRestarts(
      tryCatch(
        {
          msg = paste0("tring to read, ", dongCode, "-", year, "-", period)  
          
          debug(logger, msg)
          body = list(areaCode="", chosung="", danjiCode="", dongCode=dongCode,
                      fromAmt1="", fromAmt2="", fromAmt3="", gubunCode="LAND",
                      gugunCode=gugunCode, houseType=houseType, jimokCode="", menuGubun="A",
                      rentAmtType="3", reqPage="SRH", roadCode="", sidoCode=sidoCode,
                      srhPeriod=period, srhType="TOT", srhYear=year, toAmt1="",
                      toAmt3="", toAmt3="", useCode="", useSubCode="")          
          r = POST(url, headers, body=body, encode="form") 
          rawdata = content(r, "text")
          data = fromJSON(rawdata)
          
          # list 의 첫번째 원소가 data.frame 이다. 
          # 반환되어 온 자료가 없으면 NULL 을 반환한다.
          data = data[[1]]
          if (length(data) > 0) return(data)
          else return(NULL)
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

#-------------------------------------------------------------------------------
# 아파트 정보에 추가적인 정보를 생성한다.
#-------------------------------------------------------------------------------
f_addInfo = function(apts, dongCode) {
  sidoCode = substr(dongCode, 1, 2)
  gugunCode = substr(dongCode, 1, 5)
  apts$SIDO_CODE = sidoCode 
  apts$GUGUN_CODE = gugunCode
  apts$DONG_CODE = dongCode
  apts$DEAL_MM = str_pad(apts$DEAL_MM, 2, pad="0")
  apts$DEAL_DD= str_pad(apts$DEAL_DD, 2, pad="0")
  apts$DEAL_DATE = do.call(paste0, apts[,c("DEAL_YYYY", "DEAL_MM", "DEAL_DD")])
  apts$DEAL_DATE = strptime(apts$DEAL_DATE, "%Y%m%d")
  apts$DEAL_DATE = as.Date(apts$DEAL_DATE)
  apts$BLDG_AREA = round(as.numeric(apts$BLDG_AREA))
  apts = apts[with(apts, order(DEAL_DATE)),]
  
  apts$REAL_AREA = -1
  apts$REAL_AREA_DESC = ""  
  apts[which(apts$BLDG_AREA <= 35), c("REAL_AREA")] = 0
  apts[which(apts$BLDG_AREA <= 35), c("REAL_AREA_DESC")] = "(~35)"
  apts[which(apts$BLDG_AREA >= 36 & apts$BLDG_AREA <= 40), c("REAL_AREA")] = 1
  apts[which(apts$BLDG_AREA >= 36 & apts$BLDG_AREA <= 40), c("REAL_AREA_DESC")] = "(36~40)"
  apts[which(apts$BLDG_AREA >= 41 & apts$BLDG_AREA <= 50), c("REAL_AREA")] = 2
  apts[which(apts$BLDG_AREA >= 41 & apts$BLDG_AREA <= 50), c("REAL_AREA_DESC")] = "(41~50)"
  apts[which(apts$BLDG_AREA >= 51 & apts$BLDG_AREA <= 60), c("REAL_AREA")] = 3
  apts[which(apts$BLDG_AREA >= 51 & apts$BLDG_AREA <= 60), c("REAL_AREA_DESC")] = "(51~60)"
  apts[which(apts$BLDG_AREA >= 61 & apts$BLDG_AREA <= 70), c("REAL_AREA")] = 4
  apts[which(apts$BLDG_AREA >= 61 & apts$BLDG_AREA <= 70), c("REAL_AREA_DESC")] = "(61~70)"
  apts[which(apts$BLDG_AREA >= 71 & apts$BLDG_AREA <= 80), c("REAL_AREA")] = 5
  apts[which(apts$BLDG_AREA >= 71 & apts$BLDG_AREA <= 80), c("REAL_AREA_DESC")] = "(71~80)"
  apts[which(apts$BLDG_AREA >= 81 & apts$BLDG_AREA <= 90), c("REAL_AREA")] = 6
  apts[which(apts$BLDG_AREA >= 81 & apts$BLDG_AREA <= 90), c("REAL_AREA_DESC")] = "(81~90)"
  apts[which(apts$BLDG_AREA >= 91 & apts$BLDG_AREA <= 100), c("REAL_AREA")]  = 7
  apts[which(apts$BLDG_AREA >= 91 & apts$BLDG_AREA <= 100), c("REAL_AREA_DESC")] = "(91~100)"
  apts[which(apts$BLDG_AREA >= 101), c("REAL_AREA")]  = 8
  apts[which(apts$BLDG_AREA >= 101), c("REAL_AREA_DESC")] = "(101~)"
  
  apts$BLDG_NM = factor(apts$BLDG_NM)
  apts$GROUP = do.call(paste0, list(apts$BLDG_NM, apts$REAL_AREA_DESC))
  apts$GROUP = factor(apts$GROUP)
  apts$SUM_AMT = as.numeric(gsub(",", "", apts$SUM_AMT))
  return (apts)  
}

#-------------------------------------------------------------------------------
# 국세청에서 온 자료는 nested 형태이기 때문에 각 row 를 개별 data.frame 으로
# 변경해야 한다.
#-------------------------------------------------------------------------------
f_parseData = function(df, dealYear, dealPeriod) {
  result1 = data.frame()
  result2 = data.frame()
  result3 = data.frame()
  aptInfo = df[, c("BOBN", "BLDG_ROW", "BLDG_NM", "BUBN", "BLDG_CD", "BUILD_YEAR", 
                   "BLDG_CNT", "BUILD_ROW", "BLDG_AREA", "AREA_ROW", "AREA_CNT", 
                   "BUILD_CNT")]
  aptInfo$DEAL_YYYY = dealYear
  if (df$CNT1[1] > 0) {
    result1 = cbind(aptInfo, df$month1List[[1]], row.names = NULL)
    result1$DEAL_MM = (dealPeriod - 1) * 3 + 1
  }
  if (df$CNT2[1] > 0) {
    result2 = cbind(aptInfo, df$month2List[[1]], row.names = NULL)
    result2$DEAL_MM = (dealPeriod - 1) * 3 + 2
  }
  if (df$CNT3[1] > 0) {
    result3 = cbind(aptInfo, df$month3List[[1]], row.names = NULL)
    result3$DEAL_MM = (dealPeriod - 1) * 3 + 3
  }
  return(rbind(result1, result2, result3))
}

#-------------------------------------------------------------------------------
# 특정시기의 자료를 가져온다.
#-------------------------------------------------------------------------------
f_getData = function(sidoCode, gugunCode, dongCode, year, period, requestType) {
  
  # 실거래가 사이트에서 자료를 가져온다.
  data = f_readUrl(requestType, sidoCode, gugunCode, dongCode, year, period)  
  if (is.null(data)) return(NULL)  
  
  result = data.frame()
  # nested column 을 분해해서 data.frame 으로 만든다.
  for (idx in 1:nrow(data)) {
    temp = f_parseData(data[idx, ], year, period)
    result = rbind(result, temp)
  }
 
  # 각종 컬럼을 추가한다.
  result = f_addInfo(result, dongCode)
  
  # 전세일 경우에 월세금액을 처리한다.
  if (requestType == "r") {
    result$RENT_AMT = as.numeric(gsub(",", "", result$RENT_AMT)) 
  }
  return(result)
}

#-------------------------------------------------------------------------------
# 특정 동코드의 연도별 데이터를 생성한다.
# [수정사항]
#   2015.09.18  
#     - 데이터 컬럼을 정렬한다. 
#   2015.10.13
#     - requestType => r 전월세 t 매매
#     - 시도, 구군코드 추가
#-------------------------------------------------------------------------------
f_dongYearData = function(dongCode, from, to, requestType) {
  apts = data.frame()
  sidoCode = substr(dongCode, 1, 2)
  gugunCode = substr(dongCode, 1, 5)
  for (year in from:to) {
    for (period in 1:4) {
      tempApts = f_getData(sidoCode, gugunCode, dongCode, year, period, requestType)		  
      apts = rbind(apts, tempApts)
    }
  }  
  if (!is.null(apts)) {
    if (nrow(apts) != 0) apts = apts[, order(names(apts))]
  }
  return(apts) 
}

#-------------------------------------------------------------------------------
# 두 기간 사이에 걸쳐진 분기들을 반환한다.
#-------------------------------------------------------------------------------
f_periods = function(startYear, startPeriod, endYear, endPeriod) {
  if ((startYear * 100 + startPeriod) > (endYear * 100 + endPeriod))
    return(NULL)
  
  start.period = seq(startPeriod, 4, by = 1)
  start.year = rep(startYear, length(start.period))
  start.DF = data.frame(year = start.year, period = start.period) 
  
  end.period = seq(1, endPeriod, by = 1)
  end.year = rep(endYear, length(end.period))
  end.DF = data.frame(year = end.year, period = end.period) 
  
  midYears = seq(startYear, endYear, by = 1)
  midYears = setdiff(setdiff(midYears, startYear), endYear)
  if (length(midYears) == 0) mid.DF = data.frame()
  else {
    periods = c(1, 2, 3, 4)
    mid.years = rep(midYears, each = 4)
    mid.period = rep(periods, length(midYears))
    mid.DF = data.frame(year = mid.years, period = mid.period) 
  }
  
  return(rbind(start.DF, mid.DF, end.DF))
}

#-------------------------------------------------------------------------------
# 조회된 데이터를 파일로 저장
#-------------------------------------------------------------------------------
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

#-------------------------------------------------------------------------------
# 입력으로 받은 구군코드들에 대해서 정해진 기간의 데이터를 가져온다.
# 데이터를 가져오는 실질적인 함수
# prefix => r 전월세 t 매매
#-------------------------------------------------------------------------------
f_crawler = function(gugunCodes, fromYear, toYear,  prefix) { 
  msg = paste0(fromYear, "~", toYear, " for ", prefix) 
  info(logger, msg)

  # 구군단위의 연도별로 데이터 파일을 생성한다.  
  for (curGugunCode in gugunCodes) { 
    startTime = Sys.time()
    dongCodes = data.frame()
    result = data.frame()
    curDongs = subset(dongs, gugunCode == curGugunCode)[,3]

    for (year in fromYear:toYear) {
      result = apply(as.data.frame(curDongs), 1, f_dongYearData, year, year, prefix)
      result = do.call("rbind", result)
      fileName = paste(paste(prefix, curGugunCode, year, sep="_"), "rds", sep=".")
      saveRDS(result, file.path(savePath, fileName))
    }
    endTime = Sys.time()

    # 여기서부터 아래는 시간 로깅    
    gugunIdx = which(curGugunCode == gugunCodes)
    pct = round((gugunIdx / length(gugunCodes)) * 100, 2)
    
    timeTakes = difftime(endTime, startTime)
    remainTimes = (length(gugunCodes) - gugunIdx) * timeTakes
    
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
  msg = paste(fromYear, "~" , toYear, prefix, "ended", sep = " ")
  info(logger, msg)
}

#-------------------------------------------------------------------------------
# 특정기간 구군의 데이터를 읽어들인다.
#-------------------------------------------------------------------------------
f_readLocalGugunData = function(tradeType, gugunCode, fromYear, toYear) {
  files = sapply(fromYear:toYear, 
                 function(year) dir("data", 
                                    paste0(tradeType, "_", gugunCode, "_", year)))
  files = sapply(files, function(fileName) file.path("data", fileName))
  result = lapply(files, function(fileName) readRDS(fileName))
  result = do.call("rbind", result)
}

