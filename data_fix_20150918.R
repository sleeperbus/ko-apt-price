################################################################################
# 2015년 9월부터 웹에서 넘어오는 데이터가 변경되었다.
#   - 컬럼순서가 변경됨
#   - 컬럼명이 변경됨
# 이것을 해결하기 위해 다음과 같은 순서로 작업을 한다.
#   0. 2015년 데이터를 삭제
#   1. 데이터의 BORM 컬럼명을 BOBN 컬럼명으로 변경한다.
#   2. 데이터의 컬럼을 재정렬
#   3. 2015년 데이터를 생성
################################################################################
source("helpers.R")
fixData = function(filename) {
  df = readRDS(filename) 
  if (!is.null(df)) {
    if (nrow(df) != 0) {
      df$BOBN = df$BORM
      df$BORM = NULL
      df = df[, order(names(df))]
      saveRDS(df, filename)       
    } 
  }
}

files = dir(path = "data", pattern = "[t|r].*.rds")
files = sapply(files, function(filename) file.path("data", filename))
names(files) = NULL
sapply(files, function(filename) fixData(filename))


# 검증
#testfile.1 = readRDS("data/r_11680_2011.rds")
#names(testfile.1)
#tail(testfile.1)

#testfile.2 = readRDS("data/t_11680_2011.rds")
#names(testfile.2)
#tail(testfile.2)

f_crawler(guguns$gugunCode, 2015, 2015, "t", f_getTrade)
f_crawler(guguns$gugunCode, 2015, 2015, "r", f_getRent)
