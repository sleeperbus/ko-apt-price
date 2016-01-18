################################################################################
# 데이터를 가져와서 rds 형식으로 저장한다. 
# 분기별로 저장할 수 있지만, 유저는 연도별로 조회를 할 것이므로 연도별 자료를
# 가져와서 저장한다. 분기별로 가져오는 것이 시간이 현저하게 적게 걸리기는 하는데
# 어차피 작업은 내가 하는게 아니라, 컴퓨터가 하는 것이니...
################################################################################
source("helpers.R")

sidos = readRDS("data/sido.rds")
guguns = readRDS("data/gugun.rds")
dongs = readRDS("data/dong.rds")

f_crawler(guguns$gugunCode, 2015, 2015, "t")
f_crawler(guguns$gugunCode, 2015, 2015, "r")
f_crawler(guguns$gugunCode, 2016, 2016, "t")
f_crawler(guguns$gugunCode, 2016, 2016, "r")
