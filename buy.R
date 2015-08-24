library(jsonlite)
library(stringr)
library(ggplot2)
library(scales)
library(ggvis)

f_makeData = function(dongCode) {
	apts = data.frame()
	for (srhYear in 2006:2015) {
		for (srhPeriod in 1:4) {
			url = paste0("http://rt.molit.go.kr/rtApt.do?cmd=getTradeAptLocal&dongCode=", 
						 dongCode, "&danjiCode=", "ALL", "&srhYear=", srhYear,
						 "&srhPeriod=", srhPeriod, "&gubunRadio2=", "1")
			rawData = readLines(url, warn="F", encoding="UTF-8")
			data = fromJSON(rawData) 
			aptInfo = as.data.frame(data[1])
			prices = as.data.frame(data[2]) 
			if (nrow(prices) > 0) {
				names(aptInfo) = c("APT_NAME", "AREA_CNT", "APT_CODE", "BORM", 
								   "BUILD_YEAR", "BUBN")
				names(prices) = c("SALE_MONTH", "SUM_AMT", "SALE_DAYS", "APT_CODE", 
								  "FLOOR", "AREA")
				tempApts = merge(aptInfo, prices, by="APT_CODE") 
				tempApts$SALE_YEAR = srhYear
				apts = rbind(apts, tempApts) 
			}
		}
	} 
	apts$SALE_MONTH = str_pad(apts$SALE_MONTH, 2, pad="0")
	apts$SALE_DAYS= str_pad(apts$SALE_DAYS, 2, pad="0")
	apts$SUM_AMT = as.numeric(gsub(",", "", apts$SUM_AMT))
	apts$SALE_DATE = do.call(paste0, apts[,c("SALE_YEAR", "SALE_MONTH", "SALE_DAYS")])
	apts$SALE_DATE = strptime(apts$SALE_DATE, "%Y%m%d")
	apts$SALE_DATE = as.Date(apts$SALE_DATE)
	apts$AREA = round(as.numeric(apts$AREA))
	apts = apts[with(apts, order(SALE_DATE)),]
	apts$PYUNG = 0
	apts[with(apts, AREA < 70 ),]$PYUNG = 24
	apts[with(apts, AREA >= 70 & AREA < 80),]$PYUNG = 28
	apts[with(apts, AREA >= 80 & AREA < 90),]$PYUNG = 33
	apts[with(apts, AREA >= 90),]$PYUNG = 40
	apts$ENG_NAME = ""
	return (apts)
}

f_plot = function(data, aptCodes, baseDate, pyungs) { 
	if (!missing(aptCodes)) {
		if (length(aptCodes) > 0) data= subset(data, APT_CODE %in% aptCodes)             
	}
	
	if (!missing(baseDate)) data = subset(data, SALE_DATE >= as.Date(baseDate, "%Y%m%d"))    
	
	if (!missing(pyungs)) {
		if (length(pyungs) > 0) data= subset(data, PYUNG %in% pyungs)             
	}
	
	p = ggplot(data=data, aes(x=SALE_DATE, y=SUM_AMT, 
							group=interaction(APT_CODE, PYUNG), 
							color=interaction(APT_CODE, PYUNG))) +
		geom_smooth() +
		geom_point(size=2) +
		scale_x_date(breaks = date_breaks(width="1 year")) +
		scale_y_continuous(breaks=seq(10000,90000,1000))
	return (p)
}

f_plot2 = function(data, aptCodes, baseDate, pyungs) { 
  if (!missing(aptCodes)) {
	if (length(aptCodes) > 0) data= subset(data, APT_CODE %in% aptCodes)             
  }
  
  if (!missing(baseDate)) data = subset(data, SALE_DATE >= as.Date(baseDate, "%Y%m%d"))    
  
  if (!missing(pyungs)) {
	if (length(pyungs) > 0) data= subset(data, PYUNG %in% pyungs)             
  }
  
  p = ggvis(data, x = ~SALE_DATE, y = ~SUM_AMT) %>%
	group_by(APT_NAME) %>%
	layer_points(fill = ~factor(APT_NAME)) %>%
	layer_smooths(stroke = ~factor(APT_NAME))
  return (p)
}


# 검암동 
gumamCode = "2826010300"
gumam = f_makeData(gumamCode)
cols = c("APT_CODE", "APT_NAME", "AREA", "PYUNG")
unique(gumam[with(gumam, order(APT_NAME)), cols])

# 검암동 영문명 생성
gumam[with(gumam, APT_CODE == "51003"),]$ENG_NAME = "PL1"
gumam[with(gumam, APT_CODE == "73984"),]$ENG_NAME = "SM4"
gumam[with(gumam, APT_CODE == "218938"),]$ENG_NAME = "PL2"
gumam[with(gumam, APT_CODE == "51015"),]$ENG_NAME = "SG"
gumam[with(gumam, APT_CODE == "51017"),]$ENG_NAME = "SB1"
gumam[with(gumam, APT_CODE == "51021"),]$ENG_NAME = "SM1"
gumam[with(gumam, APT_CODE == "218939"),]$ENG_NAME = "PL3"
gumam[with(gumam, APT_CODE == "218938"),]$ENG_NAME = "PL2"
gumam[with(gumam, APT_CODE == "218940"),]$ENG_NAME = "SM3"
gumam[with(gumam, APT_CODE == "20054546"),]$ENG_NAME = "MGR"
gumam[with(gumam, APT_CODE == "20101413"),]$ENG_NAME = "RICH"
gumam[with(gumam, APT_CODE == "20141121"),]$ENG_NAME = "MORNING"
gumam[with(gumam, APT_CODE == "51011"),]$ENG_NAME = "SM2"
gumam[with(gumam, APT_CODE == "51007"),]$ENG_NAME = "SB2"
unique(gumam[c("APT_CODE", "APT_NAME", "ENG_NAME")])

targets = subset(gumam, PYUNG %in% c(28, 33))

# 신명스카이뷰3차 = 218940, 삼보해피하임4 = 51017, 신명스카이뷰1 = 51021
# 서해그랑블 = 51015, 마젤란 = 20054546
sector1 = subset(targets, APT_CODE %in% c("218940", "51017", "51021",  "51015", "20054546"))

# 풍림1차 = 51003, 풍림2차 = 218938, 풍림3차 = 218939, 삼보2차 = 51007
# 신명2차 = 51011, 신명4차 = 73984
sector2 = subset(targets, APT_CODE %in% c("51003", "218938", "218939", "51007", "51011", "73984"))

# x = f_plot(data=sector1, baseDate="20130101", pyungs=c(33))
x = f_plot2(data=sector1, baseDate="20130101", pyungs=c(33))
print(x)

# gita
gitaCode = "4413111600"
gita = f_makeData(gitaCode)
unique(gita[with(gita, order(APT_NAME)), cols])
	 
x = f_plot(data=gita, c("11133"), baseDate="20100101", pyungs=c(24, 28, 33))
print(x)
