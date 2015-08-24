library(XML)
library(stringr)
sidoCode = c("11", "26", "27", "28", "29", "30", "31", "36", "41", "42", "43",
	"44", "45", "46", "47", "48", "50")
sidoName = c("서울특별시", "부산광역시", "대구광역시", "인천광역시", 
	"광주광역시", "대전광역시", "울산광역시", "세종특별자치시", 
	"경기도", "강원도", "충청북도", "충청남도", "전라북도", "전라남도",
	"경상북도", "경상남도", "제주특별자치도")
sido = data.frame(sidoCode=sidoCode, sidoName=sidoName)
gugun = data.frame()
gugun = apply(sido, 1, function(df, result,...) {
	code = df[1]
	url = paste0("http://rt.molit.go.kr/rtApt.do?",
		"cmd=searchDong&gubunCode=1&houseType=Apt&aptGubun=1",
		"&gubunRadio=1&srhYear=2015&srhPeriod=2&gubunRadio2=1",
		"&sidoCode=", code, "&gugunCode=&dongCode=&chosung=",
		"&roadNm=&danjiCode=ALL") 
	html = htmlTreeParse(url, useInternalNodes = T)
	gugunCode = str_trim(xpathSApply(html, "//select[@id='gugunCode']//option", xmlAttrs))
	gugunName = str_trim(xpathSApply(html, "//select[@id='gugunCode']//option", xmlValue))
	data.frame(sidoCode=code, gugunCode=gugunCode, gugunName=gugunName) 
	})
gugun = do.call("rbind", gugun)
gugun = subset(gugun, gugunCode != "ALL")

dong = data.frame()
dong= apply(gugun, 1, function(df, result,...) {
	sidoCode = df[1]
	gugunCode= df[2]
	
	url = paste0("http://rt.molit.go.kr/rtApt.do?",
		"cmd=searchDong&gubunCode=1&houseType=Apt&aptGubun=1",
		"&gubunRadio=1&srhYear=2015&srhPeriod=2&gubunRadio2=1",
		"&sidoCode=", sidoCode, "&gugunCode=", gugunCode, 
		"&dongCode=&chosung=&roadNm=&danjiCode=ALL") 
	html = htmlTreeParse(url, useInternalNodes = T)
	dongCode = str_trim(xpathSApply(html, "//select[@id='dongCode']//option", xmlAttrs))
	dongName = str_trim(xpathSApply(html, "//select[@id='dongCode']//option", xmlValue))
	data.frame(sidoCode=sidoCode, gugunCode=gugunCode, dongCode = dongCode, dongName = dongName)
	})
dong = do.call("rbind", dong)
dong = subset(dong, dongCode!= "")

saveRDS(sido, file="sido.rds")
saveRDS(gugun, file="gugun.rds")
saveRDS(dong, file="dong.rds")
