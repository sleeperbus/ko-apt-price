library(shiny)
library(plyr)
library(dygraphs)

sido = readRDS("data/sido.rds")
sido$sidoCode = as.character(sido$sidoCode)
sido$sidoName = as.character(sido$sidoName)

gugun = readRDS("data/gugun.rds")
gugun$sidoCode = as.character(gugun$sidoCode)
gugun$gugunCode = as.character(gugun$gugunCode)
gugun$gugunName = as.character(gugun$gugunName)

dong = readRDS("data/dong.rds")
dong$sidoCode = as.character(dong$sidoCode)
dong$gugunCode = as.character(dong$gugunCode)
dong$dongCode = as.character(dong$dongCode)
dong$dongName = as.character(dong$dongName) 

shinyServer(function(input, output, clientData, session){  
  # 거래유형에 따른 툴팁
  tooltip = function(df) {
    message("tooltipForTrade In")
    if (is.null(df)) return(NULL)
    apts = dongData()
    
    if (newType() == "t") {
      codes = c("아파트", "전용면적", "매매가", "매매일")
      row = apts[apts$ID == df$ID, c("APT_NAME", "AREA", "TRADE_AMT", "SALE_DATE")]
      if (nrow(row) > 0)
        msg = paste0(codes, ": ", format(row), collapse = "<br />") 
      else msg = NULL  
    } else {
      codes = c("아파트", "전용면적", "보증금", "월세", "거래일")
      row = apts[apts$ID == df$ID, c("APT_NAME", "AREA", "TRADE_AMT", "MONTHLY", "SALE_DATE")]
      if (nrow(row) > 0)
        msg = paste0(codes, ": ", format(row), collapse = "<br />") 
      else msg = NULL   
    }
    return(msg)
  }
  
  # 새롭게 선택된 동코드를 반환
	newGugunCode = eventReactive(input$refreshButton, { 
		message("newGugunCode in")
		message(paste("newGugunCode is", input$gugun))
		input$gugun
		})

  # 새롭게 선택된 구군코드를 반환
	newDongCode = eventReactive(input$refreshButton, { 
		message("newDongCode in")
		message(paste("newDongCode is", input$dong))
		input$dong
		})

  # 새롭게 선택된 시작연도
  newStartYear = eventReactive(input$refreshButton, { 
    message("newStartyear In")
    message(paste("current start year", input$period[1]))
    return(input$period[1])
  })

  # 새롭게 선택된 종료연도
  newEndYear = eventReactive(input$refreshButton, {
    message("newEndyear In")
    message(paste("current end year", input$period[2]))
    return(input$period[2])
  })

  # 매매, 전세인지 반환
  newType = eventReactive(input$refreshButton, {
    message("newType In")
    return(input$type)
  })

  # 구군 데이터를 반환한다.
	gugunData = reactive({ 
    message("gugunData In")
    curGugunCode = newGugunCode()
		message(paste("selected gugunCode is", curGugunCode))
    apts = data.frame() 
    
    for (year in newStartYear():newEndYear()) {
      fileName = paste(paste(newType(), curGugunCode, year, sep="_"), "rds", sep=".")
      fileName = paste("data", fileName, sep="/")
      if (file.exists(fileName)) {
        yearApts = readRDS(fileName) 
        apts = rbind(apts, yearApts)
      }  
    }  
    if (nrow(apts) == 0) return(NULL)
    apts$ID = 1:nrow(apts)
    return(apts)
	})

  # 동 데이터를 반환한다.
  dongData = reactive({
    message("dongData In")
    gugunApts = gugunData()
    if (is.null(gugunApts)) return(NULL)
    
    curDongCode = ""
    t = try(newDongCode())
    if ("try-error" %in% class(t)) {
      curDongCode = "1168010300"
    } else curDongCode = newDongCode()
    
    dongApts = subset(gugunApts, DONG_CODE == curDongCode)
    if (nrow(dongApts) == 0) return(NULL)
    return(dongApts) 
  })
	
	output$aptNames = renderUI({
		message("aptNames in")
		apts = dongData() 
    if (is.null(apts)) return(NULL)
		aptNames = list()
		uniqueApts = apts[, c("APT_NAME", "APT_CODE")]
		uniqueApts = uniqueApts[!duplicated(uniqueApts),]
    uniqueApts = uniqueApts[with(uniqueApts, order(APT_NAME)), ]
		aptNames = as.list(uniqueApts[,2])
		names(aptNames) = uniqueApts[,1]
		# checkboxGroupInput("aptCodes", "", choices=aptNames) 
		selectizeInput("aptCodes", "아파트를 선택하세요.", choices=aptNames, 
		               multiple = TRUE, options=list(
		                placeholder="여러 아파트를 선택하실 수 있습니다.",
		                onInitialize=I('function() { this.setValue(""); }')))
		})    
	
	observe({
		message("observe gugun in")
		sidoCode = input$sido    
    message(paste("sidoCode is", sidoCode))
		
		codes = list()
		selectedGugun = subset(gugun, sidoCode == input$sido)
		codes = as.list(selectedGugun[,2])
		names(codes) = selectedGugun[,3] 
		updateSelectInput(session, "gugun", "구군", 
			choices=codes, selected=selectedGugun[1,2])     
		})

	observe({
		message("observe dong in")
		sidoCode = input$sido
		gugunCode = input$gugun
    message(paste("sidoCode is", sidoCode, "gugunCode is", gugunCode))
		
		codes = list()
		selectedDong = subset(dong, sidoCode == input$sido & gugunCode == input$gugun)
		codes = as.list(selectedDong[,3])
		names(codes) = selectedDong[,4]
		updateSelectInput(session, "dong", "동", choices=codes, selected=selectedDong[1,3])  
		})

  # 선택된 구군 지역 거래량
  observe({
    message("graph gugun histogram in")
    apts = gugunData()
    if (is.null(apts)) return(NULL)

    gugunName = gugun[which(input$gugun == gugun$gugunCode), c("gugunName")]
    gugunName = paste(gugunName, "거래량")
    ggvis(apts, x=~SALE_DATE, fill := "#fff8dc") %>%
      layer_histograms(width=30) %>%
  		add_axis("x", title="거래일") %>% 
  		add_axis("x", title=gugunName, title_offset=20, orient="top", ticks=0, 
               properties=axis_props(
                 axis=list(stroke="white"), 
                 labels=list(fontSize=0))) %>%  
  		add_axis("y", title="거래량", title_offset=70) %>%
      set_options(width="auto") %>%
      bind_shiny("plotHist")
  })
  
  observe({
		message("graph dong points in")
    
		apts = dongData() 
    apts$FREQ = 0
    if (is.null(apts)) return(NULL)
    
    dongName = dong[which(dong$dongCode == apts$DONG_CODE[1]), c("dongName")]
		aptCodes = input$aptCodes
		realArea = input$realArea
			
		result = subset(apts, APT_CODE %in% aptCodes)
		result = subset(result, REAL_AREA%in% realArea) 
		result$APT_NAME = factor(result$APT_NAME, ordered=F)
		result$GROUP = factor(result$GROUP)

		# 최종 result 에서 GROUP 별로 데이터가 2개 이하이면 layer_smooths 를 
		# 그리는데 에러가 발생한다. 
    # 추세선을 그리는 df 와 point 를 찍을 df 를 분리한다.
		pointDF = ddply(result, .(GROUP), transform, FREQ = length(GROUP))
		lineDF = subset(pointDF, FREQ > 3)
    if (nrow(lineDF) == 0) defaultDF = pointDF
    else defaultDF = lineDF
	
		# 만약 전세라면 추세선을 그릴 때 반전세들은 제외해야 한다.	
		if (newType() == "r") defaultDF = subset(defaultDF, MONTHLY == 0)
		graph = ggvis(defaultDF, x=~SALE_DATE, y=~TRADE_AMT, fill=~GROUP, stroke=~GROUP) %>%
		group_by(GROUP)
  
    if (nrow(lineDF) > 0) graph = graph %>% layer_smooths(span=1)
    graph %>%        
		layer_points(data = pointDF, opacity:=0.4, key :=~ID) %>%
		add_tooltip(tooltip, "hover") %>%
		add_axis("x", title="거래일") %>% 
		add_axis("x", title=dongName, title_offset=20, orient="top", ticks=0, 
             properties=axis_props(
               axis=list(stroke="white"), labels=list(fontSize=0))) %>%  
		add_axis("y", title="거래가격", title_offset=70) %>%
    set_options(width="auto") %>%
    bind_shiny("plotPoint")
		})  
	})
