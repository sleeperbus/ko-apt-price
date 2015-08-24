library(shiny)

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
    uniqueApts = uniqueApts[with(uniqueApts, order(APT_NAME)), ]
		uniqueApts = uniqueApts[!duplicated(uniqueApts),]
		aptNames = as.list(uniqueApts[,2])
		names(aptNames) = uniqueApts[,1]
		checkboxGroupInput("aptCodes", "", choices=aptNames) 
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
      layer_histograms(width=90) %>%
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
    if (is.null(apts)) return(NULL)
    
    dongName = dong[which(dong$dongCode == apts$DONG_CODE[1]), c("dongName")]
		aptCodes = input$aptCodes
		realArea = input$realArea
		
		result = subset(apts, APT_CODE %in% aptCodes)
		result = subset(result, REAL_AREA%in% realArea)
		result$APT_NAME = factor(result$APT_NAME, ordered=F)
		result$GROUP = factor(result$GROUP)
		
		ggvis(result, x=~SALE_DATE, y=~TRADE_AMT, fill=~GROUP, stroke=~GROUP) %>%
		layer_points(opacity:=0.4, key :=~ID) %>%
		add_tooltip(tooltip, "hover") %>%
		group_by(GROUP) %>%
		layer_smooths(span=1) %>%
		add_axis("x", title="거래일") %>% 
		add_axis("x", title=dongName, title_offset=20, orient="top", ticks=0, 
             properties=axis_props(
               axis=list(stroke="white"), labels=list(fontSize=0))) %>%  
		add_axis("y", title="거래가격", title_offset=70) %>%
    set_options(width="auto") %>%
    bind_shiny("plotPoint")
		})  
	})
