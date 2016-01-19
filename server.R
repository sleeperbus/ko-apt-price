################################################################################
# 유저의 요청을 처리한다.
################################################################################
library(shiny)
library(plyr)
library(lubridate)
library(dygraphs)

# 각종 코드 값은 처음 한 번만 읽어들인다.
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
  #-----------------------------------------------------------------------------
  # 거래유형에 따른 툴팁
  #-----------------------------------------------------------------------------
  tooltip = function(df) {
    message("tooltipForTrade In")
    if (is.null(df)) return(NULL)
    apts = dongData()
    
    if (newType() == "t") {
      codes = c("아파트", "건축연도", "전용면적", "층수", "매매가", "매매일")
      row = apts[apts$ID == df$ID, c("BLDG_NM", "BUILD_YEAR", "BLDG_AREA", "APTFNO", "SUM_AMT", "DEAL_DATE")]
      if (nrow(row) > 0)
        msg = paste0(codes, ": ", format(row), collapse = "<br />") 
      else msg = NULL  
    } else {
      codes = c("아파트", "건축연도", "전용면적", "층수", "보증금", "월세", "거래일")
      row = apts[apts$ID == df$ID, c("BLDG_NM", "BUILD_YEAR", "BLDG_AREA", "APTFNO", "SUM_AMT", "RENT_AMT", "DEAL_DATE")]
      if (nrow(row) > 0)
        msg = paste0(codes, ": ", format(row), collapse = "<br />") 
      else msg = NULL   
    }
    return(msg)
  }
  
  #-----------------------------------------------------------------------------
  # 새롭게 선택된 구군코드를 반환
  #-----------------------------------------------------------------------------
  newGugunCode = eventReactive(input$refreshButton, { 
    message("newGugunCode in")
    message(paste("newGugunCode is", input$gugun))
    input$gugun
  })
  
  #-----------------------------------------------------------------------------
  # 새롭게 선택된 동코드를 반환
  #-----------------------------------------------------------------------------
  newDongCode = eventReactive(input$refreshButton, { 
    message("newDongCode in")
    message(paste("newDongCode is", input$dong))
    input$dong
  })
  
  #-----------------------------------------------------------------------------
  # 선택된 시작연도
  #-----------------------------------------------------------------------------
  newStartYear = eventReactive(input$refreshButton, { 
    message("newStartyear In")
    message(paste("current start year", input$period[1]))
    return(input$period[1])
  })
  
  #-----------------------------------------------------------------------------
  # 선택된 종료연도
  #-----------------------------------------------------------------------------
  newEndYear = eventReactive(input$refreshButton, {
    message("newEndyear In")
    message(paste("current end year", input$period[2]))
    return(input$period[2])
  })
  
  #-----------------------------------------------------------------------------
  # 매매, 전세인지 반환
  #-----------------------------------------------------------------------------
  newType = eventReactive(input$refreshButton, {
    message("newType In")
    return(input$type)
  })
  
  #-----------------------------------------------------------------------------
  # 구군 데이터를 반환한다.
  #-----------------------------------------------------------------------------
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
  
  #-----------------------------------------------------------------------------
  # 동 데이터를 반환한다.
  # 기본 동코드를 지정해준다.
  #-----------------------------------------------------------------------------
  dongData = reactive({
    message("dongData In")
    gugunApts = gugunData()
    if (is.null(gugunApts)) return(NULL)
    
    curDongCode = ""
    dongApts = data.frame()
    t = try(newDongCode())
    if ("try-error" %in% class(t)) {
      curDongCode = "1168010300"
    } else curDongCode = newDongCode()
    
    dongApts = subset(gugunApts, DONG_CODE == curDongCode)
    message(paste("row of dongData is ", nrow(dongApts)))
#    if (nrow(dongApts) == 0) return(NULL)
    return(dongApts) 
  })
  
  #-----------------------------------------------------------------------------
  # 선택된 동데이터를 기반으로 아파트 목록을 select box 에 출력한다.
  # - 정렬을 시키는데, 정렬이 제대로 안된다.
  #-----------------------------------------------------------------------------
  
  output$aptNames = renderUI({
    message("aptNames in")
    apts = dongData() 
    aptNames = list()
    if (nrow(apts) == 0) {
      selectizeInput("aptCodes", "아파트를 선택하세요.", choices=aptNames, 
                   multiple = TRUE, options=list(
                     placeholder="해당 지역에 아파트가 없습니다.",
                     onInitialize=I('function() { this.setValue(""); }')))
      return(NULL)
       
    }
    
    uniqueApts = apts[, c("BLDG_NM", "BLDG_CD")]
    uniqueApts = uniqueApts[!duplicated(uniqueApts),]
    uniqueApts = uniqueApts[with(uniqueApts, order(BLDG_NM)), ]
    aptNames = as.list(uniqueApts[,2])
    names(aptNames) = uniqueApts[,1]
    
    selectizeInput("aptCodes", "아파트를 선택하세요.", choices=aptNames, 
                   multiple = TRUE, options=list(
                     placeholder="여러 아파트를 선택하실 수 있습니다.",
                     onInitialize=I('function() { this.setValue(""); }')))
  })    
  
  #-----------------------------------------------------------------------------
  # sido 코드가 변경되었을 경우 UI 변경
  # - 구군코드와 동코드의 내용을 새로 채운다.
  #-----------------------------------------------------------------------------
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
  
  #-----------------------------------------------------------------------------
  # gugun 코드가 변경되었을 경우 UI 변경
  # - 동코드의 내용을 새로 채운다.
  #-----------------------------------------------------------------------------
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
  
  #-----------------------------------------------------------------------------
  # gugunData 의 내용이 변경되면 해당 구군의 거래량을 갱신한다.
  #-----------------------------------------------------------------------------
  output$plotHist = renderDygraph({
    message("graph gugun histogram in")
    apts = gugunData()
    if (is.null(apts)) return(NULL)
    
    gugunName = gugun[which(input$gugun == gugun$gugunCode), c("gugunName")]
    gugunName = paste(gugunName, "거래량")
    apts$DEAL_MM = floor_date(apts$DEAL_DATE, "month")
    trade = count(apts, vars=c("DEAL_MM"))
    names(trade) = c("DEAL_MM", "거래량")
    trade$DEAL_MM = NULL
    ts.trade = ts(as.matrix(trade), start = c(newStartYear(), 1), frequency = 12)
    dygraph(ts.trade, main=gugunName) %>% dyRangeSelector() %>%
      dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2"), fillGraph = TRUE) %>%
      dyAxis("x", drawGrid = FALSE) 
  })
  
  #-----------------------------------------------------------------------------
  # 동별 아파트 매매가 변경
  # - 동데이터가 변경되거나 선택된 아파트, 전용면적이 변경되면 그래프 갱신
  # - 해당 동에 데이터가 존재하지 않으면 그래프를 클리어 한다.
  #-----------------------------------------------------------------------------
  observe({
    message("graph dong points in")
    
    apts = dongData() 
    if (nrow(apts) == 0) {
      ggvis(apts, x=~DEAL_DATE, y=~SUM_AMT) %>% layer_points() %>% bind_shiny("plotPoint")
      return(NULL)
    }
    
    apts$FREQ = 0 
    dongName = dong[which(dong$dongCode == apts$DONG_CODE[1]), c("dongName")]
    aptCodes = input$aptCodes
    realArea = input$realArea
    
    result = subset(apts, BLDG_CD %in% aptCodes)
    result = subset(result, REAL_AREA%in% realArea) 
    result$BLDG_NM = factor(result$BLDG_NM, ordered=F)
    result$GROUP = factor(result$GROUP)
    
    # 최종 result 에서 GROUP 별로 데이터가 2개 이하이면 layer_smooths 를 
    # 그리는데 에러가 발생한다. 
    # 추세선을 그리는 df 와 point 를 찍을 df 를 분리한다.
    pointDF = ddply(result, .(GROUP), transform, FREQ = length(GROUP))
    lineDF = subset(pointDF, FREQ > 3)
    if (nrow(lineDF) == 0) defaultDF = pointDF
    else defaultDF = lineDF
    
    # 만약 전세라면 추세선을 그릴 때 반전세들은 제외해야 한다.	
    if (newType() == "r") defaultDF = subset(defaultDF, RENT_AMT == 0)
    graph = ggvis(defaultDF, x=~DEAL_DATE, y=~SUM_AMT, fill=~GROUP, stroke=~GROUP) %>%
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
