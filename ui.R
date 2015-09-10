library(shiny)
library(ggvis)
library(shinydashboard)

sido = readRDS("data/sido.rds")
sido$sidoCode = as.character(sido$sidoCode)
sido$sidoName = as.character(sido$sidoName) 
codes = list()
codes = as.list(sido[,1])
names(codes) = sido[,2]

shinyUI(
  navbarPage(
    "아파트 거래가 추이",
    tabPanel("우리동네",
             fluidRow(
               column(4, 
                      radioButtons("type", h4("거래유형"), choices = list("매매"="t", "전세"="r"),
                                   selected="t"),
                      sliderInput("period", "기간:", min=2006, max=2015, value=c(2011, 2015))
               ), 
               column(4,
                      h4("지역선택"),
                      selectInput("sido", "시도", choices=codes, selected=sido[1,1]),
                      selectInput("gugun", "구군", choices=list()),
                      selectInput("dong", "동", choices=list()),
                      actionButton("refreshButton", "적용", icon("refresh"))
                      
               ),
               column(4,
                      checkboxGroupInput("realArea",  "전용면적", 
                                         list("~35"=0, "36~40"=1, "41~50"=2, "51~60"=3, 
                                              "61-70"=4, "71~80"=5, 
                                              "81~90"=6, "91~100"=7, "101~"=8), 
                                         selected=c(5,6)),
                      uiOutput("aptNames")
               )
             ), 
             hr(),
             fluidRow(
               column(7, ggvisOutput("plotPoint")),
               column(5, ggvisOutput("plotHist"))
             )             
    ),
    tabPanel(
      "About",
      fluidRow(
        column(12, includeMarkdown("about.md"))
      )
    )
  )  
)
