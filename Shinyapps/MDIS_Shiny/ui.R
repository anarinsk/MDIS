# iu part 

ui <- fluidPage(
#
titlePanel(h3("Income Camparison bewteen Q1/17 and Q1/18")),
#  
  sidebarLayout(
    sidebarPanel(
      titlePanel(h4("선별 조건")),
      checkboxGroupInput(inputId = "hhdcat", 
                         label = "가구 구분",
                         choices = c("근로자","근로자 외"), 
                         selected = c("근로자","근로자 외")), # end of hhdcat
      checkboxInput(inputId = "onehh", 
                    label = '1인 가구만 선별',
                    value = F),
      hr(), 
      radioButtons(inputId = "income", 
                   label= "소득 종류", 
                   choices = c("소득", "경상소득", "근로소득", "가구주소득") , 
                   selected = "소득"),
      radioButtons(inputId = "summary", 
                   label= "구간 소득", 
                   choices = c("평균", "중간값") , 
                   selected = "평균"),
      checkboxInput(inputId = "weight", 
                    label = '가중치 사용여부',
                    value = T),
      br(), 
      
      downloadButton(outputId = "downloadData", label = "Download")
    ), 
#
  mainPanel(
  tabsetPanel(
    tabPanel("Plot", plotOutput(outputId = "plot1")), 
    tabPanel("Desc.", includeMarkdown("desc.rmd")), 
    tabPanel("Columns", dataTableOutput(outputId = "col"))
  ) ) # End of tabsetPanel & mainPanel
#
) # End of sidebarPanel 
) # End of fluidPage