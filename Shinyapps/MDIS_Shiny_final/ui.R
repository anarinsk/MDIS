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
                    label = '가중치 사용여부(방법1)',
                    value = T),
      sliderInput(inputId = "ntile", 
                  label = "구간숫자", 
                  10, 100,
                  value = 100, step = 10),
      hr(), 
      actionButton(inputId = "go", "update!"),
      hr(),
      downloadButton(outputId = "downloadData1", label = "Download RAW"),
      hr(), 
      downloadButton(outputId = "downloadData2", label = "Download PCT")
    ), 
#
  mainPanel(
  tabsetPanel(
    tabPanel("Plot", plotOutput(outputId = "plot1"), plotOutput(outputId = "plot2")), 
    tabPanel("README", includeHTML("desc.nb.html")),
    #tabPanel("README", includeMarkdown("desc.rmd")), 
    tabPanel("Columns desc", dataTableOutput(outputId = "col"))
  ) ) # End of tabsetPanel & mainPanel
#
) # End of sidebarPanel 
) # End of fluidPage


