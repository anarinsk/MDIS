# server.R
function(input, output) {
  filter_hhdcat <- function(){
    length(input$hhdcat) -> hhdcat_l
    
    if(hhdcat_l == 0) {c(100)} else
    if(hhdcat_l == 2) {c(1,2)} else 
    if(input$hhdcat %in% "근로자") {c(1)} else {c(2)}
  }
  filter_income1 <- function(){
    if(input$income == "소득") {"V100"} else
    if(input$income == "경상소득") {"V101"} else 
    if(input$income == "근로소득") {"V102"} else {"V103"}
  }
  filter_income2 <- function(){
    if(input$summary=="평균"   & input$weight == F){"tincome_mn"} else 
    if(input$summary=="중간값" & input$weight == F){"tincome_md"} else 
    if(input$summary=="평균"   & input$weight == T){"tincome_wtn"} else {"tincome_wtd"}
  }

  observe(print(filter_hhdcat()))
  observe(print(filter_income1()))
  observe(print(filter_income2()))

  rtbl <- reactive({
    if(input$onehh == F)
      { tbl %.>% 
        dplyr::filter(., V4 %in% filter_hhdcat()) %.>%
        gen_tincome(filter_income1(), .) %.>% 
        cal_dff(filter_income2(), .)} else 
      { tbl %.>% 
             dplyr::filter(., V4 %in% filter_hhdcat()) %.>% 
             dplyr::filter(., V5 %in% c(1)) %.>%
          gen_tincome(filter_income1(), .)  %.>% 
          cal_dff(filter_income2(), .) }
    })
  
  output$plot1 <- renderPlot(
    rtbl()  %.>% 
      ggplot(.) +
      aes(x=tTarget, y=dff) %.>% 
      geom_point(., color = "red", alpha = 0.5) + 
      geom_col(., color = "grey") + 
      labs(x="Percentile block", y="Difference in Income (2018-2017)") + 
      ggtitle("Income difference of Korean Sample Households") + 
      theme_classic()
    )
  #output$res <- renderDataTable(tibble(a = c(1:2), b = c(2:3)))
  
 output$downloadData <- downloadHandler(
  filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
     },
     content = function(con) {
       write_csv(tbl0, con)
     }
   )

 output$col <- renderDataTable(
  read_excel("./data/colnames.xlsx", col_names=T) )
 
#  
}
