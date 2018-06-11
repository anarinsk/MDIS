# server.R

function(input, output, session) {
  
  session$onSessionEnded(function() {
    stopApp()
    q("no")
  })
  
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

  rtbl <- eventReactive(input$go, {
    
   id <<-  showNotification("Calculating & plotting...", duration = 0)
   
   if(input$onehh == F)
      {
      tbl %.>% 
        dplyr::filter(., V4 %in% filter_hhdcat()) %.>%
        assign_PCT(filter_income1(), ., n_tile = input$ntile) -> ttl1
      #
      write_rds(ttl1, "./data/tTarget.rds")
      #
      ttl1 %.>% 
        gen_tincome(.) %.>% 
        cal_dff(filter_income2(), .) -> ttl2} else { 
      #
      tbl %.>% 
         dplyr::filter(., V4 %in% filter_hhdcat(), V5 %in% c(1)) %.>%
      assign_PCT(filter_income1(), ., n_tile = input$ntile) -> ttl1
      #
      write_rds(ttl1, "./data/tTarget.rds")
      #
      ttl1 %.>% 
        gen_tincome(.) %.>% 
        cal_dff(filter_income2(), .) -> ttl2}
   
    removeNotification(id)   
    ttl2
    
    }
   )
  
  output$plot1 <- renderPlot(
    rtbl()  %.>% draw_hist(., c(1), input$ntile)
    )
  
  output$plot2 <- renderPlot(
    rtbl()  %.>% draw_hist(., c(2), input$ntile)
  )
  
 output$downloadData1 <- downloadHandler(
  filename = function() {
      paste('raw-', Sys.Date(), '.csv', sep='')
     },
     content = function(con) {
       write_csv(tbl0, con)
     }
   )
 
 read_rds("./data/tTarget.rds") -> ttl1
 output$downloadData2 <- downloadHandler(
   filename = function() {
     paste('label-', Sys.Date(), '.csv', sep='')
   },
   content = function(con) {
     write_csv(ttl1, con)
   }
 )

 output$col <- renderDataTable(
  read_excel("./data/colnames.xlsx", col_names=T) )
 
#  
}
