
shinyServer(function(input, output, session) {

library(rms)
library(DT)  
load("validation.RData")  
load("calibration.RData")
  
  
 nomogramData <- reactive({
   
  d <- read.csv("data/zor ent 531.csv", head=T)
  
  return(d)
  
 })
 
 
 output$nData <- renderDataTable({
   
   datatable(nomogramData(), rownames = FALSE, extensions = c('Buttons','KeyTable', 'Responsive'), options = list(
     dom = 'Bfrtip', buttons = list('copy', 'print', list(extend = 'collection',
                                                          buttons = c('csv', 'excel', 'pdf'), text = 'Download')), keys = TRUE
   ))
   
 })
 
 prediction <- reactive({
   
   on.exit(detach("design.options")) 
   attach(list(), name="design.options") 
   d =  nomogramData()
   assign('ddist', datadist(d), pos='design.options') 
   options(datadist='ddist')
   
   f <- lrm(ZE ~  IIM + TGL + MOV1, data=d, x=TRUE, y=TRUE)
   
   
   newObs = data.frame(IIM = input$iim, TGL = input$tgl, MOV1 = input$mov1)

   fitted = rms:::predict.lrm(f, newObs, type=c("fitted"))
   
   return(fitted)
   
 })
 
 
 lp <- reactive({
   
   on.exit(detach("design.options")) 
   attach(list(), name="design.options") 
   d =  nomogramData()
   assign('ddist', datadist(d), pos='design.options') 
   options(datadist='ddist')
   
   f <- lrm(ZE ~  IIM + TGL + MOV1, data=d, x=TRUE, y=TRUE)
   
   
   newObs = data.frame(IIM = input$iim, TGL = input$tgl, MOV1 = input$mov1)
   
   lp = rms:::predict.lrm(f, newObs, type=c("lp"))
   
   return(lp)
   
 })
 
 
 adf <- reactive({
   
   on.exit(detach("design.options")) 
   attach(list(), name="design.options") 
   d =  nomogramData()
   assign('ddist', datadist(d), pos='design.options') 
   options(datadist='ddist')
   
   f <- lrm(ZE ~  IIM + TGL + MOV1, data=d, x=TRUE, y=TRUE)
   
   
   newObs = data.frame(IIM = input$iim, TGL = input$tgl, MOV1 = input$mov1)
   
   adf = rms:::predict.lrm(f, newObs, type=c("adjto.data.frame"))
   
   return(adf)
   
 })
 
 
 terms <- reactive({
   
   on.exit(detach("design.options")) 
   attach(list(), name="design.options") 
   d =  nomogramData()
   assign('ddist', datadist(d), pos='design.options') 
   options(datadist='ddist')
   
   f <- lrm(ZE ~  IIM + TGL + MOV1, data=d, x=TRUE, y=TRUE)
   
   
   newObs = data.frame(IIM = input$iim, TGL = input$tgl, MOV1 = input$mov1)
   
   terms = rms:::predict.lrm(f, newObs, type=c("terms"))
   
   return(terms)
   
 })
 
 
 cterms <- reactive({
   on.exit(detach("design.options")) 
   attach(list(), name="design.options") 
   d =  nomogramData()
   assign('ddist', datadist(d), pos='design.options') 
   options(datadist='ddist')
   
   f <- lrm(ZE ~  IIM + TGL + MOV1, data=d, x=TRUE, y=TRUE)
   
   
   newObs = data.frame(IIM = input$iim, TGL = input$tgl, MOV1 = input$mov1)
   
   cterms = rms:::predict.lrm(f, newObs, type=c("cterms"))
   
   return(cterms)
   
 })
 
 
 coeffs <- reactive({
   
   on.exit(detach("design.options")) 
   attach(list(), name="design.options") 
   d =  nomogramData()
   assign('ddist', datadist(d), pos='design.options') 
   options(datadist='ddist')
   
   f <- lrm(ZE ~  IIM + TGL + MOV1, data=d, x=TRUE, y=TRUE)
   
   
   newObs = data.frame(IIM = input$iim, TGL = input$tgl, MOV1 = input$mov1)
   
   cterms = rms:::predict.lrm(f, newObs, type=c("cterms"))
   terms = rms:::predict.lrm(f, newObs, type=c("terms"))
   
   coeffs = cterms/terms
   return(coeffs)
   
 })
 

 
 output$predictRisk <- renderText( {
   
   if(input$predict){

     prediction()
   }
 })
 
 output$lp <- renderText( {
   
   if(input$predict && input$lp){
     
     lp()
   }
 })
 
 
 output$adf <- DT::renderDataTable(server = FALSE, {
  if(input$predict && input$cav){
   datatable(adf(), rownames = FALSE, extensions = c('Buttons','KeyTable', 'Responsive'), options = list(
     dom = 'Bfrtip', buttons = list('copy', 'print', list(extend = 'collection',
                                      buttons = c('csv', 'excel', 'pdf'), text = 'Download')), keys = TRUE
   ))
  }
 })
 
 
 output$terms <- DT::renderDataTable(server = FALSE, {
   if(input$predict && input$t){
   datatable(terms(), rownames = FALSE, extensions = c('Buttons','KeyTable', 'Responsive'), options = list(
     dom = 'Bfrtip', buttons = list('copy', 'print', list(extend = 'collection',
                                                          buttons = c('csv', 'excel', 'pdf'), text = 'Download')), keys = TRUE
   ))
   }
 })
 
 
 output$cterms <- DT::renderDataTable(server = FALSE, {
   if(input$predict && input$ct){
   datatable(cterms(), rownames = FALSE, extensions = c('Buttons','KeyTable', 'Responsive'), options = list(
     dom = 'Bfrtip', buttons = list('copy', 'print', list(extend = 'collection',
                                                          buttons = c('csv', 'excel', 'pdf'), text = 'Download')), keys = TRUE
   ))
   }
 })
 
 
 output$coeffs <- DT::renderDataTable(server = FALSE, {
   if(input$predict){
   datatable(coeffs(), rownames = FALSE, extensions = c('Buttons','KeyTable', 'Responsive'), options = list(
     dom = 'Bfrtip', buttons = list('copy', 'print', list(extend = 'collection',
                                                          buttons = c('csv', 'excel', 'pdf'), text = 'Download')), keys = TRUE
   ))
   }
 })
 
 
 output$validationRes <- renderPrint( {
   
   if(input$predict){
     
     val
   }
 })
 
 
 output$calibrationRes <- renderPlot( {
   
   if(input$predict){
     
     plot(cal)
   }
 })
 
 
 
 
 output$nomogram = renderPlot({
   
   on.exit(detach("design.options")) 
   attach(list(), name="design.options") 
   d =  nomogramData()
   assign('ddist', datadist(d), pos='design.options') 
   options(datadist='ddist')
   
   f <- lrm(ZE ~  IIM + TGL + MOV1, data=d, x=TRUE, y=TRUE)
   
   nom <- nomogram(f, fun=function(x)1/(1+exp(-x)), fun.at=c(.001, .01, .05, seq(0.1, 0.9, by = .1), .95, .99, .999), funlabel="Zor Entübasyon Riski")
   plot(nom)
   
 })

 
 
 
 output$table1 <- renderText({
   if(input$predict){
     'Predicted risk:'
   }
 })
 
 output$table11 <- renderText({
   if(input$predict && input$lp){
     'Linear pedictor'
   }
 })
 
 output$table2 <- renderText({
   if(input$predict && input$cav){
   'Central adjustment values'
   }
 })
 
 
 output$table3 <- renderText({
   if(input$predict && input$t){
   'Terms: Linear combination of variables making up a factor'
   }
 })
 
 
 output$table4 <- renderText({
   if(input$predict && input$ct){
   'Combined terms'
   }
 })
 

  })





