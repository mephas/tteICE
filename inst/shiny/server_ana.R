
##########- 第三部分输出结果------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


A_32 <- reactive({
  # as.numeric(DF0()[,input$a_32])
  a <- as.numeric(DF0()[,input$a_32])
  if(max(a, na.rm = TRUE)==2) a <- a-1
  return(a)
})

TIME_32 <- reactive({
  as.numeric(DF0()[,input$time_32])
})
CSTATUS_32 <- reactive({
  as.numeric(DF0()[,input$cstatus_32])
})

TIME_321 <- reactive({
  if (length(input$time_321)==0) NULL else as.numeric(DF0()[,input$time_321])
})
CSTATUS_321 <- reactive({
  if (length(input$cstatus_321)==0) NULL else as.numeric(DF0()[,input$cstatus_321])
})


COV2 <- reactive({
  if (length(input$cov1_32)==0) NULL else as.matrix(DF0()[,input$cov1_32, drop=FALSE])
})

WEIGHT <- reactive({
  if(input$wgt==FALSE) NULL 
  if(input$wgt==FALSE & length(input$weight_32)==0) NULL 
  if(input$wgt=="IPW") {
    ps = predict(glm(A_32() ~ COV2(), family='binomial'), type='response')
    w = A_32()/ps*mean(A_32()/ps) + (1-A_32())/(1-ps)*mean((1-A_32())/(1-ps))
  }
  if((input$wgt=="var") & !length(input$weight_32)==0) as.numeric(DF0()[,input$weight_32])
})





