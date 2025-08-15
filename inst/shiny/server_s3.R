##准备结果
########## Trunk 1 start: change: button id, function name, output id===================================================================================
# Method 0: treatment policy plot===================================================================================

hsn_32 <- eventReactive(input$B_32_surv,{

if (input$tbd_natural){
  if(!input$scr) 
    fit1 <- surv.ICH(A=A_32(), Time=TIME_32(), cstatus=CSTATUS_32(), strategy='natural', cov1=COV2(), method = input$meth, weights = WEIGHT()) 
  else
    fit1 <- scr.ICH(A=A_32(), Time=TIME_32(), status=CSTATUS_32(), Time_int=TIME_321(), status_int=CSTATUS_321(), strategy='natural', cov1=COV2(), method = input$meth, weights = WEIGHT()) 

} else {fit1 <- NULL}

return(fit1)
})

hsn_32_plot1 <- eventReactive(input$B_32_surv,{
  if(length(hsn_32())!=0) plot(hsn_32(), type="ate", decrease = input$d_320, conf.int = input$conf, nboot = input$bs_320, seed = 0)
  else NULL
})
hsn_32_plot2 <- eventReactive(input$B_32_surv,{
  if(length(hsn_32())!=0) plot(hsn_32(), type="inc", decrease = input$d_320, conf.int = input$conf, nboot = input$bs_320, seed = 0)
  else NULL
})

output$hsn_32a <- renderPlot({hsn_32_plot1()})
output$hsn_32b <- renderPlot({hsn_32_plot2()})



output$bstime_322 <- renderUI({
  sliderTextInput("bstime_322", label = h5("Choose time point to estimate treatment effects"), 
    choices = hsn_32()$time, grid =TRUE,
    selected = hsn_32()$time[1],
    width= "100%")
})


hsn_bstab_32 <- reactive({ 
if((length(hsn_32())!=0) & (length(input$bstime_322)!=0)){

fit <- hsn_32()
time.point <- as.numeric(input$bstime_322)
time.pos <- which(round(fit$time,6)==round(time.point,6))
ate <- fit$ate[time.pos]
ate.sd <- fit$se[time.pos]
cil = ate + qnorm((1-input$conf)/2)*ate.sd
ciu = ate - qnorm((1-input$conf)/2)*ate.sd

if(ate.sd==0) pvalue=NA else pvalue <- min(2*pnorm(abs(ate)/ate.sd, lower.tail = FALSE), 1)
dgt <- paste0("(%.", input$digit_32, "f, %.", input$digit_32, "f)")
est <- data.frame(
  t = round(time.point, input$digit_32),
  ate = round(ate, input$digit_32), 
  se = round(ate.sd, input$digit_32), 
  ci = sprintf(dgt, cil, ciu),
  pv = round(pvalue, input$digit_32))

rownames(est) <- "Hypothetical strategy (I)"
colnames(est) <- c("Time point","Treatment effect", "SE", "95%CI", "P-value")

} else est <- NULL

return(est)
})

output$hsnbs_32_tab <- renderDT({hsn_bstab_32()}, options = list(scrollX = TRUE,dom = 't'))

# Trunk 1 End===================================================================================