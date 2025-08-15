##准备结果
########## Trunk 1 start: change: button id, function name, output id===================================================================================
# Method 0: while on treatment strategy===================================================================================

wos_32 <- eventReactive(input$B_32_surv,{

if (input$tbd_whileon){
  if(!input$scr) 
    fit1 <- surv.ICH(A=A_32(), Time=TIME_32(), cstatus=CSTATUS_32(), strategy='whileon', cov1=COV2(), method = input$meth, weights = WEIGHT()) 
  else
    fit1 <- scr.ICH(A=A_32(), Time=TIME_32(), status=CSTATUS_32(), Time_int=TIME_321(), status_int=CSTATUS_321(), strategy='whileon', cov1=COV2(), method = input$meth, weights = WEIGHT()) 

} else {fit1 <- NULL}

return(fit1)
})

wos_32_plot1 <- eventReactive(input$B_32_surv,{
  plot(wos_32(), type="ate", decrease = input$d_320, conf.int = input$conf, nboot = input$bs_320, seed = 0)
})
wos_32_plot2 <- eventReactive(input$B_32_surv,{
  plot(wos_32(), type="inc", decrease = input$d_320, conf.int = input$conf, nboot = input$bs_320, seed = 0)
})

output$wos_32a <- renderPlot({wos_32_plot1()})
output$wos_32b <- renderPlot({wos_32_plot2()})



output$bstime_324 <- renderUI({
  sliderTextInput("bstime_324", label = h5("Choose time point to estimate treatment effects"), 
    choices = wos_32()$time, grid =TRUE,
    selected = wos_32()$time[1],
    width= "100%")
})


wos_bstab_32 <- reactive({ 
if((length(wos_32())!=0) & (length(input$bstime_324)!=0)){

fit <- wos_32()
time.point <- as.numeric(input$bstime_324)
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

rownames(est) <- "While on treatment strategy"
colnames(est) <- c("Time point","Treatment effect", "SE", "95%CI", "P-value")

} else est <- NULL

return(est)
})

output$wosbs_32_tab <- renderDT({wos_bstab_32()}, options = list(scrollX = TRUE,dom = 't'))

# Trunk 1 End===================================================================================