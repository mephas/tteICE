##准备结果
########## Trunk 1 start: change: button id, function name, output id===================================================================================
# Method 0: Hypothetical strategy (I)===================================================================================

hsr_32 <- eventReactive(input$B_32_surv,{

if (input$tbd_removed){
  
  if(!input$scr) 
    fit1 <- surv.tteICE(A=A_32(), Time=TIME_32(), cstatus=CSTATUS_32(), 
      strategy='removed', cov1=COV2(), method = input$meth, weights = WEIGHT(),
      nboot = input$bs_320, seed = 0)
  else
    fit1 <- scr.tteICE(A=A_32(), Time=TIME_32(), status=CSTATUS_32(), Time_int=TIME_321(), status_int=CSTATUS_321(), 
      strategy='removed', cov1=COV2(), method = input$meth, weights = WEIGHT(),
      nboot = input$bs_320, seed = 0)
} else {fit1 <- NULL}

return(fit1)
})

hsr_32_plot1 <- eventReactive(input$B_32_surv,{
  if(length(hsr_32())!=0) plot(hsr_32(), type="ate", decrease = as.logical(input$d_320), conf.int = input$conf, ylim=input$yrange)
})
hsr_32_plot2 <- eventReactive(input$B_32_surv,{
  if(length(hsr_32())!=0) {
    plot(hsr_32(), type="inc", decrease = as.logical(input$d_320), conf.int = input$conf, ylim=input$yrangecif, 
      plot.configs=list(legend=c(input$t1, input$t0), col=c(input$col1,input$col0), show.p.value=input$adp)) 
    # if(input$adp) {
    #   p = hsr_32()$p.val
    #   if(is.null(p)) p=NA
    #   text(max(hsr_32()$time)/2, max(input$yrangecif)-0.1, paste0('P = ', round(p,input$digit_32)))
    # }
  }
})

output$hsr_32a <- renderPlot({hsr_32_plot1()})
output$hsr_32b <- renderPlot({hsr_32_plot2()})


## Prediction ---------------------------
hsrbs_32_tab_pred <- eventReactive(input$B_33_surv,{
if((length(hsr_32())!=0)){

fit <- hsr_32()
time.point <- as.numeric(input$num6)
tab <- predict(fit, timeset=time.point)
ate <- tab[5]
ate.sd <- tab[6]
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

rownames(est) <- "Hypothetical strategy (II)"
colnames(est) <- c("Time point","Treatment effect", "SE", "95%CI", "P-value per time point")

} else est <- NULL

return(est)
})

# output$hsrbs_32_tab_pred <- renderDT({hsrbs_32_tab_pred()}, options = list(scrollX = TRUE,dom = 't'))

# Trunk 1 End===================================================================================
