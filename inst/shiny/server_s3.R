##准备结果
########## Trunk 1 start: change: button id, function name, output id===================================================================================
# Method 0: treatment policy plot===================================================================================

hsn_32 <- eventReactive(input$B_32_surv,{

if (input$tbd_natural){
  if(!input$scr)
    fit1 <- surv.tteICE(A=A_32(), Time=TIME_32(), cstatus=CSTATUS_32(), strategy='natural', cov1=COV2(), method = input$meth, weights = WEIGHT())
  else
    fit1 <- scr.tteICE(A=A_32(), Time=TIME_32(), status=CSTATUS_32(), Time_int=TIME_321(), status_int=CSTATUS_321(), strategy='natural', cov1=COV2(), method = input$meth, weights = WEIGHT())

} else {fit1 <- NULL}

return(fit1)
})

hsn_32_plot1 <- eventReactive(input$B_32_surv,{
  if(length(hsn_32())!=0) plot(hsn_32(), type="ate", decrease = as.logical(input$d_320), conf.int = input$conf, nboot = input$bs_320, seed = 0, ylim=input$yrange)
})
hsn_32_plot2 <- eventReactive(input$B_32_surv,{
  if(length(hsn_32())!=0) {
    plot(hsn_32(), type="inc", decrease = as.logical(input$d_320), conf.int = input$conf, nboot = input$bs_320, seed = 0, ylim=input$yrangecif, 
      plot.configs=list(legend=c(input$t1, input$t0), show.p.value=input$adp)) 
    # if(input$adp) {
    #   p = hsn_32()$p.val
    #   if(is.null(p)) p=NA
    #   text(max(hsn_32()$time)/2, max(input$yrangecif)-0.1, paste0('P = ', round(p,input$digit_32)))
    # }
  }
})

output$hsn_32a <- renderPlot({hsn_32_plot1()})
output$hsn_32b <- renderPlot({hsn_32_plot2()})

## Prediction ---------------------------
hsnbs_32_tab_pred <- eventReactive(input$B_33_surv,{
if((length(hsn_32())!=0)){

fit <- hsn_32()
time.point <- as.numeric(input$num6)
tab <- riskpredict(fit, timeset=time.point, nboot=input$bs_320, seed=0)
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

rownames(est) <- "Hypothetical strategy (I)"
colnames(est) <- c("Time point","Treatment effect", "SE", "95%CI", "P-value per time point")

} else est <- NULL

return(est)
})

# output$hsnbs_32_tab_pred <- renderDT({hsnbs_32_tab_pred()}, options = list(scrollX = TRUE,dom = 't'))

# Trunk 1 End===================================================================================
