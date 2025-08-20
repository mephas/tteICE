##准备结果
########## Trunk 1 start: change: button id, function name, output id===================================================================================
# Method 0: Principal stratum strategy===================================================================================

pss_32 <- eventReactive(input$B_32_surv,{

if (input$tbd_principal){
  if(!input$scr) 
    fit1 <- surv.ICH(A=A_32(), Time=TIME_32(), cstatus=CSTATUS_32(), strategy='principal', cov1=COV2(), method = input$meth, weights = WEIGHT()) 
  else
    fit1 <- scr.ICH(A=A_32(), Time=TIME_32(), status=CSTATUS_32(), Time_int=TIME_321(), status_int=CSTATUS_321(), strategy='principal', cov1=COV2(), method = input$meth, weights = WEIGHT()) 

} else {fit1 <- NULL}

return(fit1)
})

pss_32_plot1 <- eventReactive(input$B_32_surv,{
  if(length(pss_32())!=0) plot(pss_32(), type="ate", decrease = input$d_320, conf.int = input$conf, nboot = input$bs_320, seed = 0, ylim=input$yrange)
})
pss_32_plot2 <- eventReactive(input$B_32_surv,{
  if(length(pss_32())!=0) {
    plot(pss_32(), type="inc", decrease = input$d_320, conf.int = input$conf, nboot = input$bs_320, seed = 0, ylim=input$yrangecif, legend.inc=c(input$t1, input$t0))
    if(input$adp) {
      p = pss_32()$p.val
      if(is.null(p)) p=NA
      text(max(pss_32()$time)/2, max(input$yrangecif)-0.1, paste0('P = ', round(p,input$digit_32)))
    }
  }
})

output$pss_32a <- renderPlot({pss_32_plot1()})
output$pss_32b <- renderPlot({pss_32_plot2()})



# output$bstime_325 <- renderUI({
#   sliderTextInput("bstime_325", label = h5("Choose time point to estimate treatment effects"), 
#     choices = pss_32()$time, grid =TRUE,
#     selected = pss_32()$time[1],
#     width= "100%")
# })


# pss_bstab_32 <- reactive({ 
# if((length(pss_32())!=0) & (length(input$bstime_325)!=0)){

# fit <- pss_32()
# time.point <- as.numeric(input$bstime_325)
# time.pos <- which(round(fit$time,6)==round(time.point,6))
# ate <- fit$ate[time.pos]
# ate.sd <- fit$se[time.pos]
# cil = ate + qnorm((1-input$conf)/2)*ate.sd
# ciu = ate - qnorm((1-input$conf)/2)*ate.sd

# if(ate.sd==0) pvalue=NA else pvalue <- min(2*pnorm(abs(ate)/ate.sd, lower.tail = FALSE), 1)
# dgt <- paste0("(%.", input$digit_32, "f, %.", input$digit_32, "f)")
# est <- data.frame(
#   t = round(time.point, input$digit_32),
#   ate = round(ate, input$digit_32), 
#   se = round(ate.sd, input$digit_32), 
#   ci = sprintf(dgt, cil, ciu),
#   pv = round(pvalue, input$digit_32))

# rownames(est) <- "Principal stratum strategy"
# colnames(est) <- c("Time point","Treatment effect", "SE", "95%CI", "P-value per time point")

# } else est <- NULL

# return(est)
# })

# output$pssbs_32_tab <- renderDT({pss_bstab_32()}, options = list(scrollX = TRUE,dom = 't'))

## Prediction ---------------------------
pssbs_32_tab_pred <- reactive({
if((length(pss_32())!=0)){

fit <- pss_32()
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

rownames(est) <- "Principal stratum strategy"
colnames(est) <- c("Time point","Treatment effect", "SE", "95%CI", "P-value per time point")

} else est <- NULL

return(est)
})

output$pssbs_32_tab_pred <- renderDT({pssbs_32_tab_pred()}, options = list(scrollX = TRUE,dom = 't'))

# Trunk 1 End===================================================================================
