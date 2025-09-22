##准备结果
########## Trunk 1 start: change: button id, function name, output id===================================================================================
# Method 0: Principal stratum strategy===================================================================================

pss_32 <- eventReactive(input$B_32_surv,{

if (input$tbd_principal){
  if(!input$scr) 
    fit1 <- surv.tteICE(A=A_32(), Time=TIME_32(), cstatus=CSTATUS_32(), strategy='principal', cov1=COV2(), method = input$meth, weights = WEIGHT()) 
  else
    fit1 <- scr.tteICE(A=A_32(), Time=TIME_32(), status=CSTATUS_32(), Time_int=TIME_321(), status_int=CSTATUS_321(), strategy='principal', cov1=COV2(), method = input$meth, weights = WEIGHT()) 

} else {fit1 <- NULL}

return(fit1)
})

pss_32_plot1 <- eventReactive(input$B_32_surv,{
  if(length(pss_32())!=0) plot(pss_32(), type="ate", decrease = as.logical(input$d_320), conf.int = input$conf, nboot = input$bs_320, seed = 0, ylim=input$yrange)
})
pss_32_plot2 <- eventReactive(input$B_32_surv,{
  if(length(pss_32())!=0) {
    plot(pss_32(), type="inc", decrease = as.logical(input$d_320), conf.int = input$conf, nboot = input$bs_320, seed = 0, ylim=input$yrangecif, 
      plot.configs=list(legend=c(input$t1, input$t0), show.p.value=input$adp)) 
    # if(input$adp) {
    #   p = pss_32()$p.val
    #   if(is.null(p)) p=NA
    #   text(max(pss_32()$time)/2, max(input$yrangecif)-0.1, paste0('P = ', round(p,input$digit_32)))
    # }
  }
})

output$pss_32a <- renderPlot({pss_32_plot1()})
output$pss_32b <- renderPlot({pss_32_plot2()})

## Prediction ---------------------------
pssbs_32_tab_pred <- eventReactive(input$B_33_surv,{
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


# output$pssbs_32_tab_pred <- renderDT({pssbs_32_tab_pred()}, options = list(scrollX = TRUE,dom = 't'))

# Trunk 1 End===================================================================================

fin.res <- eventReactive(input$B_33_surv,{

if(input$tbd_treatment) dat1 <- tpsbs_32_tab_pred() else dat1 <- NULL
if(input$tbd_composite) dat2 <- cvsbs_32_tab_pred() else dat2 <- NULL
if(input$tbd_natural) dat3 <- hsnbs_32_tab_pred() else dat3 <- NULL
if(input$tbd_removed) dat4 <- hsrbs_32_tab_pred() else dat4 <- NULL
if(input$tbd_whileon) dat5 <- wosbs_32_tab_pred() else dat5 <- NULL
if(input$tbd_principal) dat6 <- pssbs_32_tab_pred() else dat6 <- NULL

rbind.data.frame(dat1,dat2,dat3,dat4,dat5,dat6)
})

output$fin.res <- renderDT({fin.res()}, 
  options = 
  list(
    scrollX = TRUE,
    dom = 'Bfrtip',
    buttons =list("copy",
        list(extend = "csv", title = "ICEresults"),
        list(extend = "excel", title = "ICEresults")
      )))

output$myText <- renderText({
    paste0("At time ", input$num6, ", the estimated treatment effect is shown as `Treatment effect` with confidence intervals (`95%CI`).
      If P-value is less than 0.05, the estimated treatment effect is significantly different from 0, suggesting evidence that the treatment is effective.
      If P-value is greater than or equal to 0.05, the treatment effect is not statistically significant, indicating insufficient evidence to conclude that the treatment has an effect.")
  })