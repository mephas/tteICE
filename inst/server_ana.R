
##########- 第三部分输出结果------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


A_32 <- reactive({
  # as.numeric(DF0()[,input$a_32])
  a <- as.numeric(DF0()[,input$a_32])
  if(max(a, na.rm = T)==2) a <- a-1
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
  if (length(input$cov1_32)==0) NULL else as.matrix(DF0()[,input$cov1_32])
})

WEIGHT <- reactive({
  if (length(input$weight_32)==0) NULL else as.matrix(DF0()[,input$weight_32])
})

##准备结果
########## Trunk 1 start: change: button id, function name, output id===================================================================================
# Method 0: treatment policy plot===================================================================================
tps_32 <- eventReactive(input$B_32_surv,{

if (input$tbd_treatment){
  fit1 <- surv.ICH(
    A=A_32(), 
    Time=TIME_32(), 
    cstatus=CSTATUS_32(),
    strategy='treatment',
    cov1=COV2(),
    method = input$meth,
    weights = WEIGHT
  )
  fit2 <- surv.boot(fit1, nboot=input$bs_320, seed=0)
  time <- fit2$Time
} else {fit1 <- fit2 <- time <- NULL}

return(list(fit1 = fit1, fit2 = fit2, time = time))
})

tps_32_plot <- eventReactive(input$B_32_surv,{
  plot.inc(tps_32()$fit1, decrease = input$d_320)
})


output$bstime_320 <- renderUI({
  sliderTextInput("bstime_320", label = h5("Choose time point to estimate treatment effects"), 
    choices = sort(unique(tps_32()$time)), grid =TRUE,
    selected = sort(unique(tps_32()$time))[2],
    width= "100%")
})

tps_bstab_32 <- reactive({ 
if(length(tps_32()$fit2)!=0){

fit <- tps_32()$fit2
time.point <- as.numeric(input$bstime_320)
time.pos <- which(round(fit$Time,6)==round(time.point,6))
ate <- fit$ate[time.pos]
ate.sd <- fit$se[time.pos]
conf.int <- 0.95
cil = ate + qnorm((1-conf.int)/2)*ate.sd
ciu = ate - qnorm((1-conf.int)/2)*ate.sd
pvalue <- min(2*pnorm(abs(ate)/ate.sd, lower.tail = FALSE), 1)

  ## define digits
dgt <- paste0("(%.", input$digit_32, "f, %.", input$digit_32, "f)")
est <- data.frame(
  t = round(time.point, input$digit_32),
  ate = round(ate, input$digit_32), 
  se = round(ate.sd, input$digit_32), 
  ci = sprintf(dgt, cil, ciu),
  pv = round(pvalue, input$digit_32))

rownames(est) <- "Treatment policy strategy"
colnames(est) <- c("Time point","Treatment effect", "SE", "95%CI", "Pvalue")

} else est <- NULL

return(est)
})

tpsbs_32_plot <- eventReactive(input$B_32_surv,{
  plot.boot(tps_32()$fit1, nboot = input$bs_320, seed = 123)
})

output$tps_32 <- renderPlot({
  tps_32_plot()
})
output$tps_bs_32 <- renderPlot({
  tpsbs_32_plot()
})
output$tpsbs_32_tab <- DT::renderDT(
{tps_bstab_32()},
options = list(scrollX = TRUE,dom = 't'))

# Trunk 1 End===================================================================================

# Method 1: CVS plot===================================================================================
cvs_32 <- eventReactive(input$B_32_surv,{

if (input$tbd_composite){
  fit1 <- surv.ICH(
  A=A_32(), 
  Time=TIME_32(), 
  cstatus=CSTATUS_32(),
  strategy='composite',
  cov1=COV2()
  )
fit2 <- surv.boot(fit1,nboot=input$bs_320,seed=123)
time <- fit2$Time
} else {fit1 <- fit2 <- time <- NULL}

return(list(fit1 = fit1, fit2 = fit2, time = time))
})

cvs_32_plot <- eventReactive(input$B_32_surv,{
  plot.inc(cvs_32()$fit1,decrease = input$d_320)
})


output$bstime_321 <- renderUI({
  sliderTextInput("bstime_321", label = h5("选择时间点，计算因果估计结果"), 
    choices = sort(unique(cvs_32()$time)), grid =TRUE,
    selected = sort(unique(cvs_32()$time))[2],
    width= "100%")
})

cvs_bstab_32 <- reactive({ 
if(length(cvs_32()$fit2)!=0){

fit <- cvs_32()$fit2
time.point <- as.numeric(input$bstime_321)
time.pos <- which(round(fit$Time,6)==round(time.point,6))
ate <- fit$ate[time.pos]
ate.sd <- fit$se[time.pos]
conf.int <- 0.95
cil = ate + qnorm((1-conf.int)/2)*ate.sd
ciu = ate - qnorm((1-conf.int)/2)*ate.sd
pvalue <- min(2*pnorm(abs(ate)/ate.sd, lower.tail = FALSE), 1)

  ## define digits
dgt <- paste0("(%.", input$digit_32, "f, %.", input$digit_32, "f)")
est <- data.frame(
  t = round(time.point, input$digit_32),
  ate = round(ate, input$digit_32), 
  se = round(ate.sd, input$digit_32), 
  ci = sprintf(dgt, cil, ciu),
  pv = round(pvalue, input$digit_32))
rownames(est) <- "组合策略"
colnames(est) <- c("选取的时间点","平均因果作用的估计值", "参数估计的标准误差",
  "95%置信区间", "P值")

# estimate <- cat(sprintf("%.3f (%.3f, %.3f), P value = %.3f", 
#                         ate, cil, ciu, pvalue))
} else est <- NULL

return(est)
})

cvsbs_32_plot <- eventReactive(input$B_32_surv,{
  plot.boot(cvs_32()$fit1, nboot = input$bs_320, seed = 123)
})

output$cvs_32 <- renderPlot({
  cvs_32_plot()
})
output$cvs_bs_32 <- renderPlot({
  cvsbs_32_plot()
})
output$cvsbs_32_tab <- DT::renderDT(
{cvs_bstab_32()},
options = list(scrollX = TRUE,dom = 't'))

# # Trunk 1 End===================================================================================

# ########## Trunk 2 start: change: button id, function name, output id===================================================================================
# # Method 2: HSN plot===================================================================================
hsn_32 <- eventReactive(input$B_32_surv,{

if (input$tbd_natural){
  fit1 <- surv.ICH(
  A=A_32(), 
  Time=TIME_32(), 
  cstatus=CSTATUS_32(),
  strategy='natural',
  cov1=COV2()
  )
  fit2 <- surv.boot(fit1,nboot=input$bs_320,seed=123)
  time <- fit2$Time
} else {fit1 <- fit2 <- time <- NULL}

return(list(fit1 = fit1, fit2 = fit2, time = time))
})

hsn_32_plot <- eventReactive(input$B_32_surv,{
  plot.inc(hsn_32()$fit1,decrease = input$d_320)
})


output$bstime_322 <- renderUI({
  sliderTextInput("bstime_322", label = h5("选择时间点，计算因果估计结果"), 
    choices = sort(unique(hsn_32()$time)), grid =TRUE,
    selected = sort(unique(hsn_32()$time))[2],
    width= "100%")
})


hsn_bstab_32 <- reactive({ 
if(length(hsn_32()$fit2)!=0){

fit <- hsn_32()$fit2
time.point <- as.numeric(input$bstime_322)
time.pos <- which(round(fit$Time,6)==round(time.point,6))
ate <- fit$ate[time.pos]
ate.sd <- fit$se[time.pos]
conf.int <- 0.95
cil = ate + qnorm((1-conf.int)/2)*ate.sd
ciu = ate - qnorm((1-conf.int)/2)*ate.sd
pvalue <- min(2*pnorm(abs(ate)/ate.sd, lower.tail = FALSE), 1)

  ## define digits
dgt <- paste0("(%.", input$digit_32, "f, %.", input$digit_32, "f)")
est <- data.frame(
  t = round(time.point, input$digit_32),
  ate = round(ate, input$digit_32), 
  se = round(ate.sd, input$digit_32), 
  ci = sprintf(dgt, cil, ciu),
  pv = round(pvalue, input$digit_32))
rownames(est) <- "假想策略"
colnames(est) <- c("选取的时间点", "平均因果作用的估计值", "参数估计的标准误差",
  "95%置信区间", "P值")

} else est <- NULL
return(est)
})

hsnbs_32_plot <- eventReactive(input$B_32_surv,{
  plot.boot(hsn_32()$fit1,nboot = input$bs_320, seed = 123)
})

output$hsn_32 <- renderPlot({
  hsn_32_plot()
})
output$hsn_bs_32 <- renderPlot({
  hsnbs_32_plot()
})
output$hsnbs_32_tab <- DT::renderDT(
{hsn_bstab_32()},
options = list(scrollX = TRUE,dom = 't'))
# # Trunk 2 End===================================================================================

# ########## Trunk 3 start: change: button id, function name, output id===================================================================================
# # Method 3: HSR plot===================================================================================
hsr_32 <- eventReactive(input$B_32_surv,{

if (input$tbd_removed){
  fit1 <- surv.ICH(
  A=A_32(), 
  Time=TIME_32(), 
  cstatus=CSTATUS_32(),
  strategy='removed',
  cov1=COV2()
  )
fit2 <- surv.boot(fit1,nboot=input$bs_320,seed=123)
  time <- fit2$Time
} else {fit1 <- fit2 <- time <- NULL}

return(list(fit1 = fit1, fit2 = fit2, time = time))
})

hsr_32_plot <- eventReactive(input$B_32_surv,{
  plot.inc(hsr_32()$fit1,decrease = input$d_320)
})


output$bstime_323 <- renderUI({
  sliderTextInput("bstime_323", label = h5("选择时间点，计算因果估计结果"), 
    choices = sort(unique(hsr_32()$time)), grid =TRUE,
    selected = sort(unique(hsr_32()$time))[2],
    width= "100%")
})

hsr_bstab_32 <- reactive({ 
if(length(hsr_32()$fit2)!=0){

fit <- hsr_32()$fit2
time.point <- as.numeric(input$bstime_323)
time.pos <- which(round(fit$Time,6)==round(time.point,6))
ate <- fit$ate[time.pos]
ate.sd <- fit$se[time.pos]
conf.int <- 0.95
cil = ate + qnorm((1-conf.int)/2)*ate.sd
ciu = ate - qnorm((1-conf.int)/2)*ate.sd
pvalue <- min(2*pnorm(abs(ate)/ate.sd, lower.tail = FALSE), 1)

  ## define digits
dgt <- paste0("(%.", input$digit_32, "f, %.", input$digit_32, "f)")
est <- data.frame(
  t = round(time.point, input$digit_32),
  ate = round(ate, input$digit_32), 
  se = round(ate.sd, input$digit_32), 
  ci = sprintf(dgt, cil, ciu),
  pv = round(pvalue, input$digit_32))
rownames(est) <- "假想策略"
colnames(est) <- c("选取的时间点","平均因果作用的估计值", "参数估计的标准误差",
  "95%置信区间", "P值")

} else est <- NULL
return(est)
})

hsrbs_32_plot <- eventReactive(input$B_32_surv,{
  plot.boot(hsr_32()$fit1,nboot = input$bs_320, seed = 123)
})

output$hsr_32 <- renderPlot({
  hsr_32_plot()
})
output$hsr_bs_32 <- renderPlot({
  hsrbs_32_plot()
})
output$hsrbs_32_tab <- DT::renderDT(
{hsr_bstab_32()},
options = list(scrollX = TRUE,dom = 't'))
# # Trunk 3 End===================================================================================

# ########## Trunk 4 start: change: button id, function name, output id===================================================================================
# # Method 4: WOS plot===================================================================================
wos_32 <- eventReactive(input$B_32_surv,{

if (input$tbd_whileon){
  fit1 <- surv.ICH(
  A=A_32(), 
  Time=TIME_32(), 
  cstatus=CSTATUS_32(),
  strategy='whileon',
  cov1=COV2()
  )
fit2 <- surv.boot(fit1,nboot=input$bs_320,seed=123)
  time <- fit2$Time
} else {fit1 <- fit2 <- time <- NULL}

return(list(fit1 = fit1, fit2 = fit2, time = time))
})

wos_32_plot <- eventReactive(input$B_32_surv,{
  plot.inc(wos_32()$fit1,decrease = input$d_320)
})

output$bstime_324 <- renderUI({
  sliderTextInput("bstime_324", label = h5("选择时间点，计算因果估计结果"), 
    choices = sort(unique(wos_32()$time)), grid =TRUE,
    selected = sort(unique(wos_32()$time))[2],
    width= "100%")
})
wos_bstab_32 <- reactive({ 
if(length(wos_32()$fit2)!=0){

fit <- wos_32()$fit2
time.point <- as.numeric(input$bstime_324)
time.pos <- which(round(fit$Time,6)==round(time.point,6))
ate <- fit$ate[time.pos]
ate.sd <- fit$se[time.pos]
conf.int <- 0.95
cil = ate + qnorm((1-conf.int)/2)*ate.sd
ciu = ate - qnorm((1-conf.int)/2)*ate.sd
pvalue <- min(2*pnorm(abs(ate)/ate.sd, lower.tail = FALSE), 1)

  ## define digits
dgt <- paste0("(%.", input$digit_32, "f, %.", input$digit_32, "f)")
est <- data.frame(
  t = round(time.point, input$digit_32),
  ate = round(ate, input$digit_32), 
  se = round(ate.sd, input$digit_32), 
  ci = sprintf(dgt, cil, ciu),
  pv = round(pvalue, input$digit_32))
rownames(est) <- "在治策略"
colnames(est) <- c("选取的时间点", "平均因果作用的估计值", "参数估计的标准误差",
  "95%置信区间", "P值")

} else est <- NULL
return(est)
})

wosbs_32_plot <- eventReactive(input$B_32_surv,{
  plot.boot(wos_32()$fit1, nboot = input$bs_320, seed = 123)
})

output$wos_32 <- renderPlot({
  wos_32_plot()
})
output$wos_bs_32 <- renderPlot({
  wosbs_32_plot()
})
output$wosbs_32_tab <- DT::renderDT(
{wos_bstab_32()},
options = list(scrollX = TRUE,dom = 't'))
# # Trunk 4 End===================================================================================

# ########## Trunk 5 start: change: button id, function name, output id===================================================================================
# # Method 5: PSS plot===================================================================================
pss_32 <- eventReactive(input$B_32_surv,{

if (input$tbd_principal){
  fit1 <- surv.ICH(
  A=A_32(), 
  Time=TIME_32(), 
  cstatus=CSTATUS_32(),
  strategy='principal',
  cov1=COV2()
  )
fit2 <- surv.boot(fit1,nboot=input$bs_320,seed=123)
  time <- fit2$Time
} else {fit1 <- fit2 <- time <- NULL}

return(list(fit1 = fit1, fit2 = fit2, time = time))
})

pss_32_plot <- eventReactive(input$B_32_surv,{
  plot.inc(pss_32()$fit1,decrease = input$d_320)
})

output$bstime_325 <- renderUI({
  sliderTextInput("bstime_325", label = h5("选择时间点，计算因果估计结果"), 
    choices = sort(unique(pss_32()$time)), grid =TRUE,
    selected = sort(unique(pss_32()$time))[2],
    width= "100%")
})

pss_bstab_32 <- reactive({ 
if(length(pss_32()$fit2)!=0){

fit <- pss_32()$fit2
time.point <- as.numeric(input$bstime_325)
time.pos <- which(round(fit$Time,6)==round(time.point,6))
ate <- fit$ate[time.pos]
ate.sd <- fit$se[time.pos]
conf.int <- 0.95
cil = ate + qnorm((1-conf.int)/2)*ate.sd
ciu = ate - qnorm((1-conf.int)/2)*ate.sd
pvalue <- min(2*pnorm(abs(ate)/ate.sd, lower.tail = FALSE), 1)

  ## define digits
dgt <- paste0("(%.", input$digit_32, "f, %.", input$digit_32, "f)")
est <- data.frame(
  t = round(time.point, input$digit_32),
  ate = round(ate, input$digit_32), 
  se = round(ate.sd, input$digit_32), 
  ci = sprintf(dgt, cil, ciu),
  pv = round(pvalue, input$digit_32)
  )
rownames(est) <- "主层策略"
colnames(est) <- c("选取的时间点","平均因果作用的估计值", "参数估计的标准误差",
  "95%置信区间", "P值")

} else est <- NULL

return(est)
})

pssbs_32_plot <- eventReactive(input$B_32_surv,{
  plot.boot(pss_32()$fit1, nboot = input$bs_320, seed = 123)
})

# Output CVS and BS plot End===================================================================================
output$pss_32 <- renderPlot({
  pss_32_plot()
})
output$pss_bs_32 <- renderPlot({
  pssbs_32_plot()
})
output$pssbs_32_tab <- DT::renderDT(
{pss_bstab_32()},
options = list(scrollX = TRUE,dom = 't'))
# Trunk 5 End===================================================================================



