#**************************************************************************************************************************************************** model
sidebarLayout(

  sidebarPanel(

    HTML("<h3><b>1. 根据预览数据选择变量</b></h3>"),
    #p(br()),

    uiOutput("a_32"),
    helpText(HTML("Z：治疗方案，Z=1 表示处理，Z=0 表示对照")),
    uiOutput("cstatus_32"),
    helpText(HTML("Δ：事件种类，Δ=1 发生结局，Δ=2 发生截断事件，Δ=0 发生删失")),
    uiOutput("time_32"),
    helpText(HTML("T：生存时间，表示出现结局或者截断事件的时间")),
    uiOutput("cov1_32"),
    helpText(HTML("X：协变量，可能对结局有影响")),
    #p(br()),

dropdownButton(label = "设置小数位",
  numericInput(inputId = 'digit_32',label = '选择结果的小数位', value = 3, min = 0, max = 10, step=1),
  circle = FALSE, 
  icon = icon("gear"), width = "300px"
  # tooltip = tooltipOptions(title = "设置结果的小数位")
  ),

    hr(),

# 选择因果推断方法以及图形展示方式

HTML("<h3><b>2. 选择因果推断的方法</b></h3>"),
helpText(HTML("说明：关于方法的解释，详见<b>帮助</b>。")),

##-----------------------------------------------------------------------------------

selectInput(
  "d_320",
  label= h5("选择生存曲线类型"),
  selected = T,
  choices = list("累积事件发生曲线"=F,
                 "生存概率曲线"=T)
),

sliderInput("bs_320", label = h5("置信区间中Bootstrap重复次数"), min = 0, 
      max = 1000, value = 0),
helpText(HTML("Bootstrap次数为0时为解析解的置信区间")),

# uiOutput("method_tbd_surv"),
prettyCheckbox(
   inputId = "tbd_treatment",
   label = "方法1：疗法策略", 
    value = T,
    icon = icon("check"), 
    bigger = TRUE,
    status = "danger"
),
prettyCheckbox(
   inputId = "tbd_composite",
   label = "方法2：组合策略", 
    value = T,
    icon = icon("check"), 
    bigger = TRUE,
    status = "danger"
),
prettyCheckbox(
   inputId = "tbd_natural",
   label = "方法3：假想策略（natural）", 
    value = T,
    icon = icon("check"), 
    bigger = TRUE,
    status = "danger"
),
prettyCheckbox(
   inputId = "tbd_removed",
   label = "方法4：假想策略（removed）", 
    value = T,
    icon = icon("check"), 
    bigger = TRUE,
    status = "danger"
),
prettyCheckbox(
   inputId = "tbd_whileon",
   label = "方法5：在治策略", 
    value = T,
    icon = icon("check"), 
    bigger = TRUE,
    status = "danger"
),
prettyCheckbox(
   inputId = "tbd_principal",
   label = "方法6：主层策略", 
    value = T,
    icon = icon("check"), 
    bigger = TRUE,
    status = "danger"
),

actionButton("B_32_surv", HTML('计算估计结果'), 
             class =  "btn-danger",
             icon  = icon("chart-column")),

## 下载报告---------------------------------------------------------------------------------------------------------------
hr(),
downloadButton('downloadReport_32',
               label = "下载结果报告", 
               class =  "btn-danger",
               icon = icon("file-arrow-down")),
p(br()),
hr()
##--------------------------------------------------------------------------------
    
  ),

  mainPanel(
    h4(("预览数据")),
    helpText("注意：以下只显示数据前6行，全部数据详见“数据”页面。"),
    DTOutput("data_32"),    
    hr(),

    h4("因果推断的估计结果"),

  tabsetPanel(
    tabPanel("方法1：疗法略策",
      conditionalPanel("input.tbd_treatment",
      splitLayout(
      plotOutput("tps_32", width = 500, height = 400),plotOutput("tps_bs_32", width = 500, height = 400)
      ),
      uiOutput("bstime_320"),
      DTOutput("tpsbs_32_tab")
      )),
    tabPanel("方法2：组合略策",
      conditionalPanel("input.tbd_composite",
      splitLayout(
      plotOutput("cvs_32", width = 500, height = 400),plotOutput("cvs_bs_32", width = 500, height = 400)
      ),
      uiOutput("bstime_321"),
      DTOutput("cvsbs_32_tab")
      )),
    tabPanel("方法3：假想策略（natural）",
      conditionalPanel("input.tbd_natural",
      splitLayout(
      plotOutput("hsn_32", width = 500, height = 400),plotOutput("hsn_bs_32", width = 500, height = 400)
      ),
      uiOutput("bstime_322"),
      DTOutput("hsnbs_32_tab")
      )),
    tabPanel("方法4：假想策略（removed）",
      conditionalPanel("input.tbd_removed",
      splitLayout(
      plotOutput("hsr_32", width = 500, height = 400),plotOutput("hsr_bs_32", width = 500, height = 400)
      ),
      uiOutput("bstime_323"),
      DTOutput("hsrbs_32_tab")
      )),
    tabPanel("方法5：在治策略",
      conditionalPanel("input.tbd_whileon",
      splitLayout(
      plotOutput("wos_32", width = 500, height = 400),plotOutput("wos_bs_32", width = 500, height = 400)
      ),
      uiOutput("bstime_324"),
    DTOutput("wosbs_32_tab")
      )),
    tabPanel("方法6：主层策略",
      conditionalPanel("input.tbd_principal",
      splitLayout(
      plotOutput("pss_32", width = 500, height = 400),plotOutput("pss_bs_32", width = 500, height = 400)
      ),
      uiOutput("bstime_325"),
    DTOutput("pssbs_32_tab")
      ))
  )
    

    )

)

