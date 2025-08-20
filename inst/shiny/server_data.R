#****************************************************************************************************************************************************

load("bmt-shiny.RData")

data <- reactive({
  switch(input$edata,
    "bmt" = bmt,
    "NULL" = NULL
  )
})

global_label <- reactiveValues(label = 0)

# 示例数据 shiny默认展示示例数据，label=0
observeEvent(input$edata,{
  csv <- input$edata
  if(!is.null(csv)){
    global_label$label <- 0
  }
})

observeEvent(input$file,{  #上传csv、txt文件
   csv <- input$file
   if(!is.null(csv)){
     global_label$label <- 1
   }
})

DF0 <- reactive({
  gl.var <- global_label$label

  inFile <- input$file
  if (gl.var == 1) {  #上传csv、txt文件
    if (!input$col) {
      csv <- read.csv(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote, stringsAsFactors = TRUE)
    } else {
      csv <- read.csv(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote, row.names = 1, stringsAsFactors = TRUE)
    }
    validate(need(ncol(csv) > 1, "Check data format"))
    validate(need(nrow(csv) > 1, "Check data format"))

    x <- as.data.frame(csv)

    return(x)
  }

  if (gl.var == 0) {
    x <- bmt
    return(data())
  }

})



output$Xdata <- renderDT(DF0(),
  extensions = list(
    "Buttons" = NULL,
    "Scroller" = NULL
  ),
  options = list(
    dom = "Bfrtip",
    buttons =
      list(
        "copy",
        list(extend = "csv", title = "DataPreview"),
        list(extend = "excel", title = "DataPreview")
      ),
    deferRender = TRUE,
    scrollX = TRUE,
    scrollY = 300,
    scroller = TRUE
  )
)


## 选出数值型的变量
type.num0 <- reactive({
  colnames(DF0()[unlist(lapply(DF0(), is.numeric))])
})


output$sum <- renderDT({if(!is.null(DF0())) .desc.numeric(DF0())},
  extensions = list(
    "Buttons" = NULL,
    "Scroller" = NULL
  ),
  options = list(
    dom = "Bfrtip",
    buttons =
      list(
        "copy",
        list(extend = "csv", title = "DescriptiveStatisticsNum"),
        list(extend = "excel", title = "DescriptiveStatisticsNum")
      ),
    deferRender = TRUE,
    scrollX = TRUE,
    scrollY = 200,
    scroller = TRUE
  )
)

output$fsum <- renderDT({if(!is.null(DF0())) .desc.factor(DF0())},
  extensions = list(
    "Buttons" = NULL,
    "Scroller" = NULL
  ),
  options = list(
    dom = "Bfrtip",
    buttons =
      list(
        "copy",
        list(extend = "csv", title = "DescriptiveStatisticsCate"),
        list(extend = "excel", title = "DescriptiveStatisticsCate")
      ),
    deferRender = TRUE,
    scrollX = TRUE,
    scrollY = 200,
    scroller = TRUE
  )
)

##-----------------------------------------------------------------------------------
##-----------------------------------------------------------------------------------
var.type.list0 <- reactive({
  if(!is.null(DF0())) .var.class(DF0())
})

type.bi.z <- reactive({
  var.type.list0 <- var.type.list0()
  binary_names <- colnames(DF0()[,var.type.list0()[,1] %in% "binary", drop=FALSE])
  #Adam数据，需要"TRT01AN"为默认选择的列名， 查找包含 "TRT01AN" 的列名
  target_columns <- binary_names[grep("TRT01AN", binary_names)]
  # 将找到的列名移到向量的开头
  sorted_col_names <- c(target_columns, binary_names[!binary_names %in% target_columns])
  return(sorted_col_names)
  })

##########- 第一部分变量选择------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## choose binary vars from type.bi.z()

output$a_32 <- renderUI({
  selectInput(
    "a_32",
    label= h5("*Treatment variable (a binary one)"),
    choices = type.bi.z()
  )
})

type.num_32 <- reactive({
  names(DF0()[sapply(DF0(), is.numeric)])
  })

var.time1 <- reactive({
  type.num_32()[!(type.num_32() %in% c(input$a_32))]
  })


output$time_32 <- renderUI({
  selectInput(
    "time_32",
    h5("*Time to the primary event"),
    choices = var.time1()
  )
})

type.num.z2 <- reactive({
  type.num_32()[!(type.num_32() %in% c(input$a_32, input$time_32))]
})

output$cstatus_32 <- renderUI({
  selectInput(
    "cstatus_32",
    h5("*Primary event indicator (a binary one)"),
    choices = type.num.z2()
  )
})

var.time2 <- reactive({
  type.num_32()[!(type.num_32() %in% c(input$a_32, input$time_32, input$cstatus_32))]
  })

output$time_321 <- renderUI({
  selectInput(
    "time_321",
    h5("*Time to the intercurrent event"),
    choices = var.time2()
  )
})

type.bi.z3 <- reactive({
  type.bi.z()[!(type.bi.z() %in% c(input$a_32, input$time_32, input$cstatus_32, input$time_321))]
})

output$cstatus_321 <- renderUI({
  selectInput(
    "cstatus_321",
    h5("*Intercurrent event indicator (a binary one)"),
    choices = type.bi.z3()
  )
})

type.num3_32 <- reactive({
  if(input$scr) type.num_32()[!(type.num_32() %in% c(input$a_32, input$time_32, input$cstatus_32, input$time_321, input$cstatus_321))]
  else type.num_32()[!(type.num_32() %in% c(input$a_32, input$time_32, input$cstatus_32))]
})

output$cov1_32 <- renderUI({
  pickerInput(
    "cov1_32",
    h5("Covariates"),
    selected = NULL,
    choices  = type.num3_32(),
    multiple = TRUE,
    options = pickerOptions(
      actionsBox = TRUE,
      liveSearch = TRUE,  # 启用搜索
      dropdownAlignRight = "auto",
      dropupAuto = FALSE)
  )
})

type.num4_32 <- reactive({
  type.num_32()[!(type.num_32() %in% c(input$a_32, input$time_32, input$cstatus_32, input$time_321, input$cstatus_321))]
  # names(DF2_32()[sapply(DF2_32(), is.numeric)])
})

output$weight_32 <- renderUI({
  pickerInput(
    "weight_32",
    h5("Weight variable"),
    choices  = type.num4_32(),
    selected = NULL,
    multiple = TRUE,
    options = pickerOptions(
      actionsBox = TRUE,
      liveSearch = TRUE,  # 启用搜索
      dropdownAlignRight = "auto",
      dropupAuto = FALSE)
  )
})
