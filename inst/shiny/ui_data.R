#****************************************************************************************************************************************************

sidebarLayout(

sidebarPanel(

  # tags$head(tags$style("#strnum {overflow-y:scroll; max-height: 200px; background: white};")),
  # tags$head(tags$style("#strfac {overflow-y:scroll; max-height: 100px; background: white};")),

##-----------------------------------------------------------------------------------
##-----------------------------------------------------------------------------------

h3("Step 1. Prepare analytical dataset"),

  prettyRadioButtons(
    inputId = "edata",
    label =  h5("Example dataset"),
    choices =  list("bmt" = "bmt", "Upload my own data"="NULL"),
    selected = "bmt",
    icon = icon("database"),
    status = "danger"),

  conditionalPanel(
  condition = "input.edata=='NULL'",
  helpText("Refer to the format of the example data to upload new data"),

  tabsetPanel(
    tabPanel("Upload data", br(), 
      fileInput("file", "Choose a CSV/TXT file", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")) 
      ),
    tabPanel("Settings for upload", br(),
      p("The values in the first row are used as variable names"),
      prettyToggle(
          inputId = "header",
          label_on = "Yes",
          icon_on = icon("check"),
          status_on = "info",
          status_off = "warning",
          label_off = "No",
          icon_off = icon("remove"),
          value = TRUE),
        p("The values in the leftmost column are used as row names"),
        prettyToggle(
          inputId = "col",
          label_on = "Yes",
          icon_on = icon("check"),
          status_on = "info",
          status_off = "warning",
          label_off = "No",
          icon_off = icon("remove"),
          value = FALSE),
        p("The field separator character"),
        prettyRadioButtons(
          inputId = "sep",
          status = "info",
          label=NULL,
          fill = TRUE,
          icon = icon("check"),
          choiceNames = list(
            HTML("Comma (,): the default for the CSV file"),
            HTML("Tab (->)：the default for the TXT file"),
            HTML("Semicolon (;)"),
            HTML("Space (_)")
          ),
          choiceValues = list(",", "\t", ";", " "))
      )
    ) 
  ),

hr(),

##-----------------------------------------------------------------------------------
##-----------------------------------------------------------------------------------

h3("Step 2. Causal analysis"),

h4("Choose variables"),
uiOutput("a_32"),
helpText(HTML("Treatment assignment; 1 for treatment, 0 for control")),

uiOutput("time_32"),
helpText(HTML("Time to the primary (terminal) event")),

uiOutput("cstatus_32"),
helpText(HTML("Primary (terminal) event indicator，1 for the primary (terminal) event, 0 for censoring")),

h4("Whether to collect time to intercurrent events (semicompeting risks)"),
prettyToggle(
          inputId = "scr",
          label_on = "Yes",
          icon_on = icon("check"),
          status_on = "info",
          status_off = "warning",
          label_off = "No",
          icon_off = icon("remove"),
          value = FALSE),
conditionalPanel(
  condition = "input.scr",
uiOutput("time_321"),
helpText(HTML("Time to the intercurrent event ")),

uiOutput("cstatus_321"),
helpText(HTML("Intercurrent event indicator，1 for the intercurrent event, 0 for censoring"))
),


uiOutput("cov1_32"),
helpText(HTML("Baseline covariates that need to be controlled")),

h4("Whether to assign weight or set a weight variable"),
prettyRadioButtons(
          inputId = "wgt",
          status = "info",
          label=NULL,
          fill = TRUE,
          icon = icon("check"),
          choiceNames = list(
            HTML("No"),
            HTML("Use inverse probability weighting"),
            HTML("Assign a weight variable")
          ),
          choiceValues = list(FALSE,"IPW", "var")),

conditionalPanel(
  condition = "input.wgt=='var'",
uiOutput("weight_32"),
helpText(HTML("Weight for each subject"))),

# uiOutput("sub_32"),
# helpText(HTML("Subset variable to define the subset of data")),

dropdownButton(label = "Settings of digits",
  numericInput(inputId = 'digit_32',label = 'decimal', value = 3, min = 0, max = 10, step=1),
  circle = FALSE, 
  icon = icon("gear"), width = "300px"
  ),

hr(),

h4("Choose strategy"),

prettyCheckbox(
   inputId = "tbd_treatment",
   label = "1: Treatment policy strategy", 
    value = T,
    icon = icon("check"), 
    bigger = TRUE,
    status = "danger"
),
prettyCheckbox(
   inputId = "tbd_composite",
   label = "2: Composite variable strategy", 
    value = T,
    icon = icon("check"), 
    bigger = TRUE,
    status = "danger"
),
prettyCheckbox(
   inputId = "tbd_natural",
   label = "3: Hypothetical strategy (I, natural ICEs)", 
    value = T,
    icon = icon("check"), 
    bigger = TRUE,
    status = "danger"
),
prettyCheckbox(
   inputId = "tbd_removed",
   label = "4: Hypothetical strategy (II, removing ICEs)", 
    value = T,
    icon = icon("check"), 
    bigger = TRUE,
    status = "danger"
),
prettyCheckbox(
   inputId = "tbd_whileon",
   label = "5: While on treatment strategy", 
    value = T,
    icon = icon("check"), 
    bigger = TRUE,
    status = "danger"
),
prettyCheckbox(
   inputId = "tbd_principal",
   label = "6: Principal stratum strategy", 
    value = T,
    icon = icon("check"), 
    bigger = TRUE,
    status = "danger"
),

hr()


), #sidebarPanel

##########----------##########----------##########
mainPanel(

##-----------------------------------------------------------------------------------
##-----------------------------------------------------------------------------------

h4("Data preview"),
tabsetPanel(

  tabPanel("Data preview", br(), DT::DTOutput("Xdata")),

  tabPanel("Descriptive statistics (numerical)", br(),
  h5( "1. Numerical variables"), DTOutput("sum") ),
  tabPanel("Descriptive statistics (categorical)", br(),
  h5( "2. Categorical variables"), DTOutput("fsum") )
  ),

##-----------------------------------------------------------------------------------
##-----------------------------------------------------------------------------------

hr(),
h4("Advanced settings for the plot"),
sliderInput("bs_320", label = h5("Number of resampling in bootstrapping"), min = 0, max = 1000, value = 0, width="100%",),
helpText(HTML("0 indicates the usage of explicit formula")),
sliderInput("conf", label = h5("Significant level"), min = 0.8, max = 0.99, step =0.01, value = 0.95, width="100%",),

splitLayout(cellWidths = c("50%", "50%"),
prettyRadioButtons(
          inputId = "meth",
          status = "info",
          label="Estimation method",
          fill = TRUE,
          icon = icon("check"),
          choiceNames = list(
            HTML("Nonparametric estimation"),
            HTML("Semiparametrically efficient estimation")
          ),
          choiceValues = list("np", "eff")),
prettyRadioButtons(
          inputId = "d_320",
          status = "info",
          label="Survival plot type",
          fill = TRUE,
          icon = icon("check"),
          choiceNames = list(
            HTML("Cumulative incidence function"),
            HTML("Survival function")
          ),
          choiceValues = list(FALSE,TRUE))
),

# selectInput("d_320", label= h5("Survival plot type"), selected = F, choices = list("Cumulative incidence function"=F, "Survival function"=T)),
sliderInput("yrange", "Set a range for the Y axis in the treatment effect plot:", min = -1, max = 1, step=0.05, width="100%", value = c(-1,1)),
sliderInput("yrangecif", "Set a range for the Y axis in the survival plot:", min = 0, max = 1, step=0.05, width="100%", value = c(0,1)),
actionButton("B_32_surv", HTML('Show results'), 
             class =  "btn-danger",
             icon  = icon("chart-column")),

hr(),

h4("Results"),

tabsetPanel(
    tabPanel("1: Treatment policy strategy",
      conditionalPanel("input.tbd_treatment",
      splitLayout(
      plotOutput("tps_32a", width = 500, height = 400),plotOutput("tps_32b", width = 500, height = 400),
      ),
      uiOutput("bstime_320"),
      DTOutput("tpsbs_32_tab")
      )),
    tabPanel("2: Composite variable strategy",
      conditionalPanel("input.tbd_composite",
      splitLayout(
      plotOutput("cvs_32a", width = 500, height = 400),plotOutput("cvs_32b", width = 500, height = 400)
      ),
      uiOutput("bstime_321"),
      DTOutput("cvsbs_32_tab")
      )),
    tabPanel("3: Hypothetical strategy (I)",
      conditionalPanel("input.tbd_natural",
      splitLayout(
      plotOutput("hsn_32a", width = 500, height = 400),plotOutput("hsn_32b", width = 500, height = 400)
      ),
      uiOutput("bstime_322"),
      DTOutput("hsnbs_32_tab")
      )),
    tabPanel("4: Hypothetical strategy (II)",
      conditionalPanel("input.tbd_removed",
      splitLayout(
      plotOutput("hsr_32a", width = 500, height = 400),plotOutput("hsr_32b", width = 500, height = 400)
      ),
      uiOutput("bstime_323"),
      DTOutput("hsrbs_32_tab")
      )),
    tabPanel("5: While on treatment strategy",
      conditionalPanel("input.tbd_whileon",
      splitLayout(
      plotOutput("wos_32a", width = 500, height = 400),plotOutput("wos_32b", width = 500, height = 400)
      ),
      uiOutput("bstime_324"),
    DTOutput("wosbs_32_tab")
      )),
    tabPanel("6: Principal stratum strategy",
      conditionalPanel("input.tbd_principal",
      splitLayout(
      plotOutput("pss_32a", width = 500, height = 400),plotOutput("pss_32b", width = 500, height = 400)
      ),
      uiOutput("bstime_325"),
    DTOutput("pssbs_32_tab")
      ))
    
)
##########----------##########----------##########
)
)
