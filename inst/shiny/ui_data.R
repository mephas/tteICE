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
    label =  NULL,
    choices =  list("Use example data: bmt" = "bmt", "Upload my own data"="NULL"),
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

# h3("Step 2. Causal analysis"),

h3("Step 2. Choose variables and other settings"),
uiOutput("a_32"),
helpText(HTML("Treatment assignment; 0 for control, 1 for treatment.")),

uiOutput("time_32"),
helpText(HTML("Time to the primary (terminal) event.")),

uiOutput("cstatus_32"),
helpText(HTML("Primary (terminal) event indicator; 0 for censoring, 1 for the primary (terminal) event, 2 for intercurrent event (or choose additional collection of intercurrent events).")),

h5("Whether to collect time to intercurrent events (semicompeting risks)"),
helpText(HTML("If the time to primary event and the time to intercurrent events are stored in separate variables, click this button to choose the time of intercurrent events.")),
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
helpText(HTML("Time to the intercurrent event.")),

uiOutput("cstatus_321"),
helpText(HTML("Intercurrent event indicator，1 for the intercurrent event, 0 for censoring."))
),


uiOutput("cov1_32"),
helpText(HTML("Baseline covariates that need to be controlled.")),

h5("Whether to assign weight or set a weight variable"),
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
helpText(HTML("Weights are typically used to account for unequal probabilities of selection, adjust for missing data, or emphasize certain observations in the analysis.")),

conditionalPanel(
  condition = "input.wgt=='var'",
uiOutput("weight_32"),
helpText(HTML("Weight for each subject."))),

# uiOutput("sub_32"),
# helpText(HTML("Subset variable to define the subset of data")),

hr(),

h3("Step 3. Choose strategy"),

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

h3("Data preview and descriptive statistics"),
materialSwitch(
   inputId = "prev",
   label = h4("Hide data"), 
    value = TRUE,
   status = "warning",
   width = "100%"
),
helpText(HTML("If you want to show/hide this part, click the button.")),
conditionalPanel("input.prev",
tabsetPanel(

  tabPanel("Data preview", br(), DT::DTOutput("Xdata")),
  tabPanel("Descriptive statistics (numerical)", br(),
  h5( "1. Numerical variables"), DTOutput("sum") ),
  tabPanel("Descriptive statistics (categorical)", br(),
  h5( "2. Categorical variables"), DTOutput("fsum") )
  )),

##-----------------------------------------------------------------------------------
##-----------------------------------------------------------------------------------

hr(),
h3("Result 1. Plots of treatment effects and survival/incident probabilities"),

materialSwitch(
   inputId = "sets",
   label = h4("Advanced settings for the plot"), 
    value = FALSE,
   status = "warning",
   width = "100%"
),
helpText(HTML("If you would like to customize the plots (e.g., change the axis labels, adjust the colors, or modify the legends), click the button.")),
conditionalPanel("input.sets",
sliderInput("bs_320", label = h5("Number of resampling in bootstrapping"), min = 0, max = 1000, value = 0, width="100%",),
helpText(HTML("0 indicates the usage of explicit formula.")),
sliderInput("conf", label = h5("Significant level of confidence interval"), min = 0.8, max = 0.99, step =0.01, value = 0.95, width="100%",),

splitLayout(
sliderInput("yrange", "Treatment effect plot: range for the Y-axis", min = -1, max = 1, step=0.05, width="100%", value = c(-1,1)),
sliderInput("yrangecif", "Survival plot: range for the Y-axis", min = 0, max = 1, step=0.05, width="100%", value = c(0,1))
),
splitLayout(
  textInput("t1", "Legend: the name of treatment (the 1st group)", value = "Treat", width = "100%", placeholder = NULL),
  textInput("t0", "Legend: the name of control (the 2nd group)", value = "Control", width = "100%", placeholder = NULL)
  ),
splitLayout(
  textInput("col1", "Color: the color of treatment (the 1st group)", value = "brown", width = "100%", placeholder = NULL),
  textInput("col0", "Color: the color of control (the 2nd group)", value = "darkcyan", width = "100%", placeholder = NULL)
  ),
helpText(HTML('Refer to the color <a href="https://colorbrewer2.org/#type=qualitative&scheme=Set1&n=3" target="_blank">ColorBrewer Set1</a>.'))
),


h5("Whether to show the P-value on the the survival plot"),
prettyToggle(
          inputId = "adp",
          label_on = "Yes",
          icon_on = icon("check"),
          status_on = "info",
          status_off = "warning",
          label_off = "No",
          icon_off = icon("remove"),
          value = TRUE),
splitLayout(
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

actionButton("B_32_surv", HTML('Show/Update plots'), 
             class =  "btn-danger",
             icon  = icon("chart-column")),
helpText(HTML("Once settings are changes, click the button to update the plots.")),

hr(),

tabsetPanel(
    tabPanel("1: Treatment policy strategy",
      conditionalPanel("input.tbd_treatment",
      splitLayout(
      plotOutput("tps_32a", width = 500, height = 400),plotOutput("tps_32b", width = 500, height = 400)
      ),
      helpText(HTML("You can copy the plot by right-clicking on it and selecting `Copy image` from the menu."))
      # uiOutput("bstime_320"),
      # DTOutput("tpsbs_32_tab"),
      # hr(),
      # h3("Prediction"),
      # numericInput("num1", "Enter a time point:", value = 0, min = 0, step = 1),
      # DTOutput("tpsbs_32_tab_pred")
      )),
    tabPanel("2: Composite variable strategy",
      conditionalPanel("input.tbd_composite",
      splitLayout(
      plotOutput("cvs_32a", width = 500, height = 400),plotOutput("cvs_32b", width = 500, height = 400)
      ),
      helpText(HTML("You can copy the plot by right-clicking on it and selecting `Copy image` from the menu."))
      # uiOutput("bstime_321"),
      # DTOutput("cvsbs_32_tab"),
      # hr(),
      # h3("Prediction"),
      # numericInput("num2", "Enter a time point:", value = 0, min = 0, step = 1),
      # DTOutput("cvsbs_32_tab_pred")
      )),
    tabPanel("3: Hypothetical strategy (I)",
      conditionalPanel("input.tbd_natural",
      splitLayout(
      plotOutput("hsn_32a", width = 500, height = 400),plotOutput("hsn_32b", width = 500, height = 400)
      ),
      helpText(HTML("You can copy the plot by right-clicking on it and selecting `Copy image` from the menu."))
      # uiOutput("bstime_322"),
      # DTOutput("hsnbs_32_tab"),
      # hr(),
      # h3("Prediction"),
      # numericInput("num3", "Enter a time point:", value = 0, min = 0, step = 1),
      # DTOutput("hsnbs_32_tab_pred")
      )),
    tabPanel("4: Hypothetical strategy (II)",
      conditionalPanel("input.tbd_removed",
      splitLayout(
      plotOutput("hsr_32a", width = 500, height = 400),plotOutput("hsr_32b", width = 500, height = 400)
      ),
      helpText(HTML("You can copy the plot by right-clicking on it and selecting `Copy image` from the menu."))
      # uiOutput("bstime_323"),
      # DTOutput("hsrbs_32_tab"),
      # hr(),
      # h3("Prediction"),
      # numericInput("num4", "Enter a time point:", value = 0, min = 0, step = 1),
      # DTOutput("hsrbs_32_tab_pred")
      )),
    tabPanel("5: While on treatment strategy",
      conditionalPanel("input.tbd_whileon",
      splitLayout(
      plotOutput("wos_32a", width = 500, height = 400),plotOutput("wos_32b", width = 500, height = 400)
      ),
      helpText(HTML("You can copy the plot by right-clicking on it and selecting `Copy image` from the menu."))
      # uiOutput("bstime_324"),
      # DTOutput("wosbs_32_tab"),
      # hr(),
      # h3("Prediction"),
      # numericInput("num5", "Enter a time point:", value = 0, min = 0, step = 1),
      # DTOutput("wosbs_32_tab_pred")
      )),
    tabPanel("6: Principal stratum strategy",
      conditionalPanel("input.tbd_principal",
      splitLayout(
      plotOutput("pss_32a", width = 500, height = 400),plotOutput("pss_32b", width = 500, height = 400)
      ),
      helpText(HTML("You can copy the plot by right-clicking on it and selecting `Copy image` from the menu."))
      # uiOutput("bstime_325"),
      # DTOutput("pssbs_32_tab"),
      # hr(),
      
      ))
    
    
),
conditionalPanel("input.B_32_surv",
      h3("Result 2. Prediction"),      
      splitLayout(
        numericInput("num6", ("Enter a time point to predict the treatment effects:"), value = 0, min = 0, step = 1, width = "100%"),
        numericInput("digit_32", ("Enter a value for the digits:"), value = 3, min = 0, step = 1, width = "100%"),
        # dropdownButton(label = "Settings of digits",
        # numericInput(inputId = 'digit_32',label = 'decimal', value = 3, min = 0, max = 10, step=1),
        # circle = FALSE, 
        # icon = icon("gear"), width = "300px"
        # )
        ),
      helpText(HTML("The time point may be within the observed data or beyond it (fitting or predicting, respectively). If input is changed, click the button above to update the results.")),
      
      actionButton("B_33_surv", HTML('Show/Update results'), 
                   class =  "btn-danger",
                   icon  = icon("chart-column")),
      helpText(HTML("Once settings are changes, click the button to update the plots first, then update the prediction results.")),
      DTOutput("fin.res"),
      h4("Interpretations"),
      textOutput("myText")
      )
##########----------##########----------##########
)
)
