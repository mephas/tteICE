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
  helpText(HTML("Variables of the above time to the primary (terminal) event and its event indicator may also need to be changed.")),
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
p(),
helpText(HTML("1. Treatment policy: What is the effect of assigning the treatment, allowing ICEs to occur naturally?")),
prettyCheckbox(
   inputId = "tbd_treatment",
   label = "1: Treatment policy strategy", 
    value = TRUE,
    icon = icon("check"), 
    bigger = TRUE,
    status = "danger"
),
helpText(HTML("2. Composite variable: What is the effect on the time to the first clinically meaningful event, either the primary outcome event or ICEs?")),
prettyCheckbox(
   inputId = "tbd_composite",
   label = "2: Composite variable strategy", 
    value = TRUE,
    icon = icon("check"), 
    bigger = TRUE,
    status = "danger"
),
helpText(HTML("3. Hypothetical (I, natural ICEs): What would the effect on the primary outcome event be if the hazard of ICEs were set to its natural level in the control group?")),
prettyCheckbox(
   inputId = "tbd_natural",
   label = "3: Hypothetical strategy (I, natural ICEs)", 
    value = TRUE,
    icon = icon("check"), 
    bigger = TRUE,
    status = "danger"
),
helpText(HTML("4. Hypothetical (II, removing ICEs): What would the effect on the primary outcome event be if ICEs were removed?")),
prettyCheckbox(
   inputId = "tbd_removed",
   label = "4: Hypothetical strategy (II, removing ICEs)", 
    value = TRUE,
    icon = icon("check"), 
    bigger = TRUE,
    status = "danger"
),
helpText(HTML("5. While on treatment: What is the effect on the primary outcome event while remaining free from ICEs?")),
prettyCheckbox(
   inputId = "tbd_whileon",
   label = "5: While on treatment strategy", 
    value = TRUE,
    icon = icon("check"), 
    bigger = TRUE,
    status = "danger"
),
helpText(HTML("6. Principal stratum: What is the effect specifically in the subgroup of patients who would never experience ICEs, regardless of their treatment assignment status?")),
prettyCheckbox(
   inputId = "tbd_principal",
   label = "6: Principal stratum strategy", 
    value = TRUE,
    icon = icon("check"), 
    bigger = TRUE,
    status = "danger"
),
# helpText(HTML("Principal stratum: What is the effect specifically in the subgroup of patients who would never experience ICEs, regardless of their treatment assignment status?")),
hr(),

numericInput("digit_32", ("Enter a value for the digits of all the results:"), value = 3, min = 0, step = 1, width = "100%")

), #sidebarPanel

##########----------##########----------##########
mainPanel(

##-----------------------------------------------------------------------------------
##-----------------------------------------------------------------------------------

h3("Data preview and descriptive statistics"),
materialSwitch(
   inputId = "prev",
   label = h4("Display data"), 
    value = FALSE,
   status = "warning",
   width = "100%"
),
helpText(HTML("If you want to dsiplay data and summaries, click the button.")),
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
  textInput("t1", "Legend: input the name of treatment (the 1st group)", value = "Treat", width = "100%", placeholder = NULL),
  textInput("t0", "Legend: input the name of control (the 2nd group)", value = "Control", width = "100%", placeholder = NULL)
  ),
splitLayout(
  textInput("col1", "Color: input the color of treatment (the 1st group)", value = "brown", width = "100%", placeholder = NULL),
  textInput("col0", "Color: input the color of control (the 2nd group)", value = "darkcyan", width = "100%", placeholder = NULL)
  ),
helpText(HTML('Refer to the color <a href="https://colorbrewer2.org/#type=qualitative&scheme=Set1&n=3" target="_blank">ColorBrewer Set1</a>.')),
textInput("xlab", "X-lable: input the label of x-axis (the unit of time)", value = "Time", width = "50%", placeholder = NULL)
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
          label=h5("Estimation method"),
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
          label= h5("Survival plot type"),
          fill = TRUE,
          icon = icon("check"),
          choiceNames = list(
            HTML("Cumulative incidence function"),
            HTML("Survival function")
          ),
          choiceValues = list(FALSE,TRUE))
),
p(),
actionButton("B_32_surv", HTML('Show/Update plots/results'), 
             class =  "btn-danger",
             icon  = icon("chart-column")),
helpText(HTML("Once settings are changes, click the button to update the plots. You can copy the plot by right-clicking on it and selecting `Copy image` from the menu.")),
# helpText(HTML("You can copy the plot by right-clicking on it and selecting `Copy image` from the menu.")),

hr(),

tabsetPanel(
    tabPanel("1: Treatment policy strategy",
      conditionalPanel("input.tbd_treatment",
      # helpText(HTML("You can copy the plot by right-clicking on it and selecting `Copy image` from the menu.")),
      splitLayout(
      plotOutput("tps_32a", width = 500, height = 400),plotOutput("tps_32b", width = 500, height = 400)
      ),
      conditionalPanel(
        condition = "input.meth == 'eff'",
          prettyCheckbox(
            inputId = "effc1",
            label = "Display the coefficients and standard errors in the Cox model", 
            value = FALSE,
            icon = icon("check"), 
            bigger = TRUE,
            status = "danger"
            ),
          conditionalPanel(
            condition = "input.effc1",
            DTOutput("eff1_tab")
            ),
          prettyCheckbox(
            inputId = "effp1",
            label = "Display the p-value of testing proportional hazards assumption", 
            value = FALSE,
            icon = icon("check"), 
            bigger = TRUE,
            status = "danger"
            ),
          conditionalPanel(
            condition = "input.effp1",
            DTOutput("effp1_tab"),
            helpText(HTML("A p-value smaller than 0.05 indicates failure of the proportional hazards assumption. Use bootstrapping to calculate confidence intervals instead."))
            ),
          helpText(HTML('If results are not shown, click the "Show/Update plots/results" button to update the plots.'))
      )
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
      # helpText(HTML("You can copy the plot by right-clicking on it and selecting `Copy image` from the menu.")),
      conditionalPanel(
        condition = "input.meth == 'eff'",
          prettyCheckbox(
            inputId = "effc2",
            label = "Display the coefficients and standard errors in the Cox model", 
            value = FALSE,
            icon = icon("check"), 
            bigger = TRUE,
            status = "danger"
            ),
          conditionalPanel(
            condition = "input.effc2",
            DTOutput("eff2_tab")
            ),
          prettyCheckbox(
            inputId = "effp2",
            label = "Display the p-value of testing proportional hazards assumption", 
            value = FALSE,
            icon = icon("check"), 
            bigger = TRUE,
            status = "danger"
            ),
          conditionalPanel(
            condition = "input.effp2",
            DTOutput("effp2_tab"),
            helpText(HTML("A p-value smaller than 0.05 indicates failure of the proportional hazards assumption. Use bootstrapping to calculate confidence intervals instead."))
            ),
          helpText(HTML('If results are not shown, click the "Show/Update plots/results" button to update the plots.'))
      )
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
      # helpText(HTML("You can copy the plot by right-clicking on it and selecting `Copy image` from the menu.")),
      conditionalPanel(
        condition = "input.meth == 'eff'",
          prettyCheckbox(
            inputId = "effc3",
            label = "Display the coefficients and standard errors in the Cox model", 
            value = FALSE,
            icon = icon("check"), 
            bigger = TRUE,
            status = "danger"
            ),
          conditionalPanel(
            condition = "input.effc3",
            DTOutput("eff3_tab")
            ),
          prettyCheckbox(
            inputId = "effp3",
            label = "Display the p-value of testing proportional hazards assumption", 
            value = FALSE,
            icon = icon("check"), 
            bigger = TRUE,
            status = "danger"
            ),
          conditionalPanel(
            condition = "input.effp3",
            DTOutput("effp3_tab"),
            helpText(HTML("A p-value smaller than 0.05 indicates failure of the proportional hazards assumption. Use bootstrapping to calculate confidence intervals instead."))
            ),
          helpText(HTML('If results are not shown, click the "Show/Update plots/results" button to update the plots.'))
      )
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
      # helpText(HTML("You can copy the plot by right-clicking on it and selecting `Copy image` from the menu.")),
      conditionalPanel(
        condition = "input.meth == 'eff'",
          prettyCheckbox(
            inputId = "effc4",
            label = "Display the coefficients and standard errors in the Cox model", 
            value = FALSE,
            icon = icon("check"), 
            bigger = TRUE,
            status = "danger"
            ),
          conditionalPanel(
            condition = "input.effc4",
            DTOutput("eff4_tab")
            ),
          prettyCheckbox(
            inputId = "effp4",
            label = "Display the p-value of testing proportional hazards assumption", 
            value = FALSE,
            icon = icon("check"), 
            bigger = TRUE,
            status = "danger"
            ),
          conditionalPanel(
            condition = "input.effp4",
            DTOutput("effp4_tab"),
            helpText(HTML("A p-value smaller than 0.05 indicates failure of the proportional hazards assumption. Use bootstrapping to calculate confidence intervals instead."))
            ),
          helpText(HTML('If results are not shown, click the "Show/Update plots/results" button to update the plots.'))
      )
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
      # helpText(HTML("You can copy the plot by right-clicking on it and selecting `Copy image` from the menu.")),
      conditionalPanel(
        condition = "input.meth == 'eff'",
          prettyCheckbox(
            inputId = "effc5",
            label = "Display the coefficients and standard errors in the Cox model", 
            value = FALSE,
            icon = icon("check"), 
            bigger = TRUE,
            status = "danger"
            ),
          conditionalPanel(
            condition = "input.effc5",
            DTOutput("eff5_tab")
            ),
          prettyCheckbox(
            inputId = "effp5",
            label = "Display the p-value of testing proportional hazards assumption", 
            value = FALSE,
            icon = icon("check"), 
            bigger = TRUE,
            status = "danger"
            ),
          conditionalPanel(
            condition = "input.effp5",
            DTOutput("effp5_tab"),
            helpText(HTML("A p-value smaller than 0.05 indicates failure of the proportional hazards assumption. Use bootstrapping to calculate confidence intervals instead."))
            ),
          helpText(HTML('If results are not shown, click the "Show/Update plots/results" button to update the plots.'))
      )
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
      # helpText(HTML("You can copy the plot by right-clicking on it and selecting `Copy image` from the menu.")),
      conditionalPanel(
        condition = "input.meth == 'eff'",
          prettyCheckbox(
            inputId = "effc6",
            label = "Display the coefficients and standard errors in the Cox model", 
            value = FALSE,
            icon = icon("check"), 
            bigger = TRUE,
            status = "danger"
            ),
          conditionalPanel(
            condition = "input.effc6",
            DTOutput("eff6_tab")
            ),
          prettyCheckbox(
            inputId = "effp6",
            label = "Display the p-value of testing proportional hazards assumption", 
            value = FALSE,
            icon = icon("check"), 
            bigger = TRUE,
            status = "danger"
            ),
          conditionalPanel(
            condition = "input.effp6",
            DTOutput("effp6_tab"),
            helpText(HTML("A p-value smaller than 0.05 indicates failure of the proportional hazards assumption. Use bootstrapping to calculate confidence intervals instead."))
            ),
          helpText(HTML('If results are not shown, click the "Show/Update plots/results" button to update the plots.'))
      )
      # uiOutput("bstime_325"),
      # DTOutput("pssbs_32_tab"),
      # hr(),
      
      ))
),
hr(),
conditionalPanel("input.B_32_surv",
      h3("Result 2. Prediction"),      
      # splitLayout(
        numericInput("num6", ("Enter a time point to predict the treatment effects:"), value = 0, min = 0, step = 1, width = "50%"),
        # numericInput("digit_32", ("Enter a value for the digits:"), value = 3, min = 0, step = 1, width = "100%"),
        # dropdownButton(label = "Settings of digits",
        # numericInput(inputId = 'digit_32',label = 'decimal', value = 3, min = 0, max = 10, step=1),
        # circle = FALSE, 
        # icon = icon("gear"), width = "300px"
        # )
        # ),
      helpText(HTML("The time point may be within the observed data or beyond it (fitting or predicting, respectively). If input is changed, click the button above to update the results.")),
      p(),
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
