##
## Causal Efficacy
##

if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
library("shiny", quietly = TRUE)
if (!requireNamespace("shinythemes", quietly = TRUE)) install.packages("shinythemes")
library("shinythemes", quietly = TRUE)
if (!requireNamespace("survival", quietly = TRUE)) install.packages("survival")
library("survival", quietly = TRUE)
if (!requireNamespace("DT", quietly = TRUE)) install.packages("DT")
library("DT", quietly = TRUE)
if (!requireNamespace("shinyWidgets")) install.packages("shinyWidgets")
library("shinyWidgets")
if (!requireNamespace("MASS")) install.packages("MASS")
library("MASS", quietly = TRUE)
if (!requireNamespace("shinycssloaders")) install.packages("shinycssloaders")
library("shinycssloaders")
if (!requireNamespace("shinyjs")) install.packages("shinyjs")
library("shinyjs", quietly = TRUE)



source("tab/panel_cn.R", encoding = "utf-8")
source("tab/tab_cn.R", encoding = "utf-8")
source("tab/func2_causal.R", encoding = "utf-8")

# jsCode <- 'shinyjs.getUserName = function(params) {
#   let userName = getCookie("userid");
#   Shiny.onInputChange("jsname", userName);
# }'

# jsFileId <- 'shinyjs.getFileId = function(params) {
#   let id = decodeURIComponent(getCookie("fileId"));
#   Shiny.onInputChange("jsFileId", id);
#  }'

tagList(
withMathJax(),
# tags$div(tags$script("
#               MathJax.Hub.Config({
#               tex2jax: {inlineMath: [ ['$','$'] ]}
#               TeX:{
#               equationNumbers:{
#               autoNumber:'AMS'  
#       }}
#   });
              
#               ")),

  # checkLogin(),
  includeCSS("www/style_cn_causal.css"),
  # stylink(),
  # useShinyjs(),
  # extendShinyjs(text = jsCode, functions = "getUserName"),
  # extendShinyjs(text = jsFileId, functions = "getFileId"),
##########------------------------------------------------------------------------------------------------------------------------------------------

navbarPage(
title="",
theme = shinythemes::shinytheme("united"), #united #simplex
collapsible = TRUE,
id = "navibar",
position = "static-top",
header=NULL,
footer=NULL,

tabPanel(
  "ICHe9r1",
  headerPanel("ICHe9r1"),
  source("ui_data.R", local = TRUE, encoding = "UTF-8")$value,
  hr()
  ),

tabPanel(
 "Wiki",
  includeMarkdown("help0.md"),
  hr()
)

        
)

  
)
