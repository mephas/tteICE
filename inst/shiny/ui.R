##
## Causal Efficacy
##

if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
library("shiny", quietly = TRUE)
if (!requireNamespace("shinythemes", quietly = TRUE)) install.packages("shinythemes")
library("shinythemes", quietly = TRUE)
if (!requireNamespace("psych")) install.packages("psych")
library("psych")
if (!requireNamespace("shinyWidgets")) install.packages("shinyWidgets")
library("shinyWidgets")
if (!requireNamespace("DT", quietly = TRUE)) install.packages("DT")
library("DT", quietly = TRUE)
source("tab/func2_causal.R", encoding = "utf-8")


tagList(

  includeCSS("www/style_cn_causal.css"),

##########------------------------------------------------------------------------------------------------------------------------------------------

navbarPage(
title="",
theme = shinytheme("united"), #united #simplex
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
  )

)


)
