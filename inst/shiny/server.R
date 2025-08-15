
# load functions
# source("sub/1missing/missingdata.R")


function(input, output, session) {

  source("server_data.R", local = TRUE, encoding = "utf-8")$value
  source("server_ana.R", local = TRUE, encoding = "utf-8")$value
  source("server_s1.R", local = TRUE, encoding = "utf-8")$value
  source("server_s2.R", local = TRUE, encoding = "utf-8")$value
  source("server_s3.R", local = TRUE, encoding = "utf-8")$value
  source("server_s4.R", local = TRUE, encoding = "utf-8")$value
  source("server_s5.R", local = TRUE, encoding = "utf-8")$value
  source("server_s6.R", local = TRUE, encoding = "utf-8")$value
}

# )
