#!/usr/bin/env Rscript
suppressPackageStartupMessages({ library(httr); library(jsonlite); library(dplyr) })

q <- function(url, label="") {
  cat("\n[",label,"]\n",url,"\n",sep="")
  r <- tryCatch(GET(url,timeout(60)),error=function(e)NULL)
  if(is.null(r)){cat("error\n");return()}
  cat("status",status_code(r))
  if(status_code(r)==200){
    b<-tryCatch(fromJSON(rawToChar(content(r,"raw")),simplifyVector=FALSE),error=function(e)NULL)
    if(!is.null(b)) cat("  vals:",length(b$value),"  sizes:",paste(unlist(b$size),collapse=","),"\n")
    else cat("  (non-JSON)\n")
  } else cat("\n")
}

# Financial assets - try without finpos filter
q("https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/nasq_10_f_bs?format=JSON&lang=en&geo=IT&sector=S14_S15&na_item=F2&unit=MIO_NAC","Deposits no finpos")
q("https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/nasq_10_f_bs?format=JSON&lang=en&geo=IT&sector=S14_S15&na_item=F5&unit=MIO_NAC","Equity no finpos")
q("https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/nasq_10_f_bs?format=JSON&lang=en&geo=IT&sector=S14_S15&na_item=F6&unit=MIO_NAC","Insurance no finpos")
q("https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/nasq_10_f_bs?format=JSON&lang=en&geo=IT&sector=S14_S15&na_item=F3&unit=MIO_NAC","Debt sec no finpos")
q("https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/nasq_10_f_bs?format=JSON&lang=en&geo=IT&sector=S14_S15&na_item=F52&unit=MIO_NAC","MF shares no finpos")
# Total financial assets and liabilities
q("https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/nasq_10_f_bs?format=JSON&lang=en&geo=IT&sector=S14_S15&na_item=F&unit=MIO_NAC","Total fin no finpos")

# HPI without purchase_type
q("https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/prc_hpi_q?format=JSON&lang=en&geo=IT","HPI no filters")

# s_adj filter for income - check what values exist
q("https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/nasq_10_nf_tr?format=JSON&lang=en&geo=IT&na_item=B6G&sector=S14_S15&unit=CP_MEUR&s_adj=NSA","Income NSA")
q("https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/nasq_10_nf_tr?format=JSON&lang=en&geo=IT&na_item=B6G&sector=S14_S15&unit=CP_MEUR&s_adj=SCA","Income SCA")
q("https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/nasq_10_nf_tr?format=JSON&lang=en&geo=IT&na_item=B6G&sector=S14_S15&unit=CP_MEUR&direct=PAID","Income direct=PAID")
