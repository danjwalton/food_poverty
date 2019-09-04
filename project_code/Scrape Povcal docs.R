required.packages <- c("reshape2","ggplot2","WDI","data.table","XML")
lapply(required.packages, require, character.only=T)

setwd("G:/My Drive/Work/GitHub/food_poverty/")

#Function to retrieve full list of Povcal surveys
povcal_svy = function(pl=1.9,group.by="WB"){
  url = "http://iresearch.worldbank.org/PovcalNet/PovcalNetAPI.ashx?"
  params = list(
    "Countries"="all",
    "PovertyLine"=as.character(pl),
    "SurveyYears"="all",
    "Display"="C",
    "GroupedBy"=group.by,
    "format"="csv"
  )
  param_names = names(params)
  for(param_name in param_names){
    param = params[[param_name]]
    url = paste0(url,param_name,"=",param,"&")
  }
  url = substr(url,1,nchar(url)-1)
  return(read.csv(url))
}


ext <- povcal_svy()
ext_country_codes <- as.data.frame(unique(ext$CountryCode))
names(ext_country_codes) <- "CountryCode"
ext_country_codes$CountryCode <- as.character(ext_country_codes$CountryCode)
#Add outdated country codes which Povcal still uses in its documents urls
ext_country_codes <- rbind(ext_country_codes,"ZAR","TMP","WBG","KSV")

#Retrieve all Povcal country docs
povcal_doc <- function(country){
  open_url <- "http://iresearch.worldbank.org/PovcalNet/Docs/CountryDocs/"
  doc_url <- paste0(open_url,country,".htm")
  doc_html <- htmlParse(doc_url, isURL=T)
  doc_root <- xmlRoot(doc_html)
  #Extract Population and CPI table
  doc_list <- xmlToList(getNodeSet(doc_root,"//table")[[2]])
  doc_list <- head(tail(doc_list,-1),-1)
  doc_df <- rbindlist(doc_list)
  if(length(doc_df)==3){
    names(doc_df) <- c("Year","Pop","CPI")
    doc_df$Year <- as.numeric(doc_df$Year)
    doc_df$CPI <- as.numeric(doc_df$CPI)
    } else {
      if(length(doc_df)==6){
        names(doc_df) <- c("Year","Pop","R.Pop","U.Pop","R.CPI","U.CPI")
        doc_df$Year <- as.numeric(doc_df$Year)
        doc_df$R.CPI <- as.numeric(doc_df$R.CPI)
        doc_df$U.CPI <- as.numeric(doc_df$U.CPI)
        } else {
        names(doc_df) <- c("Year","Pop","R.Pop","U.Pop","CPI")
        doc_df$Year <- as.numeric(doc_df$Year)
        doc_df$CPI <- as.numeric(doc_df$CPI)
      }
  
    }
  return(doc_df)
}


data.list <- list()

i <- 1

for(i in 1:nrow(ext_country_codes)){
  country <- ext_country_codes[i,"CountryCode"]
  doc_temp <- tryCatch({povcal_doc(country)},error=function(e){return(NULL)})
  doc_temp$ISO <- country
  data.list[[i]]<- doc_temp
}

doc <- rbindlist(data.list, fill=T)

write.csv(doc,"project_data/doc_cpi_list.csv")

write.csv(ext,"project_data/povcal_out.csv")
