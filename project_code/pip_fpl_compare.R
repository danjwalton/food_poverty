#Install required packages
required.packages <- c("data.table","jsonlite", "rstudioapi", "httr")
install.packages(required.packages[!(required.packages %in% installed.packages())])
lapply(required.packages, require, character.only=T)

#Set working directory as local folder
setwd(dirname(getActiveDocumentContext()$path))

##
##### Start of API querying ######\

#Set up API base address
pip <- "https://api.worldbank.org/pip/v1/pip?"

#Read in food poverty lines
fpls <- fread("fpl_compare_2023.csv")

#Melt to 'long' format
fpls <- melt(fpls, id.vars = "countrycode", variable.factor = F)

#Extract the reporting year (for API use), and effective year
fpls[, `:=` (reporting_year = substr(variable, 5, 8), type = variable, effective_year = substr(variable, nchar(variable) - 3, nchar(variable)))]

#Either read or create the output file
if(length(list.files(pattern = "^food_poverty_compare_2023[.]csv$")) == 1){
  fpl_out <- fread("food_poverty_compare_2023.csv")
} else {
  fpl_out <- data.table(cc = character(), type = character(), effective_year = integer())
}

#List the FPLs which are outstanding to do based on the read output file
fpls_todo <- fpls[!(paste0(countrycode, type, effective_year) %in% fpl_out[, paste0(cc, type, effective_year)])]

#Iterate through the outstanding FPLs
for(i in 1:nrow(fpls_todo)){
  if(exists("fpl_response")) rm(fpl_response)
  
  #Read parameters for the API call from the list of FPLs
  fpl_r <- fpls_todo[i]
  cc <- fpl_r$countrycode
  rep_lvl <- "all"
  reporting_year <- fpl_r$reporting_year
  pov_line <- round(fpl_r$value, 3)
  
  #Message country code and year to console to indicate progress
  message(cc, fpl_r$effective_year)
  
  #Select subnational data when required
  if(nchar(cc) > 3){
    
    rep_lvl <- ifelse(substr(cc, 5, 5) == "R", "rural", "urban")
    cc <- substr(cc, 0, 3)
  }
  
  #Create the API call address based on parameters
  pip_call <- paste0(pip, "country=", cc, "&year=", reporting_year, "&povline=", pov_line, "&reporting_level=", rep_lvl, "&fill_gaps=true")
  
  #Check whether the call is valid
  if(status_code(GET(pip_call)) != 404){
  
    #If valid, read the data from it
    fpl_response <- rbindlist(content(GET(pip_call)))
  } else {
    
    fpl_response <- data.table()
  }
  
  #Fill in the effective year and original country code to the outputted data
  fpl_response$effective_year <- fpl_r$effective_year
  fpl_response$type <- fpl_r$type
  fpl_response$cc <- fpl_r$countrycode

  #Append the outputted data to the output table and write it to the local folder
  fpl_out <- rbind(fpl_out, fpl_response, fill = T)
  fwrite(fpl_out, "food_poverty_compare_2023.csv")
}

##### End of API querying #####
##

##
##### Start of analysis ######

#Total calculations
food_pov <- fpl_out

food_pov <- food_pov[country_name != ""]
food_pov[, effective_year := as.integer(effective_year)]
food_pov <- food_pov[order(cc, effective_year)]

#Fill SSD with SDN pre-2008
food_pov[cc == "SSD" & is.na(headcount), headcount := food_pov[cc == "SDN" & food_pov[cc == "SSD", is.na(headcount)], headcount]]

#Fill other blanks with nearest value
food_pov[, headcount := nafill(headcount, "nocb"), by = .(cc)]
food_pov[, headcount := nafill(headcount, "locf"), by = .(cc)]

#Read WUP population function from remote repo
source("https://raw.githubusercontent.com/devinit/gha_automation/main/general/wupData.R")

#Calculate number of poor
wupPop <- wup_get()
wupPop[, cc := paste0(ISO3, ifelse(area == "total", "", ifelse(area == "rural", "-R", "-U")))]
food_pov <- merge(food_pov, wupPop[, .(cc, effective_year = as.integer(year), population)], all.x = T)
food_pov[, poor := headcount * population]

#Fill gaps with regional estimates
pip_call <- paste0(pip, "country=all&year=2019&fill_gaps=true")
pip_response <- data.table(fromJSON(pip_call))

missing_pip_countries <- pip_response[!(country_code %in% food_pov$country_code), .(country_name, country_code, region_name, region_code, estimation_type = "regional")]
regional_headcounts <- food_pov[, .(headcount = sum(poor)/sum(population)), by = .(effective_year, type, region_name)]

missing_pip_countries <- merge(missing_pip_countries, regional_headcounts, by = c("region_name"), all.x = T, allow.cartesian = T)
missing_pip_countries <- merge(missing_pip_countries, wupPop[, .(cc, country_code = cc, effective_year = as.integer(year), population)], all.x = T, by = c("country_code", "effective_year"))

missing_pip_countries[, poor := headcount * population]

food_pov <- rbind(food_pov, missing_pip_countries, fill = T)[order(effective_year, country_code)]

wb_call <- "https://api.worldbank.org/v2/country?per_page=500&format=json"
wb_response <- data.table(fromJSON(wb_call)[[2]])

missing_wb_countries <- wb_response[region.id != "NA" & !(id %in% food_pov$country_code), .(country_name = name, country_code = id, region_name = trimws(region.value), region_code = region.id, income = incomeLevel.value, estimation_type = "regional")]
missing_wb_countries[income == "High income", region_name := "Other High Income Countries"][, income := NULL]

missing_wb_countries <- merge(missing_wb_countries, regional_headcounts, by = c("region_name"), all.x = T, allow.cartesian = T)
missing_wb_countries <- merge(missing_wb_countries, wupPop[, .(cc, country_code = cc, effective_year = as.integer(year), population)], all.x = T, by = c("country_code", "effective_year"))

missing_wb_countries[, poor := headcount * population]

food_pov <- rbind(food_pov, missing_wb_countries, fill = T)[order(effective_year, country_code)]

#Total by region
food_poor_total <- food_pov[!is.na(poor), .(poor = sum(poor), population = sum(population)), by = .(effective_year, type)][, effective_headcount := poor/population][]
fwrite(food_poor_total, "food_poor_compare_total.csv")

#Total by country
food_poor_cc <- food_pov[!is.na(poor),  .(poor = sum(poor), population = sum(population)), by = .(cc, effective_year, type)][, effective_headcount := poor/population][]
food_poor_cc <- dcast(food_poor_cc, cc + type ~ effective_year, value.var = "poor")
fwrite(food_poor_cc, "food_poor_compare_cc.csv")

##### End of analysis #####
##