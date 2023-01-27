#Install required packages
required.packages <- c("data.table","jsonlite", "rstudioapi", "httr")
install.packages(required.packages[!(required.packages %in% installed.packages())])
lapply(required.packages, require, character.only=T)

#Set working directory as local folder
setwd(dirname(getActiveDocumentContext()$path))

##
##### Start of API querying ######

#Set up API base address
pip <- "https://api.worldbank.org/pip/v1/pip?"

#Read in food poverty lines
fpls <- fread("fpl_2023.csv")

#Remove countries with no FPL for 2017
fpls <- fpls[!is.na(fpl_2017)]

#Melt to 'long' format
fpls <- melt(fpls, id.vars = "countrycode", variable.factor = F)

#Extract the reporting year (for API use), and effective year
fpls[, `:=` (reporting_year = substr(variable, 5, 8), effective_year = substr(variable, nchar(variable) - 3, nchar(variable)))]

#Either read or create the output file
if(length(list.files(pattern = "^food_poverty_2023[.]csv$")) == 1){
  fpl_out <- fread("food_poverty_2023.csv")
} else {
  fpl_out <- data.table(cc = character(), effective_year = integer())
}

#List the FPLs which are outstanding to do based on the read output file
fpls_todo <- fpls[!(paste0(countrycode, effective_year) %in% fpl_out[, paste0(cc, effective_year)])]

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
    fpl_response <- data.table(fromJSON(pip_call))
  } else {
    
    fpl_response <- data.table()
  }
  
  #Fill in the effective year and original country code to the outputted data
  fpl_response$effective_year <- fpl_r$effective_year
  fpl_response$cc <- fpl_r$countrycode

  #Append the outputted data to the output table and write it to the local folder
  fpl_out <- rbind(fpl_out, fpl_response, fill = T)
  fwrite(fpl_out, "food_poverty_2023.csv")
}

##### End of API querying #####
##

##
##### Start of analysis ######

#Total calculations
food_pov <- fpl_out

food_pov <- food_pov[order(cc, effective_year)]

#Fill SSD with SDN pre-2008
food_pov[cc == "SSD" & is.na(headcount), headcount := food_pov[, headcount[cc == "SDN" & headcount[cc == "SSD" & is.na(headcount)]]]]

#Fill other blanks with nearest value
food_pov[, headcount := nafill(headcount, "nocb"), by = .(cc)]
food_pov[, headcount := nafill(headcount, "locf"), by = .(cc)]

#Read WUP population function from remote repo
source("https://raw.githubusercontent.com/devinit/gha_automation/main/general/wupData.R")

#Calculate number of poor
wupPop <- wup_get()
wupPop[, cc := paste0(ISO3, ifelse(area == "total", "", ifelse(area == "rural", "-R", "-U")))]
food_pov <- merge(food_pov, wupPop[, .(cc, effective_year = (year), population)], all.x = T)
food_pov[, poor := headcount * population]

#Total by year
food_poor_total <- food_pov[!is.na(poor), .(poor = sum(poor), population = sum(population)), by = effective_year][, effective_headcount := poor/population][]
fwrite(food_poor_total, "food_poor_total.csv")

#Total by country
food_poor_cc <- food_pov[!is.na(poor),  .(poor = sum(poor), population = sum(population)), by = .(cc, effective_year)][, effective_headcount := poor/population][]
food_poor_cc <- dcast(food_poor_cc, cc ~ effective_year, value.var = "poor")
fwrite(food_poor_cc, "food_poor_cc.csv")

##### End of analysis #####
##