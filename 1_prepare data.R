# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# PROJECT TITLE:  MLDA paper
# CODE AUTHOR:    JAKOB
# DATE STARTED:   260415

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 0) ESSENTIALS
# ______________________________________________________________________________________________________________________

# clean workspace
rm(list=ls())

packages <- c("data.table", "ggplot2", "ggthemes", "openxlsx"
              #"Hmisc", "tidyr", "nnet",
              #"tableone", "knitr", "kableExtra",
              #"survey"
              )

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
# Load packages
invisible(lapply(packages, library, character.only = TRUE))

# current date:
DATE <- format(Sys.Date(), "%y%m%d")

# themes and options
theme_set( theme_gdocs() + #base_family = "Aptos"
             theme( panel.background = element_rect(fill = "white") )
)
options(scipen = 999)

# fonts:
extrafont::loadfonts(device = "win")

# viridis colors: https://waldyrious.net/viridis-palette-generator/
vcol3 <- viridis::viridis(3)
vcol4 <- viridis::viridis(4)
vcol5 <- viridis::viridis(5)
vcol6 <- viridis::viridis(6)

# plasma colors: https://waldyrious.net/viridis-palette-generator/
pcol <- viridis::plasma(1)

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 1) LOAD DATA
# ______________________________________________________________________________________________________________________

##  MLDA data
# -------------------------------------------------------

path <- file.path("input", "2024-05-17_Alcohol Policy Scale_Age limits_All countries.xlsx")
path <- file.path("input", "2026-04-16_Alcohol Policy Scale_Age limits_All countries.xlsx")
input1 <- data.table(read.xlsx(xlsxFile = path))

nrow(input1) # 73


##  Population data
# -------------------------------------------------------

##  up to 2023
##  accessed on 15 April 2026 from UN: https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/EXCEL_FILES/2_Population/WPP2024_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES.xlsx
##  standard projection / population / single ages / both sexes:
##  "Total population (both sexes combined) by single age. De facto population as of 1 July of the year indicated classified by single age (0, 1, 2, ..., 99, 100+). Data are presented in thousands."

path <- file.path("input", "WPP2024_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES.xlsx")
input2 <- data.table(read.xlsx(xlsxFile = path, startRow = 17, sheet = "Estimates"))

nrow(input2) # 21983

##  projection after 2024:
##  accessed on 15 April 2026 from UN: https://population.un.org/wpp/assets/Excel%20Files/2_Indicators%20(Probabilistic)/EXCEL_FILES/2_Population/UN_PPP2024_Output_PopTot.xlsx
##  probabilistic projection / population / probabilistic population both sexes

path <- file.path("input", "UN_PPP2024_Output_PopulationBySingleAge_BothSexes.xlsx")
input3 <- data.table(read.xlsx(xlsxFile = path, startRow = 17, sheet = "Median"))

nrow(input3) # 22950



# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

## 2) PREPARE DATA
# ______________________________________________________________________________________________________________________

## 2.1) MLDA
#-------------------------------------------------------

mldadat <- copy(input1)
mldadat <- mldadat[1:62]
mldadat$X1 <- mldadat$X35 <- mldadat$X36 <- NULL

mldadat[, country := zoo::na.locf(COUNTRY)]
mldadat[,.(country,COUNTRY)]
mldadat$COUNTRY <- NULL

mldadat[, premise := ifelse(Legal.age.limit %like% "On-premise", "on", "off")]
mldadat$Legal.age.limit <- NULL

mldadat <- melt(mldadat, id.vars = c("country","premise"), variable.name = "year", value.name = "MLDA")
mldadat$year <- as.numeric(as.character(mldadat$year))

mldadat <- mldadat[year>=2000]

## 2.2) Population
#-------------------------------------------------------

##  until 2023
popdat1  <- copy(input2)

keep    <- names(popdat1)[names(popdat1) %like% "^[0-9]|Year|Region"]
popdat1  <- popdat1[,.SD, .SDcols = keep] ; rm(keep)
popdat1[, country := `Region,.subregion,.country.or.area.*`]
popdat1[, year := as.numeric(Year)]
popdat1$Year <- popdat1$`Region,.subregion,.country.or.area.*`  <- NULL

popdat1 <- melt(popdat1, id.vars = c("country","year"), variable.name = "age", value.name = "pop")

##  from 2024
popdat2  <- copy(input3)

keep    <- names(popdat2)[names(popdat2) %like% "^[0-9]|Year|Region"]
popdat2  <- popdat2[,.SD, .SDcols = keep] ; rm(keep)
popdat2[, country := `Region,.subregion,.country.or.area.*`]
popdat2[, year := as.numeric(Year)]
popdat2$Year <- popdat2$`Region,.subregion,.country.or.area.*`  <- NULL

popdat2 <- melt(popdat2, id.vars = c("country","year"), variable.name = "age", value.name = "pop")

##  all years
popdat <- rbind(popdat1,popdat2)[order(country,year,age)]
popdat$pop <- as.numeric(popdat$pop)
popdat$pop <- popdat$pop/1000

popdat <- popdat[age %in% c(16,17,18,19,20,21) & year %in% c(2000:2025)]
popdat[, pop_all := sum(pop), by = .(country,year)]

ggplot(data = popdat[country %like% "Austria|Germany|Finland|Italy"], aes(x = year, y = pop_all, color = country)) + 
  geom_line() + 
  geom_vline(xintercept = 2024)


## 2.3) Combine
#-------------------------------------------------------

mnames <- unique(mldadat$country)
pnames <- unique(popdat$country)

mnames[!mnames %in% pnames] # Only Czech Republic is missing
pnames[pnames %like% "Czech"] 

mldadat[country == "Czech Republic", country := "Czechia"]
mnames <- unique(mldadat$country)

popdat <- dcast(popdat[country %in% mnames], country + year + pop_all ~ age, value.var = "pop")
names(popdat) <- c("country","year","pop_all",paste0("pop_",c(16:21)))

data <- merge(mldadat,popdat,
              by = c("country","year")
              )

data[, unique(country)]

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

## 3) OUTPUT
# ______________________________________________________________________________________________________________________

saveRDS(data, paste0("output/data_prepared_",DATE,".rds"))





