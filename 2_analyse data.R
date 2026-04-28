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

packages <- c("data.table", "ggplot2", "ggthemes", "openxlsx",
              "sf","rnaturalearth"
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

path <- file.path("output", "data_prepared_260417.rds")
input1 <- data.table(readRDS(file = path))

nrow(input1) # 1612
input1 <- input1[!country == "Greece"]

input1[country == "United Kingdom", MLDA := "18"]



# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

## 2) PREPARE DATA
# ______________________________________________________________________________________________________________________

## 2.1) ...
#-------------------------------------------------------


# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

## 3) MAP
# ______________________________________________________________________________________________________________________

europe <- ne_countries(scale = "medium", returnclass = "sf") #continent = "Europe", 

add <- input1[year == 2025,.(country,premise,MLDA)]
add[,unique(MLDA), by = country] # DK, FI, SW
add[, minage := MLDA]
add[country == "Denmark", minage := "16/18"]
add[country == "Finland", minage := "18/20"]
add[country == "Sweden", minage := "18/20"]
add[country == "United Kingdom", minage := "18"]

add <- unique(add[,.(country,minage = factor(minage))])

europe_data <- merge(europe, add, by.x = "name", by.y = "country", all.x = TRUE)
#europe_data <- europe_data[!is.na(europe_data$minage),]
#europe_data$minage[is.na(europe_data$minage)] <- "no data"

ggplot(data = europe_data) +
  geom_sf(aes(fill = minage), color = "gray60", size = 0.12, alpha = 0.9) +
  scale_fill_manual(
    values = c(
      "16" = "#d73027","16/18" = "#fc8d59","18" = "#fee08b",
      "18/20" = "#d9ef8b","18/21" = "#91bfdb","20" = "#1a9850"),
    na.value = "lightgray" ) +
  coord_sf(xlim = c(-25, 32), ylim = c(35, 72), expand = FALSE) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right") +
  labs(fill = "MLDA")

ggsave(paste0("output/main_figure map europe_",DATE,".png"), width = 12, height = 6)
ggsave(paste0("output/main_figure map europe_",DATE,".svg"), width = 12, height = 6)


# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

## 4) Population coverage
# ______________________________________________________________________________________________________________________

covdat <- copy(input1)
table(covdat$MLDA)
covdat[is.na(MLDA), MLDA := 0]
covdat[,minage := ifelse(any(MLDA == 0 | MLDA == "0/18"),0,
                         ifelse(any(MLDA == "15"),15,
                                ifelse(any(MLDA == "16" | MLDA == "16/18"),16,
                                       ifelse(any(MLDA == "17"),17,
                                              ifelse(any(MLDA == "18"),18,
                                                     ifelse(any(MLDA == "18/20" | MLDA == "20"),20,
                                                            ifelse(any(MLDA == "18/21"),21,NA
                                                                   ))))))),
                 by = .(country,year)]
covdat[is.na(minage)] # none
covdat <- unique(covdat[,.(country,year,minage,pop_all,pop_16,pop_17,pop_18,pop_19,pop_20,pop_21)])

##  check
covdat[country == "Belgium"] # 
covdat[country == "Croatia"] # 
covdat[country == "Cyprus"] # 
covdat[country == "Denmark"] # 
covdat[country == "France"] # 
covdat[country == "Italy"] # 
covdat[country == "Lithuania"] # 
covdat[country == "Luxembourg"] #
covdat[country == "Malta"] # 
covdat[country == "Netherlands"] # 
covdat[country == "Portugal"] # 
covdat[country == "Slovenia"] # 
covdat[country == "Spain"] # 
covdat[country == "United Kingdom"] # 

## 4.1) all ages
#-------------------------------------------------------

pdat <- copy(covdat); pdat$pop_all <- NULL
pdat <- melt(pdat, id.vars = c("country","year","minage"), variable.name = "age", value.name = "pop")
pdat[, age := as.numeric(gsub("pop_","",age))]

pdat[, pop_withaccess := ifelse(minage <=age, pop, 0)]

pdat_agg <- pdat[,.(pop = sum(pop),
                    pop_withaccess = sum(pop_withaccess)),
                 by = .(year,age)]

pdat_agg[age < 20 & year %in% c(2000,2010,2015,2020,2025), sum(pop), by = .(year)]
pdat_agg[age < 20 & year == 2000, sum(pop)]
pdat_agg[age < 20 & year == 2025, sum(pop)]

pdat_agg[age == 18]
pdat[year == 2000 & age == 18, sum(pop)]
pdat[year == 2000 & age == 18, sum(pop_withaccess)]

pdat_agg[,.(pop,pop_withaccess,prop = pop_withaccess/pop), by = .(year,age)][year == 2025]
pdat_agg[,.(pop,pop_withaccess,prop = pop_withaccess/pop), by = .(year,age)][year == 2000 & age <20, .(
  pop = sum(pop),
  pop_withaccess = sum(pop_withaccess),
  prop = sum(pop_withaccess)/sum(pop))]
pdat_agg[,.(pop,pop_withaccess,prop = pop_withaccess/pop), by = .(year,age)][year == 2025 & age <20, .(
  pop = sum(pop),
  pop_withaccess = sum(pop_withaccess),
  prop = sum(pop_withaccess)/sum(pop))]

## 4.2) only age16
#-------------------------------------------------------

p16 <- pdat_agg[age == 16]

ggplot(data = p16, aes(x = year)) + 
  ggtitle("Population of 16-year-olds (blue) with legal access to alcoholic beverages (amber)",
          "Vertical lines indicate year in which countries have increased MLPA affecting 16-year-olds") +
  geom_area(aes(y = pop), fill = "#b0c4de") + ##D8F999
  geom_area(aes(y = pop_withaccess), fill = "#f4a523") + ##FFCCD3
  
  geom_vline(xintercept = 2003, linetype = 3) + geom_label(x = 2003, y = 4.5, label = "2003\nCroatia and Slovenia") +
  geom_vline(xintercept = 2009, linetype = 3) + geom_label(x = 2009, y = 4.5, label = "2009\nMalta") +
  geom_vline(xintercept = 2010, linetype = 3) + geom_label(x = 2010, y = 3.5, label = "2010\nFrance") +
  geom_vline(xintercept = 2013, linetype = 3) + geom_label(x = 2013, y = 4.5, label = "2013\nItaly") +
  geom_vline(xintercept = 2014, linetype = 3) + geom_label(x = 2014, y = 3.5, label = "2014\nNetherlands") +
  geom_vline(xintercept = 2016, linetype = 3) + geom_label(x = 2016, y = 4.5, label = "2016\nPortugal and Spain") + 
  
  scale_x_continuous("", breaks = seq(2000,2024,2)) + 
  scale_y_continuous("Population of 16-year-olds in 30 European countries (in millions)", expand = F) +
  theme(panel.grid.minor = element_line(color = "gray80",linetype = "dotted"),
        axis.text.x = element_text(color = "black"), # family="Aptos",
        axis.text.y = element_text(color = "black"), # family="Aptos",
        axis.title.x = element_text(color = "black"),
        axis.title.y = element_text(color = "black"),
        plot.title = element_text(color = "black"),
        plot.subtitle = element_text(color = "black"))

ggsave(paste0("output/main_figure coverage 16yo_",DATE,".png"), width = 12, height = 6)
ggsave(paste0("output/main_figure coverage 16yo_",DATE,".svg"), width = 12, height = 6)

p16[,.(year,pop,pop_withaccess,prop = pop_withaccess/pop)]


## 4.3) only age17
#-------------------------------------------------------

p17 <- pdat_agg[age == 17]

ggplot(data = p17, aes(x = year)) + 
  ggtitle("Population of 17-year-olds (blue) with legal access to alcoholic beverages (amber)",
          "Vertical lines indicate year in which countries have increased MLPA affecting 17-year-olds") +
  geom_area(aes(y = pop), fill = "#b0c4de") + ##D8F999
  geom_area(aes(y = pop_withaccess), fill = "#f4a523") + ##FFCCD3
  
  geom_vline(xintercept = 2003, linetype = 3) + geom_label(x = 2003, y = 4.5, label = "2003\nCroatia and Slovenia") +
  geom_vline(xintercept = 2010, linetype = 3) + geom_label(x = 2010, y = 3.5, label = "2010\nFrance") +
  geom_vline(xintercept = 2013, linetype = 3) + geom_label(x = 2013, y = 4.5, label = "2013\nItaly") +
  geom_vline(xintercept = 2014, linetype = 3) + geom_label(x = 2014, y = 3.5, label = "2014\nNetherlands") +
  geom_vline(xintercept = 2016, linetype = 3) + geom_label(x = 2016, y = 4.5, label = "2016\nPortugal and Spain") + 
  scale_x_continuous("", breaks = seq(2000,2024,2)) + 
  scale_y_continuous("Population of 17-year-olds in 30 European countries (in millions)", expand = F) +
  theme(panel.grid.minor = element_line(color = "gray80",linetype = "dotted"),
        axis.text.x = element_text(color = "black"), # family="Aptos",
        axis.text.y = element_text(color = "black"), # family="Aptos",
        axis.title.x = element_text(color = "black"),
        axis.title.y = element_text(color = "black"),
        plot.title = element_text(color = "black"),
        plot.subtitle = element_text(color = "black"))

ggsave(paste0("output/supp_figure coverage 17yo_",DATE,".png"), width = 12, height = 6)
ggsave(paste0("output/supp_figure coverage 17yo_",DATE,".svg"), width = 12, height = 6)

p17[,.(year,pop,pop_withaccess,prop = pop_withaccess/pop)]


## 4.3) only age18
#-------------------------------------------------------

p18 <- pdat_agg[age == 18]

ggplot(data = p18, aes(x = year)) + 
  ggtitle("Population of 18-year-olds (blue) with legal access to alcoholic beverages (amber)",
          "Vertical lines indicate year in which countries have increased MLPA affecting 18-year-olds") +
  geom_area(aes(y = pop), fill = "#b0c4de") + ##D8F999
  geom_area(aes(y = pop_withaccess), fill = "#f4a523") + ##FFCCD3
  
  geom_vline(xintercept = 2018, linetype = 3) + geom_label(x = 2018, y = 4.5, label = "2018\nLithuania") +
  scale_x_continuous("", breaks = seq(2000,2024,2)) + 
  scale_y_continuous("Population of 18-year-olds in 30 European countries (in millions)", expand = F) +
  theme(panel.grid.minor = element_line(color = "gray80",linetype = "dotted"),
        axis.text.x = element_text(color = "black"), # family="Aptos",
        axis.text.y = element_text(color = "black"), # family="Aptos",
        axis.title.x = element_text(color = "black"),
        axis.title.y = element_text(color = "black"),
        plot.title = element_text(color = "black"),
        plot.subtitle = element_text(color = "black"))

ggsave(paste0("output/supp_figure coverage 18yo_",DATE,".png"), width = 12, height = 6)
ggsave(paste0("output/supp_figure coverage 18yo_",DATE,".svg"), width = 12, height = 6)

p18[,.(year,pop,pop_withaccess,prop = pop_withaccess/pop)]


## 4.3) only age19
#-------------------------------------------------------

p19 <- pdat_agg[age == 19]

ggplot(data = p19, aes(x = year)) + 
  ggtitle("Population of 19-year-olds (blue) with legal access to alcoholic beverages (amber)",
          "Vertical lines indicate year in which countries have increased MLPA affecting 19-year-olds") +
  geom_area(aes(y = pop), fill = "#b0c4de") + ##D8F999
  geom_area(aes(y = pop_withaccess), fill = "#f4a523") + ##FFCCD3
  
  geom_vline(xintercept = 2019, linetype = 3) + geom_label(x = 2019, y = 4.5, label = "2019\nLithuania") +
  scale_x_continuous("", breaks = seq(2000,2024,2)) + 
  scale_y_continuous("Population of 19-year-olds in 30 European countries (in millions)", expand = F) +
  theme(panel.grid.minor = element_line(color = "gray80",linetype = "dotted"),
        axis.text.x = element_text(color = "black"), # family="Aptos",
        axis.text.y = element_text(color = "black"), # family="Aptos",
        axis.title.x = element_text(color = "black"),
        axis.title.y = element_text(color = "black"),
        plot.title = element_text(color = "black"),
        plot.subtitle = element_text(color = "black"))

ggsave(paste0("output/supp_figure coverage 19yo_",DATE,".png"), width = 12, height = 6)
ggsave(paste0("output/supp_figure coverage 19yo_",DATE,".svg"), width = 12, height = 6)

p19[,.(year,pop,pop_withaccess,prop = pop_withaccess/pop)]



