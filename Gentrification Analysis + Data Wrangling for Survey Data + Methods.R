setwd("xxxxxx")

# loading in libraries
packages <- c("devtools", "haven", "tidyverse", "sp", "rgdal", "rgeos", 
              "tmap", "spgwr", "grid", "gridExtra", "sf", "dplyr",  
              "mgcv", "parallel", "VIM", "patchwork","haven","survey",
              "naniar", "car", "spdep","sjPlot","modelsummary", "fixest",
              "missForest", "caret", "doParallel","RColorBrewer")


lapply(packages, library, character.only = TRUE)
rm(packages)

# loading in indices of deprivation data

m10 <- read.csv("imd2010englsoa2011.csv")

m15 <- read.csv("ID 2015 for London.csv")

m19 <- read.csv("ID 2019 for London.csv")

# Standardising data for 2011 LSOA codes

LSOA11CD <- c(m15$LSOA.code)
LSOA11NM <- c(m15$LSOA.name)


# subsetting non-London dataset to London LSOAs

m10 <- m10[m10$LSOA11CD %in% LSOA11CD,]

# Standardising column names for IMD score

m10$IMD2010 <- m10$IMD.2010.adjusted
m15$IMD2015 <- m15$IMD.Score
m19$IMD2019 <- m19$Index.of.Multiple.Deprivation..IMD..Score

# Combining MDI data

m15$LSOA11CD <- m15$LSOA.code.2011

m19$LSOA11CD <- m19$LSOA.code.2011

m <- merge(x = m10, y = m15, by = "LSOA11CD")

m <- merge(x = m, y = m19, by = "LSOA11CD")

m <- m[ , c('LSOA11CD','LSOA11NM','IMD2010','IMD2015','IMD2019')]

# removing temporary datasets
rm(m10,m15,m19)

# Now using polynomial interpolation to add values for each year.

for (i in 1:4835) {
  scores <- c(m$IMD2010[i], m$IMD2015[i], m$IMD2019[i])
  spline_results <- spline(c(2010, 2015, 2019), scores, xout = 2011:2018)$y
  m[i, paste("IMD", 2011:2018, sep = "")] <- spline_results
}


# Shifting columns to the right order
new_order <- c(1:3, 6:9, 4, 10:12, 5)

# Reorder the dataframe columns
m <- m[, new_order]


# removing temporary data

rm(i,new_order,scores,spline_results)

# writing as a csv for easy loading

write.csv(m, "m.csv", row.names = FALSE)

m <- read.csv("m.csv")


# Loading the data for population churn based for each year in the analysis
years <- 2009:2018

# Base path to the files
base_path <- "churn_1997_2022/hh_churn_lsoa11_"

# Initialize an empty data frame to store the merged results
wide_data <- NULL

for (year in years) {
  file_path <- paste0(base_path, year, ".csv")
  df <- read.csv(file_path)
  colnames(df) <- ifelse(colnames(df) == "area", "area", paste0(colnames(df), "_", year))
  if (is.null(wide_data)) {
    wide_data <- df
  } else {
  
    wide_data <- merge(wide_data, df, by = "area", all = TRUE)
  }
}

# subsetting for London LSOA codes

ch <- wide_data[wide_data$area %in% LSOA11CD,]

# subsetting relevant columns to examined time periods

ch <- ch[ , c(1,15:24, 42:51, 69:78, 96:105, 123:132, 150:159, 177:186, 204:213, 231:240, 258:267 )]

rm(wide_data, years, base_path, year, file_path, df)

# writing as a csv

write.csv(ch, "ch.csv", row.names = FALSE)

ch <- read.csv("ch.csv")

rm(base_path,file_path,i,new_order,scores,spline_results,year,years,df,data_frames)

# Organising housing data

hp <- read.csv("HPSSA Dataset 46 - Median price paid for residential properties by LSOA.csv")
hpborough <- read.csv("land-registry-house-prices-borough.csv")

# subsetting house price data for London LSOAs

hp <- hp[hp$LSOA.code %in% LSOA11CD,]

LACD <- c(hp$Local.authority.code)
LANM <- c(hp$Local.authority.name)

class(hp$Year.ending.Dec.1995) # this tells us that the numbers in this dataset are actually non-numeric characters

# This is likely due to the use of commas

hp <- data.frame(lapply(hp, function(x) {
  if(is.character(x) | is.factor(x)) {
    gsub(",", "", x)
  } else {
    x
  }
}))

# converting all : values to NA 

hp <- hp %>%
  mutate_all(~na_if(.x, ":"))

hp[, 5:ncol(hp)] <- lapply(hp[, 5:ncol(hp)], as.numeric)


# turning all values in hp to numeric values

hp[, 5:ncol(hp)] <- lapply(hp[, 5:ncol(hp)], function(x) as.numeric(as.character(x)))

# 4.22% of the house price data is made up of NA value

# subsetting to correspond to Understanding Society wave timelines AND years ending dec 2009-2019

hp <- hp[, c(1:4, 57, 61, 65, 69, 73, 75, 77, 81, 85, 87, 89, 93, 97, 99, 101 )]


# Using missforest optimised for imputation

hp_sub <- hp[, -c(1:4)]

sum(is.na(hp_sub))/(4835*15)

registerDoParallel(cores = 14)

hp_forest <- missForest(hp_sub, parallelize = 'forests')

######

hp_forest1 <- hp_forest$ximp

hp[, -c(1:4)] <- hp_forest1

write.csv(hp, file = "hp_forest.csv", row.names = FALSE)

hp <- read.csv("hp_forest.csv")

# Adding mean of all median house sale prices in each LSOA across the borough per year

mean(hp$Year.ending.Dec.2009[hp$Local.authority.code == "E09000001"], na.rm=TRUE)
mean(hp$Year.ending.Dec.2009[hp$Local.authority.code == "E09000002"])


years_of_interest <-  c(colnames(hp))

Local.authority.code <- sprintf("E090000%02d", 1:33)

# For each year, calculate the mean and then add it as a new column


for (year in years_of_interest) {
  hp[[paste0("Mean_", year)]] <- NA
  for (code in Local.authority.code) {
    if(code %in% hp$Local.authority.code){
      mean_value <- mean(hp[hp$Local.authority.code == code, year, drop = TRUE], na.rm = TRUE)
      hp[hp$Local.authority.code == code, paste0("Mean_", year)] <- mean_value
    }
  }
}

# removing unneeded imputation columns

hp <- hp[ , c(1:19,24:38)]

write.csv(hp, "hp.csv", row.names = FALSE)

hp <- read.csv("hp.csv")

rm(hp_imputed, code, Local.authority.code, mean_value, year, years_of_interest)

# adding ethnic composition estimate data

# Loop through years 2009 to 2019
for(year in 2009:2019) {
  file_name <- paste0("CDRC_MEP_LSOA_v4_1997_to_2023/MEP_lsoa11_", year, ".csv")
  variable_name <- paste0("e", year)
  assign(variable_name, read.csv(file_name))
}

# subsetting for greater london lsoas and adding a nonwhite variable


# Iterate over each year from 2015 to 2019
for(year in 2009:2019) {
  var_name <- paste0("e", year)
  assign(var_name, get(var_name)[get(var_name)$lsoa11 %in% LSOA11CD, ])
  eval(parse(text = paste0(var_name, "$nw <- ", var_name, 
                           "$abd + ", var_name, "$acn + ", var_name, "$ain + ", 
                           var_name, "$apk + ", var_name, "$aao + ", var_name, 
                           "$baf + ", var_name, "$bca + ", var_name, "$oxx")))
}


# subsetting data to only contain nw variable

for (year in 2009:2019) {
  dataset_name <- paste0("e", year)
  original_dataset <- get(dataset_name)
  subsetted_dataset <- original_dataset[c("lsoa11", "nw")]
  new_variable_name <- paste0("e", year)
  assign(new_variable_name, subsetted_dataset, envir = .GlobalEnv)
}

# adding all to same dataset

e <- merge(x = e2009, y = e2010, by = "lsoa11", suffixes = c("2009", "2010"))

for(year in 2011:2019) {
  current_dataset <- get(paste0("e", year))
  names(current_dataset)[names(current_dataset) == "nw"] <- paste0("nw", year)
  e <- merge(x = e, y = current_dataset, by = "lsoa11")
}

# removing temporary dataset
rm(e2009,e2010,e2011,e2012,e2013,e2014,e2015,e2016,e2017,e2018,e2019, current_dataset, original_dataset, subsetted_dataset,
   dataset_name, file_name, new_variable_name, var_name, variable_name,year)

write.csv(e, "e.csv", row.names = FALSE)

e <- read.csv("e.csv")

# preparing main datasets, hp, ch, e and m

names(ch)[names(ch) == "area"] <- "lsoa11"

names(hp)[names(hp) == "LSOA.code"] <- "lsoa11"

names(m)[names(m) == "LSOA11CD"] <- "lsoa11"

# now merging all the important data to make sure all data are in the right columns
rm(g)
g = merge(e, ch, by = "lsoa11")
g = merge(g, hp, by = "lsoa11")
g = merge(g, m, by = "lsoa11")

# creating gentrification score to cross reference with the same methodoloy
# used in the runnymede report

# testing relationship between other variables and % change in nw

g$change_nw <- ((g$nw2016 - g$nw2010) / g$nw2010)
g$change_churn <- (g$chn2016_2010)
g$change_dep <- ((g$IMD2015 - g$IMD2010) / g$IMD2010)
g$change_house <- (((g$Year.ending.Dec.2016 / g$Mean_Year.ending.Dec.2016) - 
                      (g$Year.ending.Dec.2009 / g$Mean_Year.ending.Dec.2009)) / 
                     (g$Year.ending.Dec.2009 / g$Mean_Year.ending.Dec.2009))


# truncating values greater than 1 to align with Almeida's (2021)
# initial methodology

sum(g$change_house >= 1)

g$change_house <- ifelse(g$change_house >=1, 1, g$change_house)

sum(g$change_nw > 1)

g$change_nw <- ifelse(g$change_nw >=1, 1, g$change_nw)

g$gent2010_2016 <- 0.5*g$change_churn - 0.25*g$change_nw +
  0.125*g$change_house - 0.125*g$change_dep + 0.25

g$gent2010_2016 <- ifelse(
  !is.na(g$change_house),
  0.5 * g$change_churn - 0.25 * g$change_nw + 0.125 * g$change_house - 0.125 * g$change_dep + 0.25, 
  0.5 * g$change_churn - 0.25 * g$change_nw - 0.125 * g$change_dep + 0.25 
)

sum(is.na(g$gent2010_2016))
# highly significant

m1 <- lm(change_nw ~ change_churn + change_dep + change_house, data =  g)
summary(m1)

vif(m1)

cor.test(g$change_churn, g$change_nw, method = "pearson")

# so yes, increase in gentrification index without race leads to a decrease
# in nonwhite population. There is also a statistically significant relationship
# between churn (negative, so as churn increases, proportion of nonwhites decrease),
# and deprivation, so as deprivation increase, proportion of nonwhites increase.

# creating gentrification scores for waves 3, 6 and 9 in Understanding Society

# firstly recording within-wave gentrification for each wave whilst truncating values

# wave 3

g$change_nw <- ((g$nw2013 - g$nw2009) / g$nw2009)
g$change_churn <- (g$chn2013_2009)
g$change_dep <- ((g$IMD2013 - g$IMD2010) / g$IMD2010)
g$change_house <- (((g$Year.ending.Jun.2013 / g$Mean_Year.ending.Jun.2013) - 
                      (g$Year.ending.Dec.2008 / g$Mean_Year.ending.Dec.2008)) / 
                     (g$Year.ending.Dec.2008 / g$Mean_Year.ending.Dec.2008))


# checking for and truncating house price ethnic change values greater than one

sum(g$change_house >= 1)

g$change_house <- ifelse(g$change_house >=1, 1, g$change_house)

sum(g$change_nw > 1)

g$change_nw <- ifelse(g$change_nw >=1, 1, g$change_nw)

g$gent_wave3_m <- 0.5*g$change_churn - 0.25*g$change_nw +
  0.125*g$change_house - 0.125*g$change_dep + 0.25

# wave 6

g$change_nw <- ((g$nw2016 - g$nw2012) / g$nw2012)
g$change_churn <- (g$chn2016_2012)
g$change_dep <- ((g$IMD2016 - g$IMD2012) / g$IMD2012)
g$change_house <- (((g$Year.ending.Jun.2016 / g$Mean_Year.ending.Jun.2016) - 
                      (g$Year.ending.Dec.2011 / g$Mean_Year.ending.Dec.2011)) / 
                     (g$Year.ending.Dec.2011 / g$Mean_Year.ending.Dec.2011))


sum(g$change_house >= 1)

g$change_house <- ifelse(g$change_house >=1, 1, g$change_house)

sum(g$change_nw > 1)

g$change_nw <- ifelse(g$change_nw >=1, 1, g$change_nw)

g$gent_wave6_m <- 0.5*g$change_churn - 0.25*g$change_nw +
  0.125*g$change_house - 0.125*g$change_dep + 0.25

# wave 9

g$change_nw <- ((g$nw2019 - g$nw2015) / g$nw2015)
g$change_churn <- (g$chn2019_2015)
g$change_dep <- ((g$IMD2019 - g$IMD2015) / g$IMD2015)
g$change_house <- (((g$Year.ending.Jun.2019 / g$Mean_Year.ending.Jun.2019) - 
                      (g$Year.ending.Dec.2014 / g$Mean_Year.ending.Dec.2014)) / 
                     (g$Year.ending.Dec.2014 / g$Mean_Year.ending.Dec.2014)) 

sum(g$change_house >= 1)
g$change_house <- ifelse(g$change_house >=1, 1, g$change_house)
sum(g$change_nw > 1)
g$change_nw <- ifelse(g$change_nw >=1, 1, g$change_nw)

g$gent_wave9_m <- 0.5*g$change_churn - 0.25*g$change_nw +
  0.125*g$change_house - 0.125*g$change_dep + 0.25


# secondly treating gentrification as a composite score

# wave 3

g$change_nw <- ((g$nw2013 - g$nw2009) / g$nw2009)
g$change_churn <- (g$chn2013_2009)
g$change_dep <- ((g$IMD2013 - g$IMD2010) / g$IMD2010)
g$change_house <- (((g$Year.ending.Jun.2013 / g$Mean_Year.ending.Jun.2013) - 
                      (g$Year.ending.Dec.2008 / g$Mean_Year.ending.Dec.2008)) / 
                     (g$Year.ending.Dec.2008 / g$Mean_Year.ending.Dec.2008))


sum(g$change_house >= 1)

g$change_house <- ifelse(g$change_house >=1, 1, g$change_house)

sum(g$change_nw > 1)

g$change_nw <- ifelse(g$change_nw >=1, 1, g$change_nw)

g$gent_wave3 <- 0.5*g$change_churn - 0.25*g$change_nw +
  0.125*g$change_house - 0.125*g$change_dep + 0.25

# wave 6
g$change_nw <- ((g$nw2016 - g$nw2009) / g$nw2009)
g$change_churn <- (g$chn2016_2009)
g$change_dep <- ((g$IMD2016 - g$IMD2010) / g$IMD2010)
g$change_house <- (((g$Year.ending.Jun.2016 / g$Mean_Year.ending.Jun.2016) - 
                      (g$Year.ending.Dec.2008 / g$Mean_Year.ending.Dec.2008)) / 
                     (g$Year.ending.Dec.2008 / g$Mean_Year.ending.Dec.2008))


sum(g$change_house >= 1)

g$change_house <- ifelse(g$change_house >=1, 1, g$change_house)

sum(g$change_nw > 1)

g$change_nw <- ifelse(g$change_nw >=1, 1, g$change_nw)

g$gent_wave6 <- 0.5*g$change_churn - 0.25*g$change_nw +
  0.125*g$change_house - 0.125*g$change_dep + 0.25

# wave 9

g$change_nw <- ((g$nw2019 - g$nw2009) / g$nw2009)
g$change_churn <- (g$chn2019_2009)
g$change_dep <- ((g$IMD2019 - g$IMD2010) / g$IMD2010)
g$change_house <- (((g$Year.ending.Jun.2019 / g$Mean_Year.ending.Jun.2019) - 
                      (g$Year.ending.Dec.2008 / g$Mean_Year.ending.Dec.2008)) / 
                     (g$Year.ending.Dec.2008 / g$Mean_Year.ending.Dec.2008))


sum(g$change_house >= 1)

g$change_house <- ifelse(g$change_house >=1, 1, g$change_house)

sum(g$change_nw > 1)

g$change_nw <- ifelse(g$change_nw >=1, 1, g$change_nw)

g$gent_wave9 <- 0.5*g$change_churn - 0.25*g$change_nw +
  0.125*g$change_house - 0.125*g$change_dep + 0.25




# writing g to csv for easy loading

write.csv(g, "g.csv", row.names = FALSE)

g <- read.csv("g.csv") %>%
  select("lsoa11","LSOA.name","Local.authority.code","Local.authority.name",
         "gent2010_2016", "gent_wave3", "gent_wave6", "gent_wave9", 
         "gent_wave3_m", "gent_wave6_m", "gent_wave9_m") 

rm(e,hp,ch,m)

# merging original gentrification index to the dataframe

original.GI <-read.csv("Gentrification Index for Small Areas in London (2010-16).csv")  
original.GI$lsoa11 <- original.GI$LSOACD

g <- merge(g, original.GI, by = "lsoa11")

# examining 10 most and least gentrified gentrified areas from 2010-2016

g[order(g$gent2010_2016, decreasing = TRUE),][1:10, ]
g[order(g$gent2010_2016, decreasing = FALSE), ][1:10, ]


# examining original index

g[order(g$gent, decreasing = TRUE), ][1:10, ]
g[order(g$gent, decreasing = FALSE), ][1:10, ]


# within specific boroughs, analysing most gentrified

top_gentrification <- g %>%
  filter(Local.authority.name == "Wandsworth") %>%
  arrange(desc(gent2010_2016)) %>%
  slice_head(n = 10) %>%
  select(lsoa11,LSOA.name, gent2010_2016)

print(top_gentrification)

# checking normality of scores

shapiro.test(g$gent_wave9)


# checking normality of scores in each borough

p_values <- numeric()

# looping over local authorities to present gentrification normality across the period
# studied

for (i in 1:33) {
  code <- sprintf("E090000%02d", i)
  data_subset <- g$gent_wave9[g$Local.authority.code == code]
  la_name <- unique(g$Local.authority.name[g$Local.authority.code == code])
  test_result <- shapiro.test(data_subset)
  p_values[i - 8] <- test_result$p.value
  cat("Local Authority Code:", code, "- Name:", la_name, "- P-value:", test_result$p.value, "\n")
}

# from 2010-2019, 15 local authorities do not have normally distributed gentrification
# values

# This tells us the Local Authorities with normally distributed
# gentrification values

g <- g %>%
  group_by(Local.authority.code) %>%
  mutate(mean_gent = mean(gent2010_2016, na.rm = TRUE)) %>%
  ungroup()

sorted_means <- g %>%
  arrange(desc(mean_gent)) %>%
  select(Local.authority.name, mean_gent) %>%
  distinct()

print(sorted_means, n = 33)

# compare to original index

g <- g %>%
  group_by(Local.authority.code) %>%
  mutate(mean_gent = mean(gent, na.rm = TRUE)) %>%
  ungroup()

sorted_means <- g %>%
  arrange(desc(mean_gent)) %>%
  select(Local.authority.name, mean_gent) %>%
  distinct()

print(sorted_means, n = 33)

# setting as a csv to use in report
meangentLA <- sorted_means
write.csv(meangentLA, "meangentLA.csv", row.names = FALSE)


# This tells us that the least gentrified borough is city of london, and the most
# is newham

Output.Areas <- readOGR(dsn = "Spatial Data/statistical-gis-boundaries-london/MapInfo",
                        layer = "LSOA_2011_London_gen_MHW")

OA.gent <- merge(Output.Areas, g, by.x = "LSOA11CD", by.y = "lsoa11")


# map for 2010 - 2016 gentrification change comparison between Runnymede and my index
pdf("Gentrification Index Comparison.pdf", width = 8.5, height = 11 * 2)
pdf("Lagged Gentrification 2010-2019.pdf")

map1 <- tm_shape(OA.gent) +
  tm_fill("gent2010_2016",
          palette = "-magma",
          midpoint = NA,
          style = "cont",
          title = "Gentrification Index",
          colorNA = "grey",
          breaks = seq(0.25, 0.7, length.out = 5)) +
  tm_layout(frame = FALSE,
            outer.margins = c(0.02, 0.02, 0.1, 0.02), # Adjust top margin to prevent overlap
            title.position = c("center", "top"),
            legend.outside = TRUE,
            legend.position = c("left", "bottom")) +
  tm_layout("Mattock (2024) Gentrification Index with Lag 2010-2016",
            legend.title.size = 1,
            legend.text.size = 0.6,
            legend.bg.color = NA,
            legend.bg.alpha = 1)

# Adjusting the layout for map2
map2 <- tm_shape(OA.gent) +
  tm_fill("gent",
          palette = "-magma",
          midpoint = NA,
          style = "cont",
          title = "Gentrification Index",
          colorNA = "grey",
          breaks = seq(0.25, 0.7, length.out = 5)) +
  tm_layout(frame = FALSE,
            outer.margins = c(0.02, 0.02, 0.1, 0.02), # Adjust top margin to prevent overlap
            title.position = c("center", "top"),
            legend.outside = TRUE,
            legend.position = c("left", "bottom")) +
  tm_layout("Almeida (2021) Gentrification Index 2010-2016",
            legend.title.size = 1,
            legend.text.size = 0.6,
            legend.bg.color = NA,
            legend.bg.alpha = 1)

grid.newpage()

pushViewport(viewport(layout=grid.layout(2,1)))

print(map1, vp=viewport(layout.pos.col = 1, layout.pos.row =1))
print(map2, vp=viewport(layout.pos.col = 1, layout.pos.row =2))

dev.off()

# comparing this gentrification index to existing samples

pdf('scatter comparison.pdf')
plot(g$gent2010_2016, g$gent, main = "Gentrification Index Comparison",
     xlab = "Author's Calculated Index", ylab = "Almeida (2021) Index",
     pch = 18, frame = FALSE)

abline(lm(g$gent ~ g$gent2010_2016), col = "blue")
dev.off()
# Plotting population churn and ethnic change

plot(g$change_churn, g$change_nw, main = "Gentrification Index Comparison",
     xlab = "Population Churn", ylab = "Ethnic change",
     pch = 18, frame = FALSE)

y <- g$nw_change_2016_2010
x <- g$chn2016_2010

abline(lm(change_churn ~ change_nw, data=g), col = "blue")

# kendalls t coefficient test comparing churn and change in nonwhite pop
?cor.test
cor.test(x, y, method = "kendall")

sum(is.na(g$nw_change_2016_2010))
sum(is.na(g$chn2016_2010))

ggplot(data = g) + 
  geom_smooth(mapping = aes(x = chn2016_2010, y = nw_change_2016_2010))

#####

# constructing the spatial weights index and exploring spatial results


# creating an object that identifies "neighbouring" LSOAs
neighbours <- poly2nb(OA.gent, queen = TRUE)
neighbours

# testing for spatial autocorrelation
listw <- nb2listw(neighbours, style="W")
listw

# producing a spatial lag correlation plot

Lag <- lag.listw(nb2listw(neighbours, style = "W"), OA.gent@data$gent_wave9)

# create an object with gentrification values, switch column to test moran score
# for different waves/ measures

Gentrification <- OA.gent$gent_wave6_m

# plotting lag against marginal gentrification
plot(Gentrification, Lag)

# This code adds the dotted lines where the average values are
abline(h = mean(Gentrification), lty = 2)
abline(v = mean(Gentrification), lty = 2)

#

g$Lag <- Lag


# running a moran test

moran.test(Gentrification, listw, randomisation = TRUE)

# this suggests strong positive spatial autocorrelation throughout greater London


# Using a getis-ord approach to illustrate clustering

Gentrification <- OA.gent$gent_wave9

# creating centroid and joins neighbours within 0 and 2500 units

nb <- dnearneigh(coordinates(OA.gent),0,2500)

# creates listw

nb_lw <- nb2listw(nb, style = 'W', zero.policy = TRUE)

# getis-ord statistic calculated
local_g <- localG(Gentrification, nb_lw)

local_g_sp<- OA.gent

local_g_sp@data <- cbind(OA.gent@data, as.matrix(local_g))
names(local_g_sp)
names(local_g_sp)[33] <- "gstat"

# map the results
pdf("gentrification_clustering_2010_2019.pdf")

tm_shape(local_g_sp) + 
  tm_fill("gstat", palette = "-RdBu", style = "cont", title = "G statistic", midpoint = NA) + 
  tm_layout(frame = FALSE,
            outer.margins = c(0.02, 0.02, 0.1, 0.02), # Adjust top margin to prevent overlap
            title.position = c("center", "top"),
            legend.outside = TRUE,
            legend.position = c("left", "bottom"))  +
  tmap_options(max.categories = 4835)

dev.off()


# creating lagged results for each wave

# for gentrification from beginning of measurement

Lag_wave3 <- lag.listw(nb2listw(neighbours, style = "W"), OA.gent@data$gent_wave3)
Lag_wave6 <- lag.listw(nb2listw(neighbours, style = "W"), OA.gent@data$gent_wave6)
Lag_wave9 <- lag.listw(nb2listw(neighbours, style = "W"), OA.gent@data$gent_wave9)

g$Lag_wave3 <- Lag_wave3
g$Lag_wave6 <- Lag_wave6
g$Lag_wave9 <- Lag_wave9

# for gentrification within each wave

Lag_wave3_m <- lag.listw(nb2listw(neighbours, style = "W"), OA.gent@data$gent_wave3_m)
Lag_wave6_m <- lag.listw(nb2listw(neighbours, style = "W"), OA.gent@data$gent_wave6_m)
Lag_wave9_m <- lag.listw(nb2listw(neighbours, style = "W"), OA.gent@data$gent_wave9_m)

g$Lag_wave3_m <- Lag_wave3_m
g$Lag_wave6_m <- Lag_wave6_m
g$Lag_wave9_m <- Lag_wave9_m

# test descriptive stats for new lagged scores

g[order(g$Lag_wave9, decreasing = TRUE), ][1:10, ]
g[order(g$Lag_wave9, decreasing = FALSE), ][1:10, ]

# visualising spatial gent

OA.gent <- merge(Output.Areas, g, by.x = "LSOA11CD", by.y = "lsoa11")


map <- tm_shape(OA.gent) +
  tm_fill("Lag_wave9",
          palette = "-magma",
          midpoint = NA,
          style = "cont",
          title = "Gentrification Index",
          colorNA = "grey",
          breaks = seq(0.4, 0.8, length.out = 5)) +
  tm_layout(frame = FALSE,
            outer.margins = c(0.02, 0.02, 0.1, 0.02), # Adjust top margin to prevent overlap
            title.position = c("center", "top"),
            legend.outside = TRUE,
            legend.position = c("left", "bottom")) +
  tm_layout("Spatially lagged gentrification",
            legend.title.size = 1,
            legend.text.size = 0.6,
            legend.bg.color = NA,
            legend.bg.alpha = 1)

print(map)

gwlag <- g[, -c(12,13)]

write.csv(gwlag, file = "gwlag.csv", row.names = FALSE)
g <- read.csv("gwlag.csv")

#####

# Loading and cleaning understanding society data

setwd("C:/Users/joema/OneDrive/Year 3/Dissertation/Gentrification Index/Understanding Society/UKDA-7248-stata/stata/stata13/ukhls")

us <- list()

# loading lsoa data
for (i in 1:10) {
  file_name <- sprintf("%s_lsoa11_protect.dta", letters[i])
  dataset_name <- paste(letters[i], "us", sep = "_")
  assign(dataset_name, read_dta(file_name))
}

# adding a column for the wave number to each dataset

# Loop through the sequence 1 to 10
for (i in 1:10) {
  data_frame_name <- paste(letters[i], "us", sep = "_")
  data_frame <- get(data_frame_name)
  data_frame$wave <- i
  assign(data_frame_name, data_frame)
}


# dropping prefixes for household identifiers and lsoas so they can be merged
# later

data_frame_names <- c("a_us", "b_us", "c_us", "d_us", "e_us", "f_us", "g_us", "h_us", "i_us", "j_us")

for (df_name in data_frame_names) {
  df <- get(df_name)
  new_col_names <- sapply(names(df), function(col_name) {
    underscore_pos <- regexpr("_", col_name)
    if (underscore_pos > 0) {
      return(substr(col_name, underscore_pos + 1, nchar(col_name)))
    } else {
      return(col_name)
    }
  })
  names(df) <- new_col_names
  assign(df_name, df)
}


# subsetting for relevant greater london lsoas

prefixes <- letters[1:10]

for (prefix in prefixes) {
  var_name <- paste0(prefix, "_us")
  current_dataset <- get(var_name)
  filtered_dataset <- current_dataset[current_dataset$lsoa11 %in% LSOA11CD, ]
  assign(var_name, filtered_dataset)
}

# merging geographic identifier datasets into long-format data

us <- rbind(a_us,b_us,c_us,d_us,e_us,f_us,g_us,h_us,i_us,j_us)

us_c_f_i <- rbind(c_us, f_us, i_us)


# Just some housekeeping before moving onto the next section

rm(a_us,b_us,c_us,d_us,e_us,f_us,g_us,h_us,i_us,j_us,
   current_dataset, data_frame, filtered_dataset, us, data_frame_name, 
   data_frame_names, df_name, file_name, i, prefix, prefixes, var_name, 
   dataset_name, new_col_names, df)

write.csv(us_c_f_i, "us369.csv", row.names = FALSE)
us369 <- read.csv("us369.csv")


# loading understanding society questionnaire data
setwd("C:/Users/joema/OneDrive/Year 3/Dissertation/Gentrification Index/Understanding Society/UKDA-6614-stata/stata/stata13_se/ukhls")

# loading in each individual response data frame, merging to household response
# survey data to create homeownership variable and equivalised income 

c_indresp <- read_dta("c_indresp.dta") %>%
  select("c_hidp","c_voteintent","c_pno","pidp","c_dvage","c_psu","c_strata", 
         "c_vote3", "c_distmov_dv", "c_hiqual_dv", "c_jbstat", "c_sex_dv", 
         "c_basrate", "c_ethn_dv", "c_urban_dv", "c_fimnsben_dv", "c_nbrsnci_dv") %>%
  left_join(
    read_dta("c_hhresp.dta", col_select = c("c_tenure_dv", "c_hidp","c_hhsize","c_nchoecd_dv", "c_fihhmnnet1_dv")),
    by = "c_hidp"
  ) 
  
f_indresp <- read_dta("f_indresp.dta") %>%
  select("f_hidp","f_voteintent","f_pno","pidp","f_dvage","f_psu","f_strata", 
         "f_vote3", "f_distmov_dv", "f_hiqual_dv", "f_jbstat", "f_sex_dv", 
         "f_basrate", "f_ethn_dv", "f_urban_dv", "f_fimnsben_dv", "f_nbrsnci_dv") %>%
  left_join(
    read_dta("f_hhresp.dta", col_select = c("f_tenure_dv", "f_hidp", "f_hhsize","f_nchoecd_dv", "f_fihhmnnet1_dv")),
    by = "f_hidp"
  ) 

i_indresp <- read_dta("i_indresp.dta")  %>%
  select("i_hidp","i_voteintent","i_pno","pidp","i_dvage","i_psu","i_strata", 
         "i_vote3", "i_distmov_dv", "i_hiqual_dv", "i_jbstat", "i_sex_dv", 
         "i_basrate", "i_ethn_dv", "i_urban_dv", "i_fimnsben_dv", "i_nbrsnci_dv") %>%
  left_join(
    read_dta("i_hhresp.dta", col_select = c("i_tenure_dv", "i_hidp", "i_hhsize","i_nchoecd_dv", "i_fihhmnnet1_dv")),
    by = "i_hidp"
  ) 


write.csv(c_indresp, "c_indresp.csv", row.names = FALSE)
write.csv(f_indresp, "f_indresp.csv", row.names = FALSE)
write.csv(i_indresp, "i_indresp.csv", row.names = FALSE)

c_indresp <- read.csv("c_indresp.csv")
f_indresp <- read.csv("f_indresp.csv")
i_indresp <- read.csv("i_indresp.csv")

# removing prefixes from individual identifiers

# removing all prefixes with a for loop

dataframe_names <- c("c_indresp", "f_indresp", 
                     "i_indresp")

for (df_name in dataframe_names) {
  df <- get(df_name)
  new_names <- gsub("^[a-z]_", "", names(df))
  names(df) <- new_names
  assign(df_name, df)
}


# Creating a wave variable to indicate the wave of each indresp

prefixes <- c("c", "f", "i")
wave_numbers <- c(3, 6, 9)

for (i in 1:length(prefixes)) {
  data_frame_name <- paste(prefixes[i], "_indresp", sep = "")
  data_frame <- get(data_frame_name)
  data_frame$wave <- wave_numbers[i]
  assign(data_frame_name, data_frame)
}

rm(data_frame, data_frame_name, i, prefixes, df_name, new_names, dataframe_names)


# remove all labels to prevent error codes

data.frame(sapply(c_indresp,class))

c_indresp$voteintent <- as.factor(c_indresp$voteintent)

f_indresp$voteintent <- as.factor(f_indresp$voteintent)

i_indresp$voteintent <- as.factor(i_indresp$voteintent)



c_indresp$vote3 <- as.factor(c_indresp$vote3)

f_indresp$vote3 <- as.factor(f_indresp$vote3)

i_indresp$vote3 <- as.factor(i_indresp$vote3)


# binding data into long format for voteintent

long <- rbind(c_indresp, f_indresp, i_indresp)


# adding the existing weighting by each persons cross-wave identifier

adjusted_weight <- read.csv("new.weights.csv")

# Now you can join dataset1_aggregated with your second dataset
long <- inner_join(adjusted_weight, long, by = "pidp")

# testing survey design functions

sum(is.na(long$i_indscub_lw1))

long_filtered <- long[!is.na(long$mean_i_indscub_lw1), ]

svy_long <- svydesign(id=~psu, strata=~strata,
                      weights=~mean_i_indscub_lw1, data=long)

svymean(~voteintent, svy_long, na.rm=TRUE)
table(long$voteintent)

# dealing with strata with only a single PSU

options(survey.lonely.psu="adjust")

# Merging the filtered long dataset with lsoa identifiers

merged <- merge(long, us369, by = c("hidp","wave"), all = TRUE)

# Now subsetting for london 

merged_filtered <- merged[!is.na(merged$lsoa11),]

sum(is.na(merged_filtered))
dim(merged_filtered)

333984/(19613*21)

# there are a large proportion of NA values because we have merged with hidp values from the geographical
# identifier dataset i.e. these additional individuals are not included in our analyses

# removing respondents introduced by the merge by removing zero weighted observations

merged_filtered <- merged_filtered[!is.na(merged_filtered$mean_i_indscub_lw1), ]


sum(is.na(merged_filtered))
dim(merged_filtered)

124/(2920*23)

# now NA values make up less than 0.2% of data

# writing up and saving with variables for analysis and nonzero response weights

write.csv(merged_filtered, file = "merged_filtered.csv", row.names = FALSE)

merged_filtered <- read.csv("merged_filtered.csv")

# Now merge gentrification data and run fixed effects regression, taking complex survey design into account

# Creating a new gent variable in merged_filtered, 
# merging gent2011 values to where wave = 3, gent2015 to where wave =6 and gent2018 to where gent = 9

g$lsoa11 <- as.character(g$lsoa11)
merged_filtered$lsoa11 <- as.character(merged_filtered$lsoa11)

merged_filtered <- merged_filtered %>%
  left_join(g, by = "lsoa11") %>%
  mutate(gent_data = case_when(
    wave == 3 ~ gent_wave3,
    wave == 6 ~ gent_wave6,
    wave == 9 ~ gent_wave9,
    TRUE ~ NA_real_ 
  )) %>%
  mutate(lag_data = case_when(
    wave == 3 ~ Lag_wave3,
    wave == 6 ~ Lag_wave6,
    wave == 9 ~ Lag_wave9,
    TRUE ~ NA_real_
  )) %>%
  mutate(gent_m_data = case_when(
    wave == 3 ~ gent_wave3_m,
    wave == 6 ~ gent_wave6_m,
    wave == 9 ~ gent_wave9_m,
    TRUE ~ NA_real_ 
  )) %>%
  mutate(lag_m_data = case_when(
    wave == 3 ~ Lag_wave3_m,
    wave == 6 ~ Lag_wave6_m,
    wave == 9 ~ Lag_wave9_m,
    TRUE ~ NA_real_
  )) %>%
  select(-c(gent_wave3, gent_wave6, gent_wave9, Lag_wave3, Lag_wave6, Lag_wave9,
            gent_wave3_m, gent_wave6_m, gent_wave9_m, Lag_wave3_m, Lag_wave6_m, Lag_wave9_m))


# creating equivalised income variable

merged_filtered$hhead <- merged_filtered$hhsize-merged_filtered$nchoecd_dv-1
merged_filtered$equiv <- 1+merged_filtered$hhead*0.5+merged_filtered$nchoecd_dv*0.3
merged_filtered$eqhhmnnet <- merged_filtered$fihhmnnet1_dv/merged_filtered$equiv
merged_filtered$eqhhmnnet <- merged_filtered$eqhhmnnet/1000

sum(is.na(merged_filtered$eqhhmnnet))

# coding NA values for job status
merged_filtered$jbsat <- ifelse(merged_filtered$jbstat < 1, NA, merged_filtered$jbstat)

# NA values for qualification

merged_filtered$hiqual_dv <- ifelse(merged_filtered$hiqual_dv < 0, NA, merged_filtered$hiqual_dv)

# zero values for longstanding residency
sum(merged_filtered$distmov_dv == -9)
merged_filtered$distmov_dv <- ifelse(merged_filtered$distmov_dv %in% c(-8, -1), 0, merged_filtered$distmov_dv)
sum(merged_filtered$distmov_dv == 0)

# coding NA for sex variables

merged_filtered$sex_dv <- ifelse(merged_filtered$sex_dv %in% c(-9, 0), NA, merged_filtered$sex_dv)

# coding NA values for ethnicity

merged_filtered$ethn_dv <- ifelse(merged_filtered$ethn_dv %in% c(-9), NA, merged_filtered$ethn_dv)

# coding NA values for urban/rural

merged_filtered$urban_dv <- ifelse(merged_filtered$urban_dv %in% c(-9), NA, merged_filtered$urban_dv)

# coding NA for whether recipient receives benefits

merged_filtered$fimnsben_dv <- ifelse(merged_filtered$fimnsben_dv %in% c(-9), NA, merged_filtered$fimnsben_dv)

# coding NA for voting intention
sum(merged_filtered$voteintent %in% c(-2,-9, -7))
# -9, -7 and -2 values can be imputed in theory

merged_filtered$voteintent <- ifelse(merged_filtered$voteintent %in% c(-9,-8,-7,-2,-1,11), NA, merged_filtered$voteintent)

# coding NA values for vote3

merged_filtered$vote3 <- ifelse(merged_filtered$vote3 %in% c(-9,-8,-7,-2,-1,94,95,96), NA, merged_filtered$vote3)

#####

# setting NA values for homeownership

merged_filtered$tenure_dv <- ifelse(merged_filtered$tenure_dv == -9, NA, merged_filtered$tenure_dv)
merged_filtered$ownership <- ifelse(merged_filtered$tenure_dv %in% c(1,2), 1, 0)


# creating a term for the interaction between time and treatment

merged_filtered$time_treat <- merged_filtered$wave*merged_filtered$gent_data

# interaction term between distance moved and voter intention

merged_filtered$longstanding <- ifelse(merged_filtered$distmov_dv == 0, 1, 0)

merged_filtered$longxgent <- merged_filtered$longstanding*merged_filtered$gent_data

# releveling job status

merged_filtered$jbstat <- factor(merged_filtered$jbstat)  
merged_filtered$jbstat <- relevel(merged_filtered$jbstat, ref = "2")

# Now we can actually perform fixed effects regression 


# converting LHS independent variables to numeric

merged_filtered$dvage <- as.numeric(as.character(merged_filtered$dvage))

merged_filtered$voteintent <- as.numeric(as.character(merged_filtered$voteintent))

merged_filtered$lsoa11 <- as.factor(merged_filtered$lsoa11)

class(merged_filtered$lsoa11)

# interaction term for nw and gent

merged_filtered$nw <- ifelse(merged_filtered$ethn_dv != 1, 1, 0)

# making coefficients for benefits variable more sensible

merged_filtered$fimnsben_dv <- merged_filtered$fimnsben_dv/1000

# conducting FE


# conducting fixed effects with within-wave gentrification variable instead

merged_filtered$longxgent_m <- merged_filtered$longstanding*merged_filtered$gent_m_data
merged_filtered$time_treat_gent_m <- merged_filtered$gent_m_data*merged_filtered$wave

model <- feols(voteintent ~ gent_m_data + longxgent_m + time_treat_gent_m | as.factor(wave) + as.factor(pidp), 
               data=merged_filtered,vcov = ~psu, weights = merged_filtered$mean_i_indscub_lw1)

summary(model)

plot_model(model)
plot_models(model)
tab_model(model)


# within-wave lagged gentrification data

merged_filtered$longxlag_m <- merged_filtered$longstanding*merged_filtered$lag_m_data
merged_filtered$time_treat_lag_m <- merged_filtered$lag_m_data*merged_filtered$wave

model <- feols(voteintent ~ lag_m_data + longxlag_m + time_treat_lag_m | as.factor(wave) + as.factor(pidp), 
               data=merged_filtered, cluster = "psu", weights = merged_filtered$mean_i_indscub_lw1)

summary(model)

plot_model(model, type = "res")
tab_model(model)

# now accounting for time-varying sociodemographic characteristics

model <- feols(voteintent ~  lag_m_data + longxlag_m + time_treat_lag_m  + 
                 eqhhmnnet + dvage + as.factor(jbstat) + as.factor(hiqual_dv) +
                 fimnsben_dv + ownership
               | as.factor(wave) +as.factor(pidp), 
               data=merged_filtered, vcov = ~psu, weights = merged_filtered$mean_i_indscub_lw1)

summary(model)

plot_model(model, type = "res")

resids <- residuals(model)
plot(resids, main = "Residuals of Fixed Effects Model", ylab = "Residuals", xlab = "Index")
abline(h = 0, col = "red")

# running model with a balanced panel

complete_pidps <- merged_filtered %>%
  group_by(pidp) %>%
  summarise(all_waves_present = all(unique(merged_filtered$wave) %in% wave)) %>%
  filter(all_waves_present) %>%
  pull(pidp)  

subset_data <- merged_filtered %>%
  filter(pidp %in% complete_pidps)

model <- feols(voteintent ~ lag_m_data + longxlag_m + time_treat_lag_m  + dvage + 
                 eqhhmnnet + as.factor(jbstat) + as.factor(hiqual_dv) +
                 fimnsben_dv + ownership
               | as.factor(wave) +as.factor(pidp), 
               data=subset_data,vcov = ~psu, weights = subset_data$mean_i_indscub_lw1)

summary(model)

plot_model(model)

# time-lagged gentrification for within-wave measure
# NOTE that this code lags gentrification scores FORWARD by one wave,
# estimating the impact of past gentrification rates on present intention to vote

merged_filtered <- merged_filtered %>%
  arrange(pidp, wave) %>%  
  group_by(pidp) %>%
  mutate(placebo_m = lag(lag_m_data, 1)) 

merged_filtered$longxplacebo_m <- merged_filtered$placebo_m*merged_filtered$longstanding
merged_filtered$time_treat_placebo_m <- merged_filtered$placebo_m*merged_filtered$wave

model <- feols(voteintent ~ placebo_m + longxplacebo_m + time_treat_placebo_m +
                 eqhhmnnet + dvage + as.factor(hiqual_dv) + as.factor(jbstat) +
                 ownership + fimnsben_dv| as.factor(wave) + as.factor(pidp), 
               data=merged_filtered,vcov = ~psu, weights = merged_filtered$mean_i_indscub_lw1)

summary(model)

# testing composite gentrification measure

# simple model for gentrification, time treatment, interaction between longstanding
# and gentrification & longstanding residency

model <- feols(voteintent ~ gent_data + longxgent + time_treat| as.factor(wave) + 
                 as.factor(pidp), 
               data=merged_filtered, cluster = "psu", weights = merged_filtered$mean_i_indscub_lw1)

summary(model)

plot_model(model)
plot_models(model)
tab_model(model)

# simple model for cumulative lagged score

merged_filtered$longxlag <- merged_filtered$lag_data*merged_filtered$longstanding
merged_filtered$time_treat_lag <- merged_filtered$lag_data*merged_filtered$wave

model <- feols(voteintent ~ lag_data + longxlag + time_treat_lag| as.factor(wave) + as.factor(pidp), 
               data=merged_filtered,cluster = "psu", weights = merged_filtered$mean_i_indscub_lw1)

summary(model)

# accounting for demographic characteristics: age, employment, education and benefits income

model <- feols(voteintent ~ lag_data + longxlag + time_treat_lag  + dvage + 
                 eqhhmnnet + as.factor(jbstat) + as.factor(hiqual_dv) +
                 fimnsben_dv + ownership|  as.factor(wave) + as.factor(pidp), 
               data=merged_filtered,cluster = "psu", weights = merged_filtered$mean_i_indscub_lw1)

summary(model)
vif(model)

# balanced panel for cumulative gentrification

complete_pidps <- merged_filtered %>%
  group_by(pidp) %>%
  summarise(all_waves_present = all(unique(merged_filtered$wave) %in% wave)) %>%
  filter(all_waves_present) %>%
  pull(pidp)  

subset_data <- merged_filtered %>%
  filter(pidp %in% complete_pidps)

model <- feols(voteintent ~ lag_data + longxlag  + time_treat_lag + dvage + 
                 eqhhmnnet  + as.factor(hiqual_dv) + as.factor(jbstat) +
                 ownership + fimnsben_dv
               | as.factor(wave) +as.factor(pidp), 
               data=subset_data,vcov = ~psu, weights = subset_data$mean_i_indscub_lw1)
summary(model)

## time lagged gentrification for composite gentrification index

merged_filtered <- merged_filtered %>%
  arrange(pidp, wave) %>%  
  group_by(pidp) %>%
  mutate(placebo.future = lag(lag_data, 1)) 

# this shifts gentrification values ahead one period due to the nature of the dataframe

merged_filtered$longxplacebo1 <- merged_filtered$placebo.future*merged_filtered$longstanding
merged_filtered$time_treat_placebo1 <- merged_filtered$placebo.future*merged_filtered$wave

model <- feols(voteintent ~ placebo.future + longxplacebo1 + time_treat_placebo1 +
                 dvage + eqhhmnnet + ownership + as.factor(hiqual_dv) + 
                 as.factor(jbstat) + fimnsben_dv | as.factor(wave) + 
                 as.factor(pidp), 
               data=merged_filtered,vcov = ~psu, weights = merged_filtered$mean_i_indscub_lw1)

summary(model)


#####

# placebo excluding influential outliers for within-wave specification
model <- feols(voteintent ~  lag_m_data + longxlag_m + time_treat_lag_m  + 
                 eqhhmnnet + dvage + as.factor(jbstat) + as.factor(hiqual_dv) +
                 fimnsben_dv + ownership
               | as.factor(wave) +as.factor(pidp), 
               data=merged_filtered, vcov = ~psu, weights = merged_filtered$mean_i_indscub_lw1)

std_residuals <- residuals(model)
plot(std_residuals, type = "h", main = "Standardized Residuals", xlab = "Observation Index", ylab = "Standardized Residuals")
threshold <- 0.5
outliers <- which(abs(std_residuals) > threshold)
clean_data <- subset_data[-outliers, ]
clean_model <- feols(voteintent ~ lag_m_data + longxlag_m + time_treat_lag_m + dvage + 
                       eqhhmnnet + as.factor(hiqual_dv) + as.factor(jbstat) +
                       ownership + fimnsben_dv
                     | as.factor(wave) + as.factor(pidp), 
                     data = clean_data, vcov = ~psu, weights = clean_data$mean_i_indscub_lw1)

summary(clean_model)

# past placebo

merged_filtered <- merged_filtered %>%
  arrange(pidp, wave) %>%  
  group_by(pidp) %>%
  mutate(placebo_m = lead(lag_m_data, 1)) 

merged_filtered$longxplacebo_m <- merged_filtered$placebo_m*merged_filtered$longstanding
merged_filtered$time_treat_placebo_m <- merged_filtered$placebo_m*merged_filtered$wave

model <- feols(voteintent ~ placebo_m + longxplacebo_m  + time_treat_placebo_m +
                 eqhhmnnet + dvage + as.factor(hiqual_dv) + as.factor(jbstat) +
                 ownership + fimnsben_dv
               | as.factor(wave) + as.factor(pidp), 
               data=merged_filtered,vcov = ~psu, weights = merged_filtered$mean_i_indscub_lw1)

summary(model)



# excluding outliers for within wave future spec

std_residuals <- residuals(model)

plot(std_residuals, type = "h", main = "Standardized Residuals", xlab = "Observation Index", ylab = "Standardized Residuals")
threshold <- 3
outliers <- which(abs(std_residuals) > threshold)
clean_data <- merged_filtered[-outliers, ]
clean_model <- feols(voteintent ~ placebo_m + longxplacebo_m + time_treat_placebo_m +
                       eqhhmnnet + dvage + as.factor(hiqual_dv) + as.factor(jbstat) +
                       ownership + fimnsben_dv
                     | as.factor(wave) + as.factor(pidp), 
                     data = clean_data, vcov = ~psu, weights = clean_data$mean_i_indscub_lw1)
summary(clean_model)

# balanced panel for future spec

complete_pidps <- merged_filtered %>%
  group_by(pidp) %>%
  summarise(all_waves_present = all(unique(merged_filtered$wave) %in% wave)) %>%
  filter(all_waves_present) %>%
  pull(pidp)  

subset_data <- merged_filtered %>%
  filter(pidp %in% complete_pidps)

model <- feols(voteintent ~ placebo_m + longxplacebo_m + time_treat_placebo_m +
                 eqhhmnnet + dvage + as.factor(hiqual_dv) + as.factor(jbstat) +
                 ownership + fimnsben_dv | as.factor(wave) + 
                 as.factor(pidp), 
               data=subset_data,vcov = ~psu, weights = subset_data$mean_i_indscub_lw1)

summary(model)


# random spatial placebo where random sample is across all 4835 LSOAs in each wave
set.seed(50) 

# generate random samples from the specified columns
generate_samples <- function(num_samples) {
  all_samples <- c(g$Lag_wave3_m, g$Lag_wave6_m, g$Lag_wave9_m)
  random_samples <- sample(all_samples, size = num_samples, replace = TRUE)
  return(random_samples)
}
# creating random spatial placebo variables from samples
randomised_lag_data <- generate_samples(nrow(merged_filtered))
merged_filtered$randomised_lag_data <- randomised_lag_data

merged_filtered <- merged_filtered %>%
  mutate(longxrandom = randomised_lag_data * longstanding,
         time_treat_random = randomised_lag_data * wave)

model <- feols(voteintent ~ randomised_lag_data + dvage + longxrandom + time_treat_random +
                 eqhhmnnet + as.factor(hiqual_dv) +
                 ownership + as.factor(jbstat) + fimnsben_dv | as.factor(wave) + as.factor(pidp), 
               data = merged_filtered, vcov = ~psu, weights = merged_filtered$mean_i_indscub_lw1)

summary(model)




# cumulative gentrification excluding outliers

# placebo excluding influential outliers for within-wave specification
model <- feols(voteintent ~  lag_data + longxlag + time_treat_lag  + 
                 eqhhmnnet + dvage + as.factor(jbstat) + as.factor(hiqual_dv) +
                 fimnsben_dv + ownership
               | as.factor(wave) +as.factor(pidp), 
               data=merged_filtered, vcov = ~psu, weights = merged_filtered$mean_i_indscub_lw1)

# plotting residuals
std_residuals <- residuals(model)
plot(std_residuals, type = "h", main = "Standardized Residuals", xlab = "Observation Index", ylab = "Standardized Residuals")

# setting residual threshold

threshold <- 3
outliers <- which(abs(std_residuals) > threshold)
clean_data <- subset_data[-outliers, ]
clean_model <- feols(voteintent ~ lag_m_data + longxlag_m + time_treat_lag_m + dvage + 
                       eqhhmnnet + as.factor(hiqual_dv) + as.factor(jbstat) +
                       ownership + fimnsben_dv
                     | as.factor(wave) + as.factor(pidp), 
                     data = clean_data, vcov = ~psu, weights = clean_data$mean_i_indscub_lw1)

summary(clean_model)



#conducting a past placebo test with a time-lagged variable (i.e. treatment for waves 6 and 9 is lagged
# to waves 3 and 6 instead)

merged_filtered <- merged_filtered %>%
  arrange(pidp, wave) %>%  
  group_by(pidp) %>%
  mutate(placebo = lead(lag_data, 1)) 

merged_filtered$longxplacebo <- merged_filtered$placebo*merged_filtered$longstanding
merged_filtered$time_treat_placebo <- merged_filtered$placebo*merged_filtered$wave


model <- feols(voteintent ~ placebo + longxplacebo + time_treat_placebo +
                 dvage + eqhhmnnet + ownership + as.factor(hiqual_dv) + 
                 as.factor(jbstat) + fimnsben_dv| as.factor(wave) + as.factor(pidp), 
               data=merged_filtered,vcov = ~psu, weights = merged_filtered$mean_i_indscub_lw1)

summary(model)



# residual test for future placebo cumulative spec

std_residuals <- residuals(model)
plot(std_residuals, type = "h", main = "Standardized Residuals", xlab = "Observation Index", ylab = "Standardized Residuals")

# setting residuals
threshold <- 0.1
outliers <- which(abs(std_residuals) > threshold)
clean_data <- merged_filtered[-outliers, ]
clean_model <- feols(voteintent ~ placebo.future + longxplacebo1 + time_treat_placebo1 +
                       dvage + eqhhmnnet + ownership + as.factor(hiqual_dv) + 
                       as.factor(jbstat) + fimnsben_dv
                     | as.factor(wave) + as.factor(pidp), 
                     data = clean_data, vcov = ~psu, weights = clean_data$mean_i_indscub_lw1)
summary(clean_model)


# testing time lag with balanced panel

complete_pidps <- merged_filtered %>%
  group_by(pidp) %>%
  summarise(all_waves_present = all(unique(merged_filtered$wave) %in% wave)) %>%
  filter(all_waves_present) %>%
  pull(pidp)  

subset_data <- merged_filtered %>%
  filter(pidp %in% complete_pidps)

model <- feols(voteintent ~ placebo.future + longxplacebo1 + time_treat_placebo1 +
                 dvage + eqhhmnnet + ownership + as.factor(hiqual_dv) + 
                 as.factor(jbstat) + fimnsben_dv | as.factor(wave) + 
                 as.factor(pidp), 
               data=subset_data,vcov = ~psu, weights = subset_data$mean_i_indscub_lw1)

summary(model)

# testing future placebo with random spatial lag

set.seed(50) 

# generate random samples from the specified columns
generate_samples <- function(num_samples) {
  all_samples <- c(g$Lag_wave3, g$Lag_wave6, g$Lag_wave9)
  random_samples <- sample(all_samples, size = num_samples, replace = TRUE)
  return(random_samples)
}
# creating random spatial placebo variables from samples
randomised_lag_data <- generate_samples(nrow(merged_filtered))
merged_filtered$randomised_lag_data <- randomised_lag_data

merged_filtered <- merged_filtered %>%
  mutate(longxrandom = randomised_lag_data * longstanding,
         time_treat_random = randomised_lag_data * wave)

model <- feols(voteintent ~ randomised_lag_data + dvage + longxrandom + time_treat_random +
                 eqhhmnnet + as.factor(hiqual_dv) +
                 ownership + as.factor(jbstat) + fimnsben_dv | as.factor(wave) + as.factor(pidp), 
               data = merged_filtered, vcov = ~psu, weights = merged_filtered$mean_i_indscub_lw1)

summary(model)

# random spatial placebo across composite function

#####

# conducting descriptive analysis of LSOA's in the main models

# analyse the LSOA's included in the analysis

subset_mf <- merged_filtered[!is.na(merged_filtered$voteintent) & !is.na(merged_filtered$eqhhmnnet),]

# proportion of wave 3 respondents that last until wave 9

wave3_ids <- unique(subset_mf[subset_mf$wave == 3, "pidp"])
wave6_ids <- unique(subset_mf[subset_mf$wave == 6, "pidp"])
wave9_ids <- unique(subset_mf[subset_mf$wave == 9, "pidp"])

wave3_in_wave6 <- length(intersect(wave3_ids, wave6_ids)) / length(wave3_ids)
wave3_in_wave9 <- length(intersect(wave3_ids, wave9_ids)) / length(wave3_ids)

print(paste("Proportion of Wave 3 IDs in Wave 6:", wave3_in_wave6))
print(paste("Proportion of Wave 3 IDs in Wave 9:", wave3_in_wave9))

# proportion of LSOAs that are the same from wave 3 to 6 and 3 to 9

wave3_ids <- unique(subset_mf[subset_mf$wave == 3, "lsoa11"])
wave6_ids <- unique(subset_mf[subset_mf$wave == 6, "lsoa11"])
wave9_ids <- unique(subset_mf[subset_mf$wave == 9, "lsoa11"])

wave3_in_wave6 <- length(intersect(wave3_ids, wave6_ids)) / length(wave3_ids)
wave3_in_wave9 <- length(intersect(wave3_ids, wave9_ids)) / length(wave3_ids)

print(paste("Proportion of Wave 3 IDs in Wave 6:", wave3_in_wave6))
print(paste("Proportion of Wave 3 IDs in Wave 9:", wave3_in_wave9))


plot(density(subset_mf$gent2010_2016[subset_mf$wave == 9], na.rm = TRUE), main="Density Plot of Variable", xlab="Variable", ylab="Density")

mean(subset_mf$gent_data[subset_mf$wave == 3])
sum(subset_mf$gent_data[subset_mf$wave == 3] < mean(subset_mf$gent_data[subset_mf$wave == 3]))
# 504 above the mean and 484 below the mean

mean(subset_mf$gent_data[subset_mf$wave == 6])
sum(subset_mf$gent_data[subset_mf$wave == 6] < mean(subset_mf$gent_data[subset_mf$wave == 6]))
# 507 above the mean and 466 below the mean

mean(subset_mf$gent_data[subset_mf$wave == 9])
sum(subset_mf$gent_data[subset_mf$wave == 9] < mean(subset_mf$gent_data[subset_mf$wave == 9]))
# 515 above the mean and 442 below the mean

summary_mf_w3 <- subset_mf %>%
  filter(wave == 3) %>%
  group_by(lsoa11) %>%
  summarise(
    count = n(),
    local_authority_name = first(Local.authority.name),
    wave = first(wave),
    .groups = 'drop'
  )

summary_mf_w6 <- subset_mf %>%
  filter(wave == 6) %>%
  group_by(lsoa11) %>%
  summarise(
    count = n(),
    local_authority_name = first(Local.authority.name),
    wave = first(wave),
    .groups = 'drop'
  )

summary_mf_w9 <- subset_mf %>%
  filter(wave == 9) %>%
  group_by(lsoa11) %>%
  summarise(
    count = n(),
    local_authority_name = first(Local.authority.name),
    wave = first(wave),
    .groups = 'drop'
  )

# number of observations in each wave

sum(summary_mf_w3$count)
sum(summary_mf_w6$count)
sum(summary_mf_w9$count)

num_unique_lsoa11 <- subset_mf %>%
  distinct(lsoa11) %>%
  nrow()

# Print the number of unique LSOAs
print(num_unique_lsoa11)

OA.model_w3 <- merge(Output.Areas, summary_mf_w3, by.x = "LSOA11CD", by.y = "lsoa11")
OA.model_w6 <- merge(Output.Areas, summary_mf_w6, by.x = "LSOA11CD", by.y = "lsoa11")
OA.model_w9 <- merge(Output.Areas, summary_mf_w9, by.x = "LSOA11CD", by.y = "lsoa11")

sum(is.na(OA.model_w9@data))

max(summary_mf_w3$count, na.rm = TRUE)
max(summary_mf_w6$count, na.rm = TRUE)
max(summary_mf_w9$count, na.rm = TRUE)

# creating a map of respondent locations for visualisation on spread across
# london 

pdf("survey_response1.pdf", width = 30, height = 10)

model1 <- tm_shape(OA.model_w3) +
  tm_fill("count",
          palette = "-magma",
          breaks = c(1, 2, 3, 4, 5),
          labels = c("1", "2", "3", "4", "5"),
          colorNA = "grey") +
  tm_layout(frame = FALSE,
            legend.position = c("right", "bottom"),
            legend.outside = TRUE, 
            legend.outside.position = "right")


model2 <- tm_shape(OA.model_w6) +
  tm_fill("count",
          palette = "-magma",
          breaks = c(1, 2, 3, 4, 5, 6),
          labels = c("1", "2", "3", "4", "5", "6"),
          colorNA = "grey") +
  tm_layout(frame = FALSE,
            legend.position = c("right", "bottom"),
            legend.outside = TRUE, 
            legend.outside.position = "right")


model3 <- tm_shape(OA.model_w9) +
  tm_fill("count",
          palette = "-magma",
          breaks = c(1, 2, 3, 4, 5),
          labels = c("1", "2", "3", "4", "5"),
          colorNA = "grey") +
  tm_layout(frame = FALSE,
            legend.position = c("right", "bottom"),
            legend.outside = TRUE, 
            legend.outside.position = "right")

grid.newpage()

pushViewport(viewport(layout=grid.layout(1,3)))

print(model1, vp=viewport(layout.pos.col = 1, layout.pos.row =1))
print(model2, vp=viewport(layout.pos.col = 2, layout.pos.row =1))
print(model3, vp=viewport(layout.pos.col = 3, layout.pos.row =1))

dev.off()



# now create a dataframe for responses per borough and wave

summary_authority <- subset_mf %>%
  group_by(Local.authority.name, wave) %>%
  summarise(count = n(),# Assumes local.authority.name is consistent within each lsoa11
            .groups = 'drop'  # This option drops the grouping, so summary_mf is no longer grouped
  )

mean(summary_mf$gentrification)
mean(merged_filtered$gent2010_2016)

sum(subset_mf$wave ==3)

unique_lsoa11_wave3 <- unique(subset_mf$lsoa11[subset_mf$wave == 9])
length(unique_lsoa11_wave3)


sum(summary_mf$gentrification < 0.5001791)


# finding the most frequently occurring borough for each wave

mode_value <- subset_mf %>%
  filter(wave == 3) %>%
  count(LSOA.name, sort = TRUE) %>%
  slice(1) %>%
  pull(LSOA.name)

# Print the mode
print(mode_value)

# finding the least common value
least_common_value <- subset_mf %>%
  filter(wave == 9) %>%
  count(LSOA.name, sort = TRUE) %>%
  arrange(n) %>%
  slice(1) %>%
  pull(LSOA.name)

print(least_common_value)


wide_data <- pivot_wider(
  data = summary_authority, 
  names_from = wave,   # Column that will turn into new header names
  values_from = count  # Column that contains the values for the new columns
)

write.csv(wide_data, file = "response over waves and boroughs.csv", row.names = FALSE)


