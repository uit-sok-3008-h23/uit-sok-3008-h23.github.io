rm(list=ls())

library(tidyverse)

# The movement files contain sales information at the store level for each upc in a category.
# The information is stored on a weekly basis.
wber <- data.table::fread("http://ansatte.uit.no/oystein.myrland/SOK3008/H2020/wber.csv",
                          colClasses=c("integer","numeric","integer","integer","integer","numeric",
                                       "character","numeric","integer","character","character"))

str(wber)
# remove variables
wber <- wber %>% dplyr::select(-PRICE_HEX, -PROFIT_HEX)
names(wber) <- c("store","upc","week","move","qty","price","sale","profit","ok")

# The UPC files contain a description of each UPC in a category. 
upcber <- data.table::fread("http://ansatte.uit.no/oystein.myrland/SOK3008/H2020/upcber.csv",
                            colClasses=c("integer","numeric","character","character","integer","numeric"))
names(upcber) <- c("com_code","upc","descrip","size","case","nitem")
str(upcber)

# Merge the upc data into the movement data
beer <- left_join(wber, upcber, by="upc")

# save as a R data file
#save(beer, file = "beer.RData")
#save(upcber, file = "upcber.RData")


########################################################################
# Load beer data
rm(list=ls())

library(tidyverse)

dir()
load("beer.RData")
load("upcber.RData")

str(beer)
str(upcber)

# Most sold upc
beer %>% group_by(upc) %>% 
  summarise(aggmove=sum(move)) %>% 
   arrange(desc(aggmove)) %>% 
    mutate(prop=100*aggmove/sum(aggmove)) -> aggitemsUPC

aggitemsUPC <- left_join(aggitemsUPC, upcber, by = "upc")
aggitemsUPC  %>% dplyr::select(upc,aggmove, prop, descrip, size) %>% arrange(desc(prop)) %>% head(.,20)

# Miller lite beer cans 24 pack
# browseURL("https://www.dlmdriveup.com/shopping/item.pl?i=3004)
millerlite24cans <- beer %>% filter(upc==3410057306) %>% filter(move>0) %>% group_by(week) %>% 
  summarise(q1=sum(move), p1=mean(price, na.rm = TRUE))

# Miller draft beer cans and/or bottles 24 pack
millerdraft24 <- beer %>% filter(upc %in% c(3410017306,3410015306)) %>% filter(move>0) %>% group_by(week) %>% 
  summarise(q2=sum(move), p2=mean(price, na.rm = TRUE))

# Old style beer 24 packs
oldstyle24 <- beer %>% filter(upc==7336011301) %>% filter(move>0) %>% group_by(week) %>% 
  summarise(q3=sum(move), p3=mean(price, na.rm = TRUE))

# Budweiser 24 pack
bud24 <- beer %>% filter(upc==1820011168) %>% filter(move>0) %>% group_by(week) %>% 
  summarise(q4=sum(move), p4=mean(price, na.rm = TRUE))

# merge
left_join(millerlite24cans, millerdraft24, by="week") %>% left_join(.,oldstyle24, by="week") %>% 
  left_join(., bud24, by="week") -> beerdata

#####################################################
#####################################################
# Find NA's
beerdata %>% filter(is.na(q3))
apply(is.na(beerdata), 2, which) # position
colSums(is.na(beerdata))

# Indicator for missing
q3_miss_ind <- is.na(beerdata$q3)

##### Imputation of mean on a vector
data <- beerdata

data %>% filter(is.na(q3))
data$q3[is.na(data$q3)] <- mean(data$q3, na.rm = TRUE)
data %>% filter(is.na(q3))

data %>% filter(is.na(p3))
data$p3[is.na(data$p3)] <- mean(data$p3, na.rm = TRUE)
data %>% filter(is.na(p3))

apply(is.na(data), 2, which)


##### Density of q3 pre and post imputation #####
# Density of observed data
plot(density(data$q3[q3_miss_ind == FALSE]),
     lwd = 2, 
     main = "Density Pre and Post Mean Imputation",
     xlab = "q3")

# Density of observed & imputed data
points(density(data$q3), 
       lwd = 2, 
       type = "l", 
       col = "red")

# Legend
legend("topright",
       c("Before Imputation", "After Imputation"),
       lty = 1,
       lwd = 2,
       col = c("black", "red"))

##### Descriptive statistics for q3
# Pre imputation
round(summary(data$q3[q3_miss_ind == FALSE]), 2)
# Post imputation
round(summary(data$q3), 2)


# Fill in the previous value zoo::na.locf()
library(zoo)
beerdata %>% filter(week > 209, week < 212)
# All columns
beerdata %>% do(na.locf(.)) %>% filter(week > 209, week < 212)
# just one column
beerdata %>% mutate(q3=na.locf(q3)) %>% filter(week > 209, week < 212)

# https://cran.r-project.org/web/packages/Amelia/vignettes/amelia.pdf
#library(Amelia)
#https://www.andrewheiss.com/blog/2018/03/07/amelia-tidy-melding/

# Mice
#https://datascienceplus.com/imputing-missing-data-with-r-mice-package/


# Detecting outliers
beerdata <- beerdata %>% do(na.locf(.))
mod <- glm(p1 ~ 1 + week, data = beerdata)
summary(mod)
beerdata$cooksdp1 <- cooks.distance(mod)

# Defining outliers based on 4/n criteria
beerdata$outlierp1 <- ifelse(beerdata$cooksdp1 < 4/nrow(beerdata), "No","Yes")
beerdata$outlierp1_3dm <- ifelse(abs(beerdata$p1-(mean(beerdata$p1, na.rm=TRUE))) < 3, "No","Yes")

mosaic::favstats(~p1, data = beerdata)
sd(beerdata$p1, na.rm=TRUE)

# Inspecting the dataset
beerdata %>% filter(outlierp1=="Yes")
beerdata %>% filter(outlierp1_3dm=="Yes")

ggplot(beerdata) + geom_line(aes(x=week,y=p1)) 

#####################################################
#####################################################

# Miller is miller (pricewise)
# p1 millerlite24cans, p2 millerdraft24, p3 oldstyle24, p4 bud24
ggplot(beerdata) + geom_line(aes(x=week,y=p1)) + geom_line(aes(x=week,y=p2), col="red") +
  geom_line(aes(x=week,y=p3), col="blue") + geom_line(aes(x=week,y=p4), col="orange")

###########################################################

# Miller per can/bottle
rm(millerdraft24,millerlite24cans,oldstyle24,bud24)

aggitemsUPC  %>% dplyr::select(upc,aggmove, prop, descrip, size) %>% arrange(desc(prop)) %>% head(.,20)

# Miller lite beer cans 24 pack
millerlite24cans <- beer %>% filter(upc==3410057306) %>% filter(move>0) %>% group_by(week) %>% 
  summarise(q1=sum(move*24), p1=mean(price/24, na.rm = TRUE))

millerlite12 <- beer %>% filter(upc==3410057602) %>% filter(move>0) %>% group_by(week) %>% 
  summarise(q2=sum(move*12), p2=mean(price/12, na.rm = TRUE))

# merge
left_join(millerlite24cans, millerlite12, by="week") -> beerdata

# Smaller packs higher price per bottle
ggplot(beerdata) + geom_line(aes(x=week,y=p1)) + geom_line(aes(x=week,y=p2), col="red")

