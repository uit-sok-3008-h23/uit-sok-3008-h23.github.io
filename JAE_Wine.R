
# I downloaded the wine data, and saved it on my server account
library(haven)
friberg_gronqvist_wine_data <- read_dta("C:/Users/dki007/OneDrive - UiT Office 365/Econometrics_Teaching_Fall_2022/SOK-3008_Fall_2022/friberg_gronqvist_wine_data.dta")

friberg_gronqvist_wine_data

library(mosaic)
library(tidyverse)

tally(~year, data=friberg_gronqvist_wine_data)
tally(~name, data=friberg_gronqvist_wine_data)
tally(~segm, data=friberg_gronqvist_wine_data)

friberg_gronqvist_wine_data <- friberg_gronqvist_wine_data %>% 
  mutate(Month_Yr = format(as.Date(date), "%Y-%m"))

friberg_gronqvist_wine_data %>% group_by(segm, Month_Yr) %>% 
  summarise(q=sum(litre), x=sum(litre*price)) -> dframe

dframe <- dframe %>% mutate(p=x/q)

# From long to wide
dframe %>% select(segm, Month_Yr, q) %>% tidyr::spread(segm, q) -> qdf
dframe %>% select(segm, Month_Yr, x) %>% tidyr::spread(segm, x) -> xdf
dframe %>% select(segm, Month_Yr, p) %>% tidyr::spread(segm, p) -> pdf

names(qdf) <- c("Month_Yr","q.red","q.spa","q.whi")
names(xdf) <- c("Month_Yr","x.red","x.spa","x.whi")
names(pdf) <- c("Month_Yr","p.red","p.spa","p.whi")

left_join(qdf,xdf, by="Month_Yr") %>% left_join(.,pdf, by="Month_Yr") -> DF

head(DF)
View(DF)


#-----------------------------------------------------------------------------------


library(haven)
friberg_gronqvist_wine_data <- read_dta("C:/Users/dki007/OneDrive - UiT Office 365/Econometrics_Teaching_Fall_2022/SOK-3008_Fall_2022/friberg_gronqvist_wine_data.dta")

# a tad long data name
wine  <- friberg_gronqvist_wine_data
rm(friberg_gronqvist_wine_data)

wine

library(tidyverse)

# The number of observations per year
wine %>% group_by(year) %>% summarise(nobs=n())

# The name of alle different bottles of wine
wine %>% select(name) %>% unique()

# The number of observations per segment per year
wine %>% group_by(segm,year) %>% summarise(nobs=n())

library(lubridate)

# create a month variable
wine  <- wine %>% mutate(month = month(date))

#' Notice, this is the absolutely most crude way of doing this analysis.
#' I am turning all wine into 3 groups, red, white and sparkling, and aggregating everything on a monthly basis.

# making monthly data with day number 1 for each month
dframe <- wine %>% 
  mutate(year_month = as.Date(strftime(date,"%Y/%m/01"))) %>% 
  group_by(segm, year_month) %>% 
  summarise(quantity=sum(litre), 
            expenditure=sum(litre*price)) %>% 
  mutate(price=expenditure/quantity)

# note many months with NA sales, filtering on date
test <- dframe %>% filter(year_month <= "2004-04-01")

# replace NA with group means
test <- test %>% group_by(segm) %>% 
  mutate_at(vars(-group_cols(),-year_month), ~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))    

# From long to wide
test %>% pivot_wider(names_from = "segm",
                     values_from = c("quantity", "expenditure", "price")) %>% View()

# these data 'could' have been used for a project.
# However, maybe using weekly data <= april 2004, and some specific wines would be much more interesting
