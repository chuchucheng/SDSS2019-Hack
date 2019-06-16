logit = function(p, offset=0.001)
   log( (p+offset)/(1-p+offset) )

# Read in ACS data --------------------------------------------------------
# See http://rpubs.com/kitadasmalley/getACSVars

library(tidycensus)
library(tidyverse)

# census_api_key("a1050f88aa1f7aeb0cf1bfa742b56aa836f43013", install = TRUE)

Sys.getenv("CENSUS_API_KEY")

create_acs_data <- function(this.year) {

  # This looks at the 5 year estimates
  # You can also do "acs1"
  vars <- load_variables(year = this.year,
                        dataset = "acs5",
                        cache = TRUE)

  dim(vars)

  waDem <- get_acs(geography = "tract", year=this.year,
                    state = "WA", county = "King", geometry = TRUE,
                    variables = c(population = "B02001_001",
                                median.gross.rent = "B25064_001",
                                median.household.income = "B19013_001",
                                rent.burden = "B25071_001",
                                white = "B03002_003", 
                                af.am = "B03002_004",
                                hispanic = "B03002_012",
                                am.ind = "B03002_005",
                                asian = "B03002_006",
                                nh.pi = "B03002_007",
                                multiple = "B03002_009",
                                other = "B03002_008"))

  waPct<-as.data.frame(waDem)[,c(1,3:4)]%>%
    spread(variable, estimate)%>%
    mutate(checkTot = white+af.am+hispanic+am.ind+ # looks good!
             asian+nh.pi+multiple+other)%>%
    mutate(pct.white = white/checkTot,
           pct.af.am = af.am/checkTot,
           pct.hispanic = hispanic/checkTot,
           pct.am.ind = am.ind/checkTot,
           pct.asian = asian/checkTot,
           pct.nh.pi = nh.pi/checkTot,
           pct.multiple = multiple/checkTot, 
           pct.other = other/checkTot, 
           year = this.year)

  head(waPct)

  out <- waPct %>% 
    select(GEOID, median.gross.rent, median.household.income, population, rent.burden,
           pct.white, pct.af.am, pct.hispanic, pct.am.ind, pct.asian, pct.nh.pi, pct.multiple, pct.other, year)
}

acs_list <- list(create_acs_data(2011),
                 create_acs_data(2012),
                 create_acs_data(2013),
                 create_acs_data(2014),
                 create_acs_data(2015),
                 create_acs_data(2016))

acs <- as_tibble(bind_rows(acs_list))
table(acs$year)
# 2011 2012 2013 2014 2015 2016 
#  398  398  398  398  398  398 

# Read in Evictions data --------------------------------------------------

evict <- read_csv(file = "evictions.csv") %>% 
          janitor::clean_names(case = "snake") %>% 
          select(-median_gross_rent, -median_household_income, -population, -rent_burden,
                 -pct_white, -pct_af_am, -pct_hispanic, -pct_am_ind, -pct_asian, -pct_nh_pi, -pct_multiple, -pct_other,
                 -name, -parent_location, -low_flag, -imputed, -subbed) %>% 
          filter(year %in% 2011:2016)
          
acs <- acs %>% 
        janitor::clean_names(case = "snake") %>% 
        mutate(geoid = as.double(geoid))

intersect(names(acs), names(evict))
# [1] "geoid" "year" 
table(acs$year)
# 2011 2012 2013 2014 2015 2016 
#  398  398  398  398  398  398 
table(evict$year)
# 2011 2012 2013 2014 2015 2016 
# 1458 1458 1458 1458 1458 1458 

# Why more than 398? Because it has more than just King County

table(table(acs$geoid))
table(table(evict$geoid))
length(unique(acs$geoid)) # 398
length(unique(evict$geoid)) # 1458

dat <- inner_join(evict, acs, by = c("geoid", "year"))
dim(dat) # 2388   27
table(dat$year) # 398
table(table(dat$geoid))
length(unique(dat$geoid))
summary(dat)
# Only a few missing values

NAMES = names(dat)
for(i in 2:ncol(dat)) hist(dat[[i]], main = NAMES[i])

library(gridExtra)
g1 <- dat %>% 
        # ggplot(aes(y = eviction_rate, x = year)) +
        ggplot(aes(y = log(eviction_rate + 0.1), x = year)) +
        geom_point() +
        geom_smooth(col = "magenta", method = "lm", formula = y ~ poly(x, 3))
g2 <- dat %>% 
        # ggplot(aes(y = poverty_rate, x = year)) +
        ggplot(aes(y = log(poverty_rate + 1), x = year)) +
        geom_point() +
        geom_smooth(col = "magenta", method = "lm", formula = y ~ poly(x, 3))
g3 <- dat %>% 
        ggplot(aes(y = pct_renter_occupied, x = year)) +
        geom_point() +
        geom_smooth(col = "magenta", method = "lm", formula = y ~ poly(x, 3))
g4 <- dat %>% 
        # ggplot(aes(y = median_property_value, x = year)) +
        ggplot(aes(y = log(median_property_value + 100000), x = year)) +
        geom_point() +
        geom_smooth(col = "magenta", method = "lm", formula = y ~ poly(x, 3))  # Truncate low and high values?
g5 <- dat %>% 
        # ggplot(aes(y = eviction_filing_rate, x = year)) +
        ggplot(aes(y = log(eviction_filing_rate + 1), x = year)) +
        geom_point() +
        geom_smooth(col = "magenta", method = "lm", formula = y ~ poly(x, 3))
g6 <- dat %>% 
        ggplot(aes(y = median_gross_rent, x = year)) +
        geom_point() +
        geom_smooth(col = "magenta", method = "lm", formula = y ~ poly(x, 3)) # What happened in 2015 and 2016?
g7 <- dat %>% 
        ggplot(aes(y = median_household_income, x = year)) +
        geom_point() +
        geom_smooth(col = "magenta", method = "lm", formula = y ~ poly(x, 3))
g8 <- dat %>% 
        ggplot(aes(y = population, x = year)) +
        geom_point() +
        geom_smooth(col = "magenta", method = "lm", formula = y ~ poly(x, 3)) # Truncate at low and high end?
g9 <- dat %>% 
        ggplot(aes(y = rent_burden, x = year)) +
        geom_point() +
        geom_smooth(col = "magenta", method = "lm", formula = y ~ poly(x, 3))
g10 <- dat %>% 
        # ggplot(aes(y = pct_white, x = year)) +
        ggplot(aes(y = logit(pct_white), x = year)) +
        geom_point() +
        geom_smooth(col = "magenta", method = "lm", formula = y ~ poly(x, 3))
g11 <- dat %>% 
        # ggplot(aes(y = pct_af_am, x = year)) +
        ggplot(aes(y = logit(pct_af_am), x = year)) +
        geom_point() +
        geom_smooth(col = "magenta", method = "lm", formula = y ~ poly(x, 3))
g12 <- dat %>% 
        # ggplot(aes(y = pct_hispanic, x = year)) +
        ggplot(aes(y = logit(pct_hispanic), x = year)) +
        geom_point() +
        geom_smooth(col = "magenta", method = "lm", formula = y ~ poly(x, 3))
g13 <- dat %>% 
        # ggplot(aes(y = pct_am_ind, x = year)) +
        ggplot(aes(y = logit(pct_am_ind), x = year)) +
        geom_point() +
        geom_smooth(col = "magenta", method = "lm", formula = y ~ poly(x, 3))
g14 <- dat %>% 
        # ggplot(aes(y = pct_asian, x = year)) +
        ggplot(aes(y = logit(pct_asian), x = year)) +
        geom_point() +
        geom_smooth(col = "magenta", method = "lm", formula = y ~ poly(x, 3))
g15 <- dat %>% 
        # ggplot(aes(y = pct_nh_pi, x = year)) +
        ggplot(aes(y = logit(pct_nh_pi), x = year)) +
        geom_point() +
        geom_smooth(col = "magenta", method = "lm", formula = y ~ poly(x, 3))
g16 <- dat %>% 
        # ggplot(aes(y = pct_multiple, x = year)) +
        ggplot(aes(y = logit(pct_multiple), x = year)) +
        geom_point() +
        geom_smooth(col = "magenta", method = "lm", formula = y ~ poly(x, 3))
g17 <- dat %>% 
        # ggplot(aes(y = pct_other, x = year)) +
        ggplot(aes(y = logit(pct_other), x = year)) +
        geom_point() +
        geom_smooth(col = "magenta", method = "lm", formula = y ~ poly(x, 3))

grid.arrange(g1, g2, g3, ncol = 1)
grid.arrange(g4, g5, g6, ncol = 1)
grid.arrange(g7, g8, g9, ncol = 1)
grid.arrange(g10, g11, g12, ncol = 1)
grid.arrange(g13, g14, g15, ncol = 1)
grid.arrange(g16, g17, ncol = 1)

library(gridExtra)
# g18 <- dat %>% 
#         ggplot(aes(y = eviction_rate, x = year, color = year, group = year)) +
#         # ggplot(aes(y = log(eviction_rate + 0.1), x = year, color = year, group = year)) +
#         geom_point() +
#         geom_smooth(method = "lm", formula = y ~ poly(x, 3))
g19 <- dat %>% 
        # ggplot(aes(y = eviction_rate, x = poverty_rate, color = year, group = year)) +
        ggplot(aes(y = log(eviction_rate + 0.1), x = log(poverty_rate + 1), color = year, group = year)) +
        geom_point() +
        geom_smooth()
g20 <- dat %>% 
        # ggplot(aes(y = eviction_rate, x = pct_renter_occupied, color = year, group = year)) +
        ggplot(aes(y = log(eviction_rate + 0.1), x = pct_renter_occupied, color = year, group = year)) +
        geom_point() +
        geom_smooth()
g21 <- dat %>% 
        # ggplot(aes(y = eviction_rate, x = median_property_value, color = year, group = year)) +
        ggplot(aes(y = log(eviction_rate + 0.1), x = log(median_property_value + 100000), color = year, group = year)) +
        geom_point() +
        geom_smooth()
g22 <- dat %>% 
        # ggplot(aes(y = eviction_rate, x = eviction_filing_rate, color = year, group = year)) +
        ggplot(aes(y = log(eviction_rate + 0.1), x = log(eviction_filing_rate + 1), color = year, group = year)) +
        geom_point() +
        geom_smooth()
g23 <- dat %>% 
        # ggplot(aes(y = eviction_rate, x = median_gross_rent, color = year, group = year)) +
        ggplot(aes(y = log(eviction_rate + 0.1), x = median_gross_rent, color = year, group = year)) +
        geom_point() +
        geom_smooth()
g24 <- dat %>% 
        # ggplot(aes(y = eviction_rate, x = median_household_income, color = year, group = year)) +
        ggplot(aes(y = log(eviction_rate + 0.1), x = median_household_income, color = year, group = year)) +
        geom_point() +
        geom_smooth()
g25 <- dat %>% 
        # ggplot(aes(y = eviction_rate, x = population, color = year, group = year)) +
        ggplot(aes(y = log(eviction_rate + 0.1), x = population, color = year, group = year)) +
        geom_point() +
        geom_smooth()
g26 <- dat %>% 
        # ggplot(aes(y = eviction_rate, x = rent_burden, color = year, group = year)) +
        ggplot(aes(y = log(eviction_rate + 0.1), x = rent_burden, color = year, group = year)) +
        geom_point() +
        geom_smooth()
g27 <- dat %>% 
        # ggplot(aes(y = eviction_rate, x = pct_white, color = year, group = year)) +
        ggplot(aes(y = log(eviction_rate + 0.1), x = logit(pct_white), color = year, group = year)) +
        geom_point() +
        geom_smooth()
g28 <- dat %>% 
        # ggplot(aes(y = eviction_rate, x = pct_af_am, color = year, group = year)) +
        ggplot(aes(y = log(eviction_rate + 0.1), x = logit(pct_af_am), color = year, group = year)) +
        geom_point() +
        geom_smooth()
g29 <- dat %>% 
        # ggplot(aes(y = eviction_rate, x = pct_hispanic, color = year, group = year)) +
        ggplot(aes(y = log(eviction_rate + 0.1), x = logit(pct_hispanic), color = year, group = year)) +
        geom_point() +
        geom_smooth()
g30 <- dat %>% 
        # ggplot(aes(y = eviction_rate, x = pct_am_ind, color = year, group = year)) +
        ggplot(aes(y = log(eviction_rate + 0.1), x = logit(pct_am_ind), color = year, group = year)) +
        geom_point() +
        geom_smooth()
g31 <- dat %>% 
        # ggplot(aes(y = eviction_rate, x = pct_asian, color = year, group = year)) +
        ggplot(aes(y = log(eviction_rate + 0.1), x = logit(pct_asian), color = year, group = year)) +
        geom_point() +
        geom_smooth()
g32 <- dat %>% 
        # ggplot(aes(y = eviction_rate, x = pct_nh_pi, color = year, group = year)) +
        ggplot(aes(y = log(eviction_rate + 0.1), x = logit(pct_nh_pi), color = year, group = year)) +
        geom_point() +
        geom_smooth()
g33 <- dat %>% 
        # ggplot(aes(y = eviction_rate, x = pct_multiple, color = year, group = year)) +
        ggplot(aes(y = log(eviction_rate + 0.1), x = logit(pct_multiple), color = year, group = year)) +
        geom_point() +
        geom_smooth()
g34 <- dat %>% 
        # ggplot(aes(y = eviction_rate, x = pct_other, color = year, group = year)) +
        ggplot(aes(y = log(eviction_rate + 0.1), x = logit(pct_other), color = year, group = year)) +
        geom_point() +
        geom_smooth()

grid.arrange(     g19, g20, ncol = 1)
grid.arrange(g21, g22, g23, ncol = 1)
grid.arrange(g24, g25, g26, ncol = 1)
grid.arrange(g27, g28, g29, ncol = 1)
grid.arrange(g30, g31, g32, ncol = 1)
grid.arrange(g33, g34, ncol = 1)

plot(dat$evictions, dat$eviction_rate)
plot(dat$renter_occupied_households, dat$pct_renter_occupied)


# Feature Engineering -----------------------------------------------------

# Make decisions about each variable

dat <- dat %>% 
  mutate(log_eviction_rate         = log(eviction_rate + 0.1)) %>% 
  mutate(log_poverty_rate          = log(poverty_rate + 1)) %>% 
  mutate(log_median_property_value = log(median_property_value + 100000)) %>% 
  mutate(logit_pct_white           = logit(pct_white)) %>% 
  mutate(logit_pct_af_am           = logit(pct_af_am)) %>% 
  mutate(logit_pct_hispanic        = logit(pct_hispanic)) %>% 
  mutate(logit_pct_am_ind          = logit(pct_am_ind)) %>% 
  mutate(logit_pct_asian           = logit(pct_asian)) %>% 
  mutate(logit_pct_nh_pi           = logit(pct_nh_pi)) %>% 
  mutate(logit_pct_multiple        = logit(pct_multiple)) # %>% 
  # select(geoid, year, log_eviction_rate, pct_renter_occupied, median_gross_rent, median_household_income, population, rent_burden,
         # log_poverty_rate, log_median_property_value, logit_pct_white, logit_pct_af_am, logit_pct_hispanic,       
         # logit_pct_am_ind, logit_pct_asian, logit_pct_nh_pi, logit_pct_multiple)
  
  # pct_renter_occupied
  # median_gross_rent
  # median_household_income
  # population
  # rent_burden

x = dat$pct_af_am + dat$pct_am_ind + dat$pct_asian + dat$pct_hispanic + dat$pct_multiple + dat$pct_nh_pi + dat$pct_other + dat$pct_white
summary(x)
   # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   #    1       1       1       1       1       1       6 

summary(lm(log_eviction_rate ~ year, data = dat))

save(dat, file = "dat.rda")

# Visual check for collinearity -------------------------------------------

# dat %>% 
#   select(log_eviction_rate, pct_renter_occupied, median_gross_rent, median_household_income, population, rent_burden,
#          log_poverty_rate, log_median_property_value, logit_pct_white, logit_pct_af_am, logit_pct_hispanic,       
#          logit_pct_am_ind, logit_pct_asian, logit_pct_nh_pi, logit_pct_multiple) %>% 
#   as.data.frame %>% 
#   plot


# Numerical check for collinearity ----------------------------------------

library(car)
fit = lm(log_eviction_rate ~ pct_renter_occupied + median_gross_rent + median_household_income + population + rent_burden +
         log_poverty_rate + log_median_property_value + logit_pct_white + logit_pct_af_am + logit_pct_hispanic +
         logit_pct_am_ind + logit_pct_asian + logit_pct_nh_pi + logit_pct_multiple, data = dat)
vif(fit)
     # pct_renter_occupied         median_gross_rent   median_household_income                population               rent_burden 
     #             2.821040                  2.392349                  7.132568                  1.076676                  1.463649 
     #     log_poverty_rate log_median_property_value           logit_pct_white           logit_pct_af_am        logit_pct_hispanic 
     #             2.318971                  3.276115                  8.649650                  2.788200                  2.345783 
     #     logit_pct_am_ind           logit_pct_asian           logit_pct_nh_pi        logit_pct_multiple 
     #             1.187648                  4.009238                  1.378274                  1.140497 
                 
plot(dat$median_household_income, dat$logit_pct_white)

# All < 10 so not a huge problem

# Linear regression by year -----------------------------------------------
load("dat.rda")

fit_2011 = lm(log_eviction_rate ~ pct_renter_occupied + median_gross_rent + median_household_income + population + rent_burden +
         log_poverty_rate + log_median_property_value + logit_pct_white + logit_pct_af_am + logit_pct_hispanic +
         logit_pct_am_ind + logit_pct_asian + logit_pct_nh_pi + logit_pct_multiple, data = dat, subset = year == 2011)

fit_2012 = lm(log_eviction_rate ~ pct_renter_occupied + median_gross_rent + median_household_income + population + rent_burden +
                log_poverty_rate + log_median_property_value + logit_pct_white + logit_pct_af_am + logit_pct_hispanic +
                logit_pct_am_ind + logit_pct_asian + logit_pct_nh_pi + logit_pct_multiple, data = dat, subset = year == 2012)

fit_2013 = lm(log_eviction_rate ~ pct_renter_occupied + median_gross_rent + median_household_income + population + rent_burden +
                log_poverty_rate + log_median_property_value + logit_pct_white + logit_pct_af_am + logit_pct_hispanic +
                logit_pct_am_ind + logit_pct_asian + logit_pct_nh_pi + logit_pct_multiple, data = dat, subset = year == 2013)

fit_2014 = lm(log_eviction_rate ~ pct_renter_occupied + median_gross_rent + median_household_income + population + rent_burden +
                log_poverty_rate + log_median_property_value + logit_pct_white + logit_pct_af_am + logit_pct_hispanic +
                logit_pct_am_ind + logit_pct_asian + logit_pct_nh_pi + logit_pct_multiple, data = dat, subset = year == 2014)

fit_2015 = lm(log_eviction_rate ~ pct_renter_occupied + median_gross_rent + median_household_income + population + rent_burden +
                log_poverty_rate + log_median_property_value + logit_pct_white + logit_pct_af_am + logit_pct_hispanic +
                logit_pct_am_ind + logit_pct_asian + logit_pct_nh_pi + logit_pct_multiple, data = dat, subset = year == 2015)

fit_2016 = lm(log_eviction_rate ~ pct_renter_occupied + median_gross_rent + median_household_income + population + rent_burden +
                log_poverty_rate + log_median_property_value + logit_pct_white + logit_pct_af_am + logit_pct_hispanic +
                logit_pct_am_ind + logit_pct_asian + logit_pct_nh_pi + logit_pct_multiple, data = dat, subset = year == 2016)

# plot of the regression coefficients vs. year -----------------------------------------------

coef <- c(fit_2011$coefficients[-1], fit_2012$coefficients[-1], fit_2013$coefficients[-1],
          fit_2014$coefficients[-1], fit_2015$coefficients[-1], fit_2016$coefficients[-1])
coef <- round(coef, digits = 3)
names(coef)= c()

years<-c(rep(2011,14),rep(2012,14), rep(2013,14),rep(2014,14),rep(2015,14),rep(2016,14))

var_names <- rep(names(fit_2011$coefficients[-1]), 6)

coef_years<-cbind(coef, years, var_names)
coef_years<-as.data.frame(coef_years)
coef_years$coef <- as.numeric(as.character(coef_years$coef))

library(ggplot2) 

scaleFUN <- function(x) sprintf("%.3f", x)

plot_coef = ggplot(coef_years, aes(x = years, y = coef, group=var_names,shape = var_names, colour = var_names)) + 
  geom_line(linetype = "solid")+ geom_point(aes(shape = var_names),size = 2) + 
  labs(y="Regression Coefficients",x="Year",title = "Regression Coefficients by Year") +
  scale_shape_manual(values=seq(0,15))+
  scale_y_continuous(breaks=seq(-1.2, 0.2, .05), labels = scaleFUN)

# Test time trends
fit_all = lm(log_eviction_rate ~ pct_renter_occupied + median_gross_rent + median_household_income + population + rent_burden +
                log_poverty_rate + log_median_property_value + logit_pct_white + logit_pct_af_am + logit_pct_hispanic +
                logit_pct_am_ind + logit_pct_asian + logit_pct_nh_pi + logit_pct_multiple +
                year +
                year:(pct_renter_occupied + median_gross_rent + median_household_income + population + rent_burden +
                log_poverty_rate + log_median_property_value + logit_pct_white + logit_pct_af_am + logit_pct_hispanic +
                logit_pct_am_ind + logit_pct_asian + logit_pct_nh_pi + logit_pct_multiple),
                data = dat)
summary(fit_all)
# No interactions with year were significant

# Fit without time trend, using standardized variables
dat_std <- data.frame(scale(dat))
summary(apply(dat_std, 2, mean, na.rm=T))
summary(apply(dat_std, 2, sd, na.rm=T))
fit1 = lm(log_eviction_rate ~ pct_renter_occupied + median_gross_rent + median_household_income + population + rent_burden +
                log_poverty_rate + log_median_property_value + logit_pct_white + logit_pct_af_am + logit_pct_hispanic +
                logit_pct_am_ind + logit_pct_asian + logit_pct_nh_pi + logit_pct_multiple,
                data = dat_std)
summary(fit1)
sort(coef(fit1))

fit2 = lm(log_eviction_rate ~ pct_renter_occupied + median_gross_rent + median_household_income + population + rent_burden +
                log_poverty_rate + log_median_property_value + logit_pct_white + logit_pct_af_am + logit_pct_hispanic +
                logit_pct_am_ind + logit_pct_asian + logit_pct_nh_pi + logit_pct_multiple +
                year,
                data = dat_std)
summary(fit2)
sort(coef(fit2))

plot(fit2$fitted.values, fit2$residuals)
abline(h = 0, col = "red")
hist(fit2$residuals)



library(caret)
lmfit1 <-train(log_eviction_rate ~ pct_renter_occupied + median_gross_rent + median_household_income + population + rent_burden +
               log_poverty_rate + log_median_property_value + logit_pct_white + logit_pct_af_am + logit_pct_hispanic +
               logit_pct_am_ind + logit_pct_asian + logit_pct_nh_pi + logit_pct_multiple +
               year, data = dat, method = "lm", na.action = na.pass)
varImp(lmfit1)
#                           Overall
# year                      100.000
# log_median_property_value  87.733
# population                 49.302
# logit_pct_hispanic         45.893
# logit_pct_nh_pi            39.166
# logit_pct_white            33.431
# logit_pct_asian            30.100
# logit_pct_af_am            27.647
# pct_renter_occupied        27.075
# log_poverty_rate           22.699
# median_gross_rent          22.303
# logit_pct_am_ind            7.840
# median_household_income     6.829
# logit_pct_multiple          4.397
# rent_burden                 0.000

lmfit2 <- train(log_eviction_rate ~ pct_renter_occupied + median_gross_rent + median_household_income + population + rent_burden +
               log_poverty_rate + log_median_property_value + logit_pct_white + logit_pct_af_am + logit_pct_hispanic +
               logit_pct_am_ind + logit_pct_asian + logit_pct_nh_pi + logit_pct_multiple, data = dat, method = "lm", na.action = na.pass)

varImp(lmfit2)
# log_median_property_value 100.000
# median_gross_rent          53.624
# population                 52.672
# logit_pct_hispanic         47.178
# logit_pct_nh_pi            46.533
# pct_renter_occupied        42.896
# logit_pct_white            33.422
# logit_pct_af_am            28.786
# logit_pct_asian            25.093
# log_poverty_rate           22.214
# logit_pct_multiple         14.318
# median_household_income    10.578
# logit_pct_am_ind            8.647
# rent_burden                 0.000

plot(varImp(lmfit1))
plot(varImp(lmfit2))


# fit = lm(log_eviction_rate ~ pct_renter_occupied + median_gross_rent + median_household_income + population + rent_burden +
#                 log_poverty_rate + log_median_property_value + logit_pct_white + logit_pct_af_am + logit_pct_hispanic +
#                 logit_pct_am_ind + logit_pct_asian + logit_pct_nh_pi + logit_pct_multiple,
#                 data = dat)
# summary(fit)
# plot(fit$fitted.values, fit$residuals)


### NOTE: We used Tableau for the maps... Below was older code when we thought we'd use Shiny

#  Map --------------------------------------------------------------------
# See http://rpubs.com/kitadasmalley/howToTrigris

load("dat.rda")
library(tidyverse)
library(tigris)
options(tigris_use_cache = TRUE)
king_spatial <- tracts(state = "WA", county = "King")

dim(dat)
mapdat <- dat %>% 
  rename(GEOID = geoid) %>% 
  filter(year == 2011)
dim(mapdat)
mapdat <- geo_join(spatial_data = king_spatial, mapdat, by = "GEOID") 

# See http://zevross.com/blog/2015/10/14/manipulating-and-mapping-us-census-data-in-r-using-the-acs-tigris-and-leaflet-packages-3/#make-your-map-leaflet
library(leaflet)

mapdat$eviction_rate = exp(mapdat$log_eviction_rate) - 0.01

mapdat <- dat %>% 
  rename(GEOID = geoid) %>% 
  filter(year == 2011)
mapdat_2011 <- geo_join(mapdat, spatial_data = king_spatial,by = "GEOID", how = "left") 


mapdat <- dat %>% 
  rename(GEOID = geoid) %>% 
  filter(year == 2012)
mapdat_2012 <- geo_join(mapdat, spatial_data = king_spatial,by = "GEOID", how = "left") 

mapdat <- dat %>% 
  rename(GEOID = geoid) %>% 
  filter(year == 2013)
mapdat_2013 <- geo_join(mapdat, spatial_data = king_spatial,by = "GEOID", how = "left") 

mymap <- function(DAT, X, LABEL, YEAR = 2011:2016, UNITS = "") {
  DAT$x = X
  DAT   = subset(DAT, year %in% YEAR)

  popup <- paste0("GEOID: ", DAT$GEOID, "<br>", LABEL, DAT$x)
  pal <- colorNumeric(
    palette = "YlGnBu",
    domain = DAT$x
  )
  
  map3 <- leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = DAT, 
                fillColor = ~pal(x), 
                color = "#b2aeae", # you need to use hex colors
                fillOpacity = 0.7, 
                weight = 1, 
                smoothFactor = 0.2,
                popup = popup) %>%
    addLegend(pal = pal, 
              values = DAT$x, 
              position = "bottomright", 
              title = LABEL,
              labFormat = labelFormat(suffix = UNITS)) 
  return(map3)
}

# mapdat11 <- subset(mapdat, year == 2011)
# mapdat12 <- subset(mapdat, year == 2012)
# mapdat13 <- subset(mapdat, year == 2013)
# mapdat14 <- subset(mapdat, year == 2014)
# mapdat15 <- subset(mapdat, year == 2015)
# mapdat16 <- subset(mapdat, year == 2016)

mymap(mapdat, mapdat$eviction_rate, "Eviction Rate<br>(per 100K)")

# mymap(mapdat11, mapdat11$eviction_rate, "Eviction Rate<br>(per 100K)<br>2011")
# mymap(mapdat12, mapdat11$eviction_rate, "Eviction Rate<br>(per 100K)<br>2012")
# mymap(mapdat13, mapdat11$eviction_rate, "Eviction Rate<br>(per 100K)<br>2013")
# mymap(mapdat14, mapdat11$eviction_rate, "Eviction Rate<br>(per 100K)<br>2014")
# mymap(mapdat15, mapdat11$eviction_rate, "Eviction Rate<br>(per 100K)<br>2015")
# mymap(mapdat16, mapdat11$eviction_rate, "Eviction Rate<br>(per 100K)<br>2016")

mymap(mapdat, mapdat$median_gross_rent, "Median Gross Rent<br>(USD/month)")

library(shiny)    # for shiny apps
library(leaflet)  # renderLeaflet function
library(spData)   # loads the world dataset 



ui = fluidPage(
  #sliderInput(inputId = "year", "year 2011-2016", 2011, 2016, value = 2015),
  sidebarPanel(
    selectInput(inputId = "year",
                label = "Select year:",
                choices = c("2011", "2012", "2013", "2014", "2015", "2016"),
                selected = "2015",
                multiple = FALSE)
    
  ),
  
  # Output(s)
  mainPanel(
    leafletOutput(outputId = "map")
  )
)
  
UNITS = ""
server = function(input, output) {
  output$map = renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(#data = subset(mapdat, year %in% mapdat$year),
                  data = geo_join(subset(dat, year == input$year), spatial_data = king_spatial,
                                  by = "GEOID", how = "left"),
                  fillColor = ~pal(subset(dat, year == input$year)$median_gross_rent), 
                  color = "#b2aeae", # you need to use hex colors
                  fillOpacity = 0.7, 
                  weight = 1, 
                  smoothFactor = 0.2,
                  popup = paste0("GEOID: ", subset(dat, year == input$year)$GEOID, "<br>", "LABEL", subset(dat, input %in% all_years)$median_gross_rent)) %>%
      addLegend(pal = colorNumeric(
        palette = "YlGnBu",
        domain = subset(dat, year == input$year)$median_gross_rent
      ), 
                values = subset(dat, year == input$year)$median_gross_rent, 
                position = "bottomright", 
                title = "gross rent",
                labFormat = labelFormat(suffix = UNITS))
  })}



server = function(input, output) {
  output$map = renderLeaflet({
    mymap(geo_join(subset(dat, year ==input), spatial_data = king_spatial,by = "GEOID", how = "left"), subset(dat, year ==input)$eviction_rate, "Eviction Rate<br>(per 100K)")
    })}
mymap()

shinyApp(ui, server)