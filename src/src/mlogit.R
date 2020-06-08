library(mlogit)
library(AER)

data("Fishing", package = "mlogit")

#are four fishing modes (beach, pier, boat, charter), two alternative specific variables (price and catch) and one choice/individual specific variable (income)
head(Fishing)

Fish <- mlogit.data(Fishing, shape = "wide", varying = 2:9, choice = "mode")

head(Fish)

#mode: the acutal choice
#index attributes
#chid: choice index
# alt: alternative

data("TravelMode", package = "AER")
#four transport modes (air, train, bus and car)and most of the variable are alternative specific (wait, vcost, travel, gcost). individual specififc: income, size

head(TravelMode)
TM <- mlogit.data(TravelMode, choice = "choice", shape = "long", alt.var = "mode")

f1 <- mFormula(choice ~ vcost | income + size | travel)

summary(mlogit(f1, TM))


library("zoo")
Fish <- mlogit.data(Fishing, varying = c(2:9), shape = "wide", choice = "mode")
m <- mlogit(mode ~ price | income | catch, data = Fish)
# compute a data.frame containing the mean value of the covariates in
# the sample
z <- with(Fish, data.frame(price = tapply(price, index(m)$alt, mean),
                           catch = tapply(catch, index(m)$alt, mean),
                           income = mean(income)))
# compute the marginal effects (the second one is an elasticity
## IGNORE_RDIFF_BEGIN
effects(m, covariate = "income", data = z)
## IGNORE_RDIFF_END
#' effects(m, covariate = "price", type = "rr", data = z)
effects(m, covariate = "catch", type = "ar", data = z)
