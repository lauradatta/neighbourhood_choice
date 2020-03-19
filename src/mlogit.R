library(mlogit)
library(AER)

data("Fishing", package = "mlogit")

#are four fishing modes (beach, pier, boat, charter), two alternative specific variables (price and catch) and one choice/individual specific variable (income)
head(Fishing)

Fish <- mlogit.data(Fishing, shape = "wide", varying = 2:9, choice = "mode")

#mode: the acutal choice
#index attributes
#chid: choice index
# alt: alternative

data("TravelMode", package = "AER")
#four transport modes (air, train, bus and car)and most of the variable are alternative specific (wait, vcost, travel, gcost). individual specififc: income, size

head(TravelMode)
