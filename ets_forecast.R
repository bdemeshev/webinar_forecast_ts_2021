url = "https://github.com/bdemeshev/webinar_forecast_ts_2021/raw/main/monthly_marriages_2020.csv"

library(fable)
library(feasts)
library(rio)
library(tidyverse)
library(tsibble)
library(lubridate)

marr = import(url)
glimpse(marr)

unique(marr$region)

rf_marr = filter(marr, region == "643 Российская Федерация")

rf_marr2 = rf_marr %>% select(-region)

rf_marr3 = rf_marr2 %>% mutate(date = yearmonth(date))

data = as_tsibble(rf_marr3, index = date)

autoplot(data, marriages)

gg_season(data, marriages)

gg_tsdisplay(data, marriages)

train = filter(data, date < ymd("2019-01-01"))
tail(train)

mod_table = model(train,
                  simple = SNAIVE(marriages),
                  aaa = ETS(marriages ~ error('A') + trend('A') + season('A')),
                  ln_aaa = ETS(marriages ~ error('A') + trend('A') + season('A')))
fcst = forecast(mod_table, h = "2 year")
fcst

mod_table %>% select(ln_aaa) %>% report()

accuracy(fcst, data)

autoplot(fcst)
autoplot(fcst, data)
