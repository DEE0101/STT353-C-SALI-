# NAME: JUDISMA A. SALI
# STT353 TIME SERIES ANALYSIS
# 07/31/2023

#____________________________CHAPTER 8_________________________________

library(fpp3) #Load Package

# 8.1 The Simple Exponential Smoothing (SES)

 ## is the simplest of the exponentially smoothing methods, This method is suitable
 ## for forecasting data with no clear trend or seasonal pattern. For example,

algeria_economy <- global_economy |>
  filter(Country == "Algeria")
algeria_economy |>
  autoplot(Exports) +
  labs(y = "% of GDP", title = "Exports of goods and services from Algeria from 
                                        1960 to 2017.")

# the data in Figure 8.1 above shows the Exports of goods and services from Algeria from 
# 1960 to 2017. And the figure 8.1 do not display any clear trending behaviour or any 
# seasonality. (There is a decline in the last few years, which might suggest a
# trend. We will consider whether a trended method would be better for this series 
# later in this chapter.) We have already considered the naïve and the average as
# possible methods for forecasting such data.

# Simple exponential smoothing is applied to forecast exports of goods and 
# services from Algeria.

 ## Estimate parameters

  ##Figure 8.2: Simple exponential smoothing applied to exports from Algeria 
  ##(1960–2017). The orange curve shows the one-step-ahead fitted values.

fit <- algeria_economy |>
  model(ETS(Exports ~ error("A") + trend("N") + season("N")))
fc <- fit |>
  forecast(h = 5)

fc |>
  autoplot(algeria_economy) +
  geom_line(aes(y = .fitted), col="#D55E00",
            data = augment(fit)) +
  labs(y="% of GDP", title=" Exports: Algeria") +
  guides(colour = "none")

 ## The figure 8.2 above shows the Simple exponential smoothing applied to exports 
 ## from Algeria (1960–2017). The orange curve shows the one-step-ahead fitted values.

# The forecasts for the period 2018–2022 are plotted . Also plotted 
# are one-step-ahead fitted values alongside the data over the period 1960–2017.
# The large value of  α in this example is reflected in the large adjustment that 
# takes place in the estimated level  ℓtat each time. A smaller value of  α would
# lead to smaller changes over time, and so the series of fitted values would be 
# smoother.The prediction intervals shown here are calculated using the methods
# described in Section 8.7. The prediction intervals show that there is considerable
# uncertainty in the future exports over the five-year forecast period. So 
# interpreting the point forecasts without accounting for the large uncertainty 
# can be very misleading.

# 8.2 Holt’s linear trend method

 ## Holt (1957) extended simple exponential smoothing to allow the forecasting 
 ## of data with a trend. This method involves a forecast equation and two 
 ## smoothing equations 

## Example: Australian population

aus_economy <- global_economy |>
  filter(Code == "AUS") |>
  mutate(Pop = Population / 1e6)
autoplot(aus_economy, Pop) +
  labs(y = "Millions", title = "Australian population , 1960-2017.")

fit <- aus_economy |>
  model(
    AAN = ETS(Pop ~ error("A") + trend("A") + season("N"))
  )
fc <- fit |> forecast(h = 10)

## Figure 8.3 above shows Australia’s annual population from 1960 to 2017. We will apply Holt’s 
## method to this series. The smoothing parameters,α and β∗ , and the initial values
## ℓ0 and b 0 are estimated by minimising the SSE for the one-step training errors 
## as in Section 8.1.


## Example: Australian Population (continued)
  ## The Figure shows the forecasts for years 2018–2032 generated from Holt’s 
  ## linear trend method and the damped trend method.

aus_economy |>
  model(
    `Holt's method` = ETS(Pop ~ error("A") +
                            trend("A") + season("N")),
    `Damped Holt's method` = ETS(Pop ~ error("A") +
                                   trend("Ad", phi = 0.9) + season("N"))
  ) |>
  forecast(h = 15) |>
  autoplot(aus_economy, level = NULL) +
  labs(title = "Australian population",
       y = "Millions") +
  guides(colour = guide_legend(title = "Forecasting annual Australian population 
  (millions) over 2018-2032."))

 ##  For the damped trend method, ϕ= 0.90We have set the damping parameter to a
 ## relatively low number (ϕ =0.90) to exaggerate the effect of damping for comparison.
 ## Usually, we would estimate ϕ along with the other parameters. We have also 
 ## used a rather large forecasthorizon (h=15) to highlight the difference between 
 ## a damped trend and a linear trend.

# Example: Internet usage

 ## In this example, we compare the forecasting performance of the three exponential
 ## smoothing methods that we have considered so far in forecasting the number of users
 ## connected to the internet via a server. The data is observed over 100 minutes and
 ## is shown in Figure 8.5.

www_usage <- as_tsibble(WWWusage)
www_usage |> autoplot(value) +
  labs(x="Minute", y="Number of users",
       title = "Internet usage per minute
Figure 8.5: Users connected to the internet through a server

")

## We will use time series cross-validation to compare the one-step forecast
## accuracy of the three methods.

www_usage |>
  stretch_tsibble(.init = 10) |>
  model(
    SES = ETS(value ~ error("A") + trend("N") + season("N")),
    Holt = ETS(value ~ error("A") + trend("A") + season("N")),
    Damped = ETS(value ~ error("A") + trend("Ad") +
                   season("N"))
  ) |>
  forecast(h = 1) |>
  accuracy(www_usage)

#> # A tibble: 3 × 10
#>   .model .type     ME  RMSE   MAE   MPE  MAPE  MASE RMSSE  ACF1
#>   <chr>  <chr>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Damped Test  0.288   3.69  3.00 0.347  2.26 0.663 0.636 0.336
#> 2 Holt   Test  0.0610  3.87  3.17 0.244  2.38 0.701 0.668 0.296
#> 3 SES    Test  1.46    6.05  4.81 0.904  3.55 1.06  1.04  0.803


## Damped Holt’s method is best whether you compare MAE or RMSE values. So we will
## proceed with using the damped Holt’s method and apply it to the whole data set 
## to get forecasts for future minutes.

fit <- www_usage |>
  model(
    Damped = ETS(value ~ error("A") + trend("Ad") +
                   season("N"))
  )

# Estimated parameters:

tidy(fit)

#> # A tibble: 5 × 3
#>   .model term  estimate
#>   <chr>  <chr>    <dbl>
#> 1 Damped alpha   1.00  
#> 2 Damped beta    0.997 
#> 3 Damped phi     0.815 
#> 4 Damped l[0]   90.4   
#> 5 Damped b[0]   -0.0173

## The smoothing parameter for the slope is estimated to be almost one,
## indicating that the trend changes to mostly reflect the slope between the last
## two minutes of internet usage. The value of  α is very close to one, showing 
## that the level reacts strongly to each new observation.


fit |>
  forecast(h = 10) |>
  autoplot(www_usage) +
  labs(x="Minute", y="Number of users",
       title = "Internet usage per minute 
       Figure 8.6: Forecasting internet usage: 
       comparing forecasting performance of non-seasonal methods.")

## The resulting forecasts look sensible with decreasing trend, which flattens out
## due to the low value of the damping parameter (0.815), and relatively wide 
## prediction intervals reflecting the variation in the historical data. The
## prediction intervals are calculated using the methods described in Section 8.7.

## In this example, the process of selecting a method was relatively easy as both
## MSE and MAE comparisons suggested the same method (damped Holt’s). However,
## sometimes different accuracy measures will suggest different forecasting methods,
## and then a decision is required as to which forecasting method we prefer to use.
## As forecasting tasks can vary by many dimensions (length of forecast horizon, 
## size of test set, forecast error measures, frequency of data, etc.), it is unlikely
## that one method will be better than all others for all forecasting scenarios. 
## What we require from a forecasting method are consistently sensible forecasts,
## and these should be frequently evaluated against the task at hand.

# 8.3 Methods with seasonality

 ## Example: Domestic overnight trips in Australia
  ## We apply Holt-Winters’ method with both additive and multiplicative
  ## seasonality to forecast quarterly visitor nights in Australia spent by
  ## domestic tourists. Figure 8.7 shows the data from 1998–2017, and the
  ## forecasts for 2018–2020. The data show an obvious seasonal pattern, with 
  ## peaks observed in the Marchquarter of each year, corresponding to the
  ##Australian summer.

aus_holidays <- tourism |>
  filter(Purpose == "Holiday") |>
  summarise(Trips = sum(Trips)/1e3)
fit <- aus_holidays |>
  model(
    additive = ETS(Trips ~ error("A") + trend("A") +
                     season("A")),
    multiplicative = ETS(Trips ~ error("M") + trend("A") +
                           season("M"))
  )
fc <- fit |> forecast(h = "3 years")
fc |>
  autoplot(aus_holidays, level = NULL) +
  labs(title="Australian domestic tourism",
       y="Overnight trips (millions)") +
  guides(colour = guide_legend(title = "Forecast"))

## Figure 8.7: Forecasting domestic overnight trips in Australia using the 
## Holt-Winters method with both additive and multiplicative seasonality.

## Example: Holt-Winters method with daily data

 ## The Holt-Winters method can also be used for daily type of data, where
 ## the seasonal period is m= 7, and the appropriate unit of time for  h
 ## is in days. Here we forecast pedestrian traffic at a busy Melbourne train 
 ## station in July 2016.

sth_cross_ped <- pedestrian |>
  filter(Date >= "2016-07-01",
         Sensor == "Southern Cross Station") |>
  index_by(Date) |>
  summarise(Count = sum(Count)/1000)
sth_cross_ped |>
  filter(Date <= "2016-07-31") |>
  model(
    hw = ETS(Count ~ error("M") + trend("Ad") + season("M"))
  ) |>
  forecast(h = "2 weeks") |>
  autoplot(sth_cross_ped |> filter(Date <= "2016-08-14")) +
  labs(title = "Daily traffic: Southern Cross",
       y="Pedestrians ('000)")
## Figure 8.9: Forecasts of daily pedestrian traffic at the Southern Cross
## railway station, Melbourne.

  ## Clearly the model has identified the weekly seasonal pattern and the 
  ## increasing trend at the end of the data, and the forecasts are a close 
  ## match to the test data.

# 8.6 Estimation and model selection

 ## Example: Domestic holiday tourist visitor nights in Australia
  ## We now employ the ETS statistical framework to forecast Australian holiday
  ## tourism over the period 2016–2019. We let the ETS() function select the model 
  ## y minimising the AICc.

aus_holidays <- tourism |>
  filter(Purpose == "Holiday") |>
  summarise(Trips = sum(Trips)/1e3)
fit <- aus_holidays |>
  model(ETS(Trips))
report(fit)

#> Series: Trips 
#> Model: ETS(M,N,A) 
#>   Smoothing parameters:
#>     alpha = 0.3484 
#>     gamma = 1e-04 
#> 
#>   Initial states:
#>   l[0]    s[0]   s[-1]   s[-2] s[-3]
#>  9.727 -0.5376 -0.6884 -0.2934 1.519
#> 
#>   sigma^2:  0.0022
#> 
#>   AIC  AICc   BIC 
#> 226.2 227.8 242.9

## Figure 8.10 shows the states over time, while Figure 8.12 shows point 
## forecasts and prediction intervals generated from the model. The small values
## of  γ indicate that the seasonal states change very little over time.

components(fit) |>
  autoplot() +
  labs(title = "ETS(M,N,A) components")
 ## Figure 8.10: Graphical representation of the estimated states over time.

# 8.7 Forecasting with ETS models

 ## To obtain forecasts from an ETS model, we use the forecast() function from 
 ## the fable package. This function will always return the means of the forecast 
 ## distribution, even when they differ from these traditional point forecasts.

fit |>
  forecast(h = 8) |>
  autoplot(aus_holidays)+
  labs(title="Australian domestic tourism",
       y="Overnight trips (millions)")

#Figure 8.12: Forecasting Australian domestic overnight trips using an ETS(M,N,A) 
# model.
 
#____________________________________END_________________________________

