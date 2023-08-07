# NAME: JUDISMA A. SALI
# STT353 TIME SERIES ANALYSIS
# 08/07/2023

#____________________________CHAPTER 9_________________________________
install.packages("urca")

library(urca)  #Load Package
library(fpp3) #Load Package


#CHAPTER 9 ARIMA MODELS

 ##ARIMA models provide another approach to time series forecasting.
 ##Exponential smoothing and ARIMA models are the two most widely used approaches 
 ##to time series forecasting, and provide complementary approaches to the problem. 
 ##While exponential smoothing models are based on a description of the trend and
 ##seasonality in the data, ARIMA models aim to describe the autocorrelations in 
 ##the data.


# 9.1 Stationarity and Differencing

 ## In Figure 9.1, note that the Google stock price was non-stationary in panel 
 ##(a), but the daily changes were stationary in panel (b). This shows one way 
 ##to make a non-stationary time series stationary — compute the differences
 ##between consecutive observations. This is known as differencing.
 

google_2015 <- gafa_stock |>
  filter(Symbol == "GOOG", year(Date) == 2015)

google_2015 |> ACF(Close) |>
  autoplot() + labs(subtitle = "Google closing stock price")
google_2015 |> ACF(difference(Close)) |>
  autoplot() + labs(subtitle = "Changes in Google closing stock price")

# Figure 9.2: The ACF of the Google closing stock price in 2015 (left) and of
#the daily changes in Google closing stock price in 2015 (right).

google_2015 |>
  mutate(diff_close = difference(Close)) |>
  features(diff_close, ljung_box, lag = 10)

#> # A tibble: 1 × 3
#>   Symbol lb_stat lb_pvalue
#>   <chr>    <dbl>     <dbl>
#> 1 GOOG      7.91     0.637
#> 

# The ACF of the differenced Google stock price looks just like that of a white 
# noise series. Only one autocorrelation is outside of the 95% limits, and the
# Ljung-Box Q∗ statistic has a p-value of 0.637 (for h = 10). This suggests that
# the daily change in the Google stock price is essentially
# a random amount which is uncorrelated with that of previous 

 ## Seasonal differencing

# A seasonal difference is the difference between an observation and the previous
# observation from the same season. 

 # Figure 9.4 plots Australian corticosteroid drug sales ($AUD) (top panel). 
 # Here, the data are first transformed using logarithms (second panel), then
 # seasonal differences are calculated (third panel). The data still seem somewhat
 # non-stationary, and so a further lot of first differences are computed (bottom 
 # panel).

PBS |>
  filter(ATC2 == "H02") |>
  summarise(Cost = sum(Cost)/1e6) |>
  transmute(
    `Sales ($million)` = Cost,
    `Log sales` = log(Cost),
    `Annual change in log sales` = difference(log(Cost), 12),
    `Doubly differenced log sales` =
      difference(difference(log(Cost), 12), 1)
  ) |>
  pivot_longer(-Month, names_to="Type", values_to="Sales") |>
  mutate(
    Type = factor(Type, levels = c(
      "Sales ($million)",
      "Log sales",
      "Annual change in log sales",
      "Doubly differenced log sales"))
  ) |>
  ggplot(aes(x = Month, y = Sales)) +
  geom_line() +
  facet_grid(vars(Type), scales = "free_y") +
  labs(title = "Corticosteroid drug sales", y = NULL)

# Figure 9.4: Top panel: Corticosteroid drug sales ($AUD). Other panels show 
# the same data after transforming and differencing.


#sThere is a degree of subjectivity in selecting which differences to apply. 
#sThe seasonally differenced data in Figure 9.3 do not show substantially different
#sbehaviour from the seasonally differenced data in Figure 9.4. In the latter case,
#we could have decided to stop with the seasonally differenced data, and not done 
#an extra round of differencing. In the former case, we could have decided that
#the data were not sufficiently stationary and taken an extra round of differencing.
#Some formal tests for differencing are discussed below, but there are always some 
#choices to be made in the modelling process, and different analysts may make 
#different choices.

#Unit root tests

#One way to determine more objectively whether differencing is required is to 
#use a unit root test. These are statistical hypothesis tests of stationarity
#that are designed for determining whether differencing is required.

#For example, let us apply it to the Google stock price data.

google_2015 |>
  features(Close, unitroot_kpss)

#> # A tibble: 1 × 3
#>   Symbol kpss_stat kpss_pvalue
#>   <chr>      <dbl>       <dbl>
#> 1 GOOG        3.56        0.01


#The KPSS test p-value is reported as a number between 0.01 and 0.1. If the 
#actual p-value is less than 0.01, it is reported as 0.01; and if the actual 
#p-value is greater than 0.1, it is reported as 0.1. In this case, the p-value
#is shown as 0.01 (and therefore it may be smaller than that), indicating that 
#the null hypothesis is rejected. That is, the data are not stationary. We can 
#difference the data, and apply the test again.

google_2015 |>
  mutate(diff_close = difference(Close)) |>
  features(diff_close, unitroot_kpss)

#> # A tibble: 1 × 3
#>   Symbol kpss_stat kpss_pvalue
#>   <chr>      <dbl>       <dbl>
#> 1 GOOG      0.0989         0.1

#This time, the p-value is reported as 0.1 (and so it could be larger than that). 
#We can conclude that the differenced data appear stationary.

#This process of using a sequence of KPSS tests to determine the appropriate 
#number of first differences is carried out using the unitroot_ndiffs() feature.

google_2015 |>
  features(Close, unitroot_ndiffs)

#> # A tibble: 1 × 2
#>   Symbol ndiffs
#>   <chr>   <int>
#> 1 GOOG        1

#As we saw from the KPSS tests above, one difference is required to make the 
#google_2015 data stationary.

#A similar feature for determining whether seasonal differencing is required is 
# unitroot_nsdiffs(), which uses the measure of seasonal strength introduced in
#Section 4.3 to determine the appropriate number of seasonal differences required. 
#No seasonal differences are suggested if FS<0.6, otherwise one seasonal difference is suggested.
#We can apply unitroot_nsdiffs() to the monthly total Australian retail turnover.

aus_total_retail <- aus_retail |>
  summarise(Turnover = sum(Turnover))
aus_total_retail |>
  mutate(log_turnover = log(Turnover)) |>
  features(log_turnover, unitroot_nsdiffs)

#> # A tibble: 1 × 1
#>   nsdiffs
#>     <int>
#> 1       1

aus_total_retail |>
  mutate(log_turnover = difference(log(Turnover), 12)) |>
  features(log_turnover, unitroot_ndiffs)

#> # A tibble: 1 × 1
#>   ndiffs
#>    <int>
#> 1      1

#Because unitroot_nsdiffs() returns 1 (indicating one seasonal difference is
#required), we apply the unitroot_ndiffs() function to the seasonally differenced
#data. These functions suggest we should do both a seasonal difference and a first 
#difference.

#9.5 Non-seasonal ARIMA models

#If we combine differencing with autoregression and a moving average model,
#we obtain a non-seasonal ARIMA model. ARIMA is an acronym for AutoRegressive 
#Integrated Moving Average (in this context, “integration” is the reverse of
#differencing).

## Example: Egyptian exports\

#Figure 9.7 shows Egyptian exports as a percentage of GDP from 1960 to 2017.


global_economy |>
  filter(Code == "EGY") |>
  autoplot(Exports) +
  labs(y = "% of GDP", title = "Egyptian exports")
 ##Figure 9.7: Annual Egyptian exports as a percentage of GDP since 1960.

#The following R code selects a non-seasonal ARIMA model automatically.


fit <- global_economy |>
  filter(Code == "EGY") |>
  model(ARIMA(Exports))
report(fit)

#> Series: Exports 
#> Model: ARIMA(2,0,1) w/ mean 
#> 
#> Coefficients:
#>          ar1      ar2      ma1  constant
#>       1.6764  -0.8034  -0.6896    2.5623
#> s.e.  0.1111   0.0928   0.1492    0.1161
#> 
#> sigma^2 estimated as 8.046:  log likelihood=-141.6
#> AIC=293.1   AICc=294.3   BIC=303.4


#Forecasts from the ARIMA(2,0,1) model are shown in Figure 9.8. Notice how they have 
#picked up the cycles evident in the Egyptian economy over the last few decades.

fit |> forecast(h=10) |>
  autoplot(global_economy) +
  labs(y = "% of GDP", title = "Egyptian exports")

#Figure 9.8: Forecasts of Egyptian exports.

#Figures 9.9 and 9.10 shows the ACF and PACF plots for the Egyptian exports data
#shown in Figure 9.7. The partial autocorrelations have the same critical values of  
#±1.96/√T as for ordinary autocorrelations, and these are typically shown on the 
#plot as in Figure 9.10.

global_economy |>
  filter(Code == "EGY") |>
  ACF(Exports) |>
  autoplot()
#Figure 9.9: ACF of Egyptian exports.

global_economy |>
  filter(Code == "EGY") |>
  PACF(Exports) |>
  autoplot()
#Figure 9.10: PACF of Egyptian exports.

#In Figure 9.9, we see that there is a decaying sinusoidal pattern in the ACF, 
#nd in Figure 9.10 the PACF shows the last significant spike at lag 4. This is
#what you would expect from an ARIMA(4,0,0) model.


fit2 <- global_economy |>
  filter(Code == "EGY") |>
  model(ARIMA(Exports ~ pdq(4,0,0)))
report(fit2)
#> Series: Exports 
#> Model: ARIMA(4,0,0) w/ mean 
#> 
#> Coefficients:
#>          ar1      ar2     ar3      ar4  constant
#>       0.9861  -0.1715  0.1807  -0.3283    6.6922
#> s.e.  0.1247   0.1865  0.1865   0.1273    0.3562
#> 
#> sigma^2 estimated as 7.885:  log likelihood=-140.5
#> AIC=293.1   AICc=294.7   BIC=305.4


#This model is only slightly worse than the ARIMA(2,0,1) model identified by
#ARIMA() (with an AICc value of 294.70 compared to 294.29).

#9.7 ARIMA modelling in fable

 ##Portmanteau tests of residuals for ARIMA models
  ##With ARIMA models, more accurate portmanteau tests are obtained if the 
  ##degrees of freedom of the test statistic are adjusted to take account of the 
  ##number of parameters in the model.

#Example: Central African Republic exports

  ##We will apply this procedure to the exports of the Central African Republic 
  ##shown in Figure 9.13.

global_economy |>
  filter(Code == "CAF") |>
  autoplot(Exports) +
  labs(title="Central African Republic exports",
       y="% of GDP")

#Figure 9.13: Exports of the Central African Republic as a percentage of GDP.

# 1. The time plot shows some non-stationarity, with an overall decline. The 
 #improvement in 1994 was due to a new government which overthrew the military
 #junta and had some initial success, before unrest caused further economic decline.

# 2. There is no evidence of changing variance, so we will not do a Box-Cox transformation.

# 3. To address the non-stationarity, we will take a first difference of the data.
# The differenced data are shown in Figure 9.14.


global_economy |>
  filter(Code == "CAF") |>
  gg_tsdisplay(difference(Exports), plot_type='partial')

# Figure 9.14: Time plot and ACF and PACF plots for the differenced Central 
# African Republic Exports.

# These now appear to be stationary.

# 4. The PACF shown in Figure 9.14 is suggestive of an AR(2) model; so an initial 
# candidate model is an ARIMA(2,1,0). The ACF suggests an MA(3) model; so an 
# alternative candidate is an ARIMA(0,1,3).

# 5. We fit both an ARIMA(2,1,0) and an ARIMA(0,1,3) model along with two automated
# model selections, one using the default stepwise procedure, and one working 
# harder to search a larger model space.

caf_fit <- global_economy |>
  filter(Code == "CAF") |>
  model(arima210 = ARIMA(Exports ~ pdq(2,1,0)),
        arima013 = ARIMA(Exports ~ pdq(0,1,3)),
        stepwise = ARIMA(Exports),
        search = ARIMA(Exports, stepwise=FALSE))

caf_fit |> pivot_longer(!Country, names_to = "Model name",
                        values_to = "Orders")
#> # A mable: 4 x 3
#> # Key:     Country, Model name [4]
#>   Country                  `Model name`         Orders
#>   <fct>                    <chr>               <model>
#> 1 Central African Republic arima210     <ARIMA(2,1,0)>
#> 2 Central African Republic arima013     <ARIMA(0,1,3)>
#> 3 Central African Republic stepwise     <ARIMA(2,1,2)>
#> 4 Central African Republic search       <ARIMA(3,1,0)>

glance(caf_fit) |> arrange(AICc) |> select(.model:BIC)

#> # A tibble: 4 × 6
#>   .model   sigma2 log_lik   AIC  AICc   BIC
#>   <chr>     <dbl>   <dbl> <dbl> <dbl> <dbl>
#> 1 search     6.52   -133.  274.  275.  282.
#> 2 arima210   6.71   -134.  275.  275.  281.
#> 3 arima013   6.54   -133.  274.  275.  282.
#> 4 stepwise   6.42   -132.  274.  275.  284.

# The four models have almost identical AICc values. Of the models fitted, the full 
#search has found that an ARIMA(3,1,0) gives the lowest AICc value, closely 
#followed by the ARIMA(2,1,0) and ARIMA(0,1,3) — the latter two being the models 
#that we guessed from the ACF and PACF plots. The automated stepwise selection 
#has identified an ARIMA(2,1,2) model, which has the highest AICc value of the 
#four models.

# 6. The ACF plot of the residuals from the ARIMA(3,1,0) model shows that all 
# autocorrelations are within the threshold limits, indicating that the residuals
# are behaving like white noise.

caf_fit |>
  select(search) |>
  gg_tsresiduals()

#Figure 9.15: Residual plots for the ARIMA(3,1,0) model.

#A portmanteau test (setting K=3) returns a large p-value, also suggesting that
#the residuals are white noise.


augment(caf_fit) |>
  filter(.model=='search') |>
  features(.innov, ljung_box, lag = 10, dof = 3)
#> # A tibble: 1 × 4
#>   Country                  .model lb_stat lb_pvalue
#>   <fct>                    <chr>    <dbl>     <dbl>
#> 1 Central African Republic search    5.75     0.569

# 7. Forecasts from the chosen model are shown in Figure 9.16.

caf_fit |>
  forecast(h=5) |>
  filter(.model=='search') |>
  autoplot(global_economy)

#Figure 9.16: Forecasts for the Central African Republic Exports.

# 9.9 Seasonal ARIMA models

  ##ACF/PACF
   ##The seasonal part of an AR or MA model will be seen in the seasonal lags
   ##of the PACF and ACF.

## Example: Monthly US leisure and hospitality employment

 ## We will describe seasonal ARIMA modelling using monthly US employment data 
 ## for leisure and hospitality jobs from January 2001 to September 2019, shown 
 ## in Figure 9.18.


leisure <- us_employment |>
  filter(Title == "Leisure and Hospitality",
         year(Month) > 2000) |>
  mutate(Employed = Employed/1000) |>
  select(Month, Employed)
autoplot(leisure, Employed) +
  labs(title = "US employment: leisure and hospitality",
       y="Number of people (millions)")

#   #Figure 9.18: Monthly US leisure and hospitality employment, 2001-2019.

 # The data are clearly non-stationary, with strong seasonality and a nonlinear
 # trend, so we will first take a seasonal difference. The seasonally differenced data are shown in Figure 9.19.

leisure |>
  gg_tsdisplay(difference(Employed, 12),
               plot_type='partial', lag=36) +
  labs(title="Seasonally differenced", y="")

#  #Figure 9.19: Seasonally differenced Monthly US leisure and hospitality employment.


# These are also clearly non-stationary, so we take a further first difference 
# in Figure 9.20.


leisure |>
  gg_tsdisplay(difference(Employed, 12) |> difference(),
               plot_type='partial', lag=36) +
  labs(title = "Double differenced", y="")

#Figure 9.20: Double differenced Monthly US leisure and hospitality employment.

fit <- leisure |>
  model(
    arima012011 = ARIMA(Employed ~ pdq(0,1,2) + PDQ(0,1,1)),
    arima210011 = ARIMA(Employed ~ pdq(2,1,0) + PDQ(0,1,1)),
    auto = ARIMA(Employed, stepwise = FALSE, approx = FALSE)
  )
fit |> pivot_longer(everything(), names_to = "Model name",
                    values_to = "Orders")
#> # A mable: 3 x 2
#> # Key:     Model name [3]
#>   `Model name`                    Orders
#>   <chr>                          <model>
#> 1 arima012011  <ARIMA(0,1,2)(0,1,1)[12]>
#> 2 arima210011  <ARIMA(2,1,0)(0,1,1)[12]>
#> 3 auto         <ARIMA(2,1,0)(1,1,1)[12]>

glance(fit) |> arrange(AICc) |> select(.model:BIC)


#> # A tibble: 3 × 6
#>   .model       sigma2 log_lik   AIC  AICc   BIC
#>   <chr>         <dbl>   <dbl> <dbl> <dbl> <dbl>
#> 1 auto        0.00142    395. -780. -780. -763.
#> 2 arima210011 0.00145    392. -776. -776. -763.
#> 3 arima012011 0.00146    391. -775. -775. -761.



# The residuals for the best model are shown in Figure 9.21.


fit |> select(auto) |> gg_tsresiduals(lag=36)

  ##Figure 9.21: Residuals from the fitted ARIMA(2,1,0)(1,1,1) 12 model.

# One small but significant spike (at lag 11) out of 36 is still consistent with
# white noise. To be sure, we use a Ljung-Box test, being careful to set the 
# degrees of freedom to match the number of parameters in the model.

augment(fit) |>
  filter(.model == "auto") |>
  features(.innov, ljung_box, lag=24, dof=4)

#> # A tibble: 1 × 3
#>   .model lb_stat lb_pvalue
#>   <chr>    <dbl>     <dbl>
#> 1 auto      16.6     0.680

#The large p-value confims that the residuals are similar to white noise.

# Thus, we now have a seasonal ARIMA model that passes the required checks and
# is ready for forecasting. Forecasts from the model for the next three years 
# are shown in Figure 9.22. The forecasts have captured the seasonal pattern 
# very well, and the increasing trend extends the recent pattern. The trend in 
# the forecasts is induced by the double differencing.

forecast(fit, h=36) |>
  filter(.model=='auto') |>
  autoplot(leisure) +
  labs(title = "US employment: leisure and hospitality",
       y="Number of people (millions)")

# Figure 9.22: Forecasts of monthly US leisure and hospitality employment using 
# the ARIMA(2,1,0)(1,1,1)12 model. 80% and 95% prediction intervals are shown.

# Example: Corticosteroid drug sales in Australia

  ## For our second example, we will try to forecast monthly corticosteroid drug 
  ## sales in Australia. These are known as H02 drugs under the Anatomical 
  ## Therapeutic Chemical classification scheme.


h02 <- PBS |>
  filter(ATC2 == "H02") |>
  summarise(Cost = sum(Cost)/1e6)
h02 |>
  mutate(log(Cost)) |>
  pivot_longer(-Month) |>
  ggplot(aes(x = Month, y = value)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  labs(y="", title="Corticosteroid drug scripts (H02)")

# Figure 9.23: Corticosteroid drug sales in Australia (in millions of scripts 
# per month). Logged data shown in bottom panel.

 ## Data from July 1991 to June 2008 are plotted in Figure 9.23. There is a small 
 ## increase in the variance with the level, so we take logarithms to stabilise
 ## the variance.

# The data are strongly seasonal and obviously non-stationary, so seasonal 
# differencing will be used. The seasonally differenced data are shown in 
# Figure 9.24. It is not clear at this point whether we should do another 
# difference or not. We decide not to, but the choice is not obvious.

# The last few observations appear to be different (more variable) from the 
# earlier data. This may be due to the fact that data are sometimes revised 
# when earlier sales are reported late.


h02 |> gg_tsdisplay(difference(log(Cost), 12),
                    plot_type='partial', lag_max = 24)

# Figure 9.24: Seasonally differenced corticosteroid drug sales in Australia 
# (in millions of scripts per month).

# In the plots of the seasonally differenced data, there are spikes in the PACF 
# at lags 12 and 24, but nothing at seasonal lags in the ACF. This may be 
# suggestive of a seasonal AR(2) term. In the non-seasonal lags, there are three 
# significant spikes in the PACF, suggesting a possible AR(3) term. The pattern
# in the ACF is not indicative of any simple model.

# Consequently, this initial analysis suggests that a possible model for these 
# data is an ARIMA(3,0,0)(2,1,0)12. We fit this model, along with some variations
# on it, and compute the AICc 
# values shown in Table 9.2.

# Of these models, the best is the ARIMA(3,0,1)(0,1,2) 12 model (i.e., it has the
# smallest AICc value). The innovation residuals from this model are shown in
# Figure 9.25.

fit <- h02 |>
  model(ARIMA(log(Cost) ~ 0 + pdq(3,0,1) + PDQ(0,1,2)))
fit |> gg_tsresiduals(lag_max=36)

# Figure 9.25: Innovation residuals from the ARIMA(3,0,1)(0,1,2) 12model 
# applied to the H02 monthly script sales data.


augment(fit) |>
  features(.innov, ljung_box, lag = 36, dof = 6)

#> # A tibble: 1 × 3
#>   .model                                             lb_stat lb_pvalue
#>   <chr>                                                <dbl>     <dbl>
#> 1 ARIMA(log(Cost) ~ 0 + pdq(3, 0, 1) + PDQ(0, 1, 2))    50.7    0.0104

# There are a few significant spikes in the ACF, and the model fails the
# Ljung-Box test. The model can still be used for forecasting, but the prediction
# intervals may not be accurate due to the correlated residuals.

#  Test set evaluation:

# We will compare some of the models fitted so far using a test set consisting 
# of the last two years of data.

# Forecasts from the ARIMA(3,0,1)(0,1,2) 12 model (which has the second lowest 
# RMSE value on the test set, and the best AICc value amongst models with only 
# seasonal differencing) are shown in Figure 9.26.


h02 |>
  model(ARIMA(log(Cost) ~ 0 + pdq(3,0,1) + PDQ(0,1,2))) |>
  forecast() |>
  autoplot(h02) +
  labs(y=" $AU (millions)",
       title="Corticosteroid drug scripts (H02) sales")
 
  # Figure 9.26: Forecasts from the ARIMA(3,0,1)(0,1,2) 12 model applied to the 
  # H02 monthly script sales data


# 9.10 ARIMA vs ETS

 ## Comparing ARIMA() and ETS() on non-seasonal data

   ## We can use time series cross-validation to compare ARIMA and ETS models. 
   ## Let’s consider the Australian population from the global_economy dataset,
   ## as introduced in Section 8.2.

aus_economy <- global_economy |>
  filter(Code == "AUS") |>
  mutate(Population = Population/1e6)

aus_economy |>
  slice(-n()) |>
  stretch_tsibble(.init = 10) |>
  model(
    ETS(Population),
    ARIMA(Population)
  ) |>
  forecast(h = 1) |>
  accuracy(aus_economy) |>
  select(.model, RMSE:MAPE)

#> # A tibble: 2 × 5
#>   .model              RMSE    MAE   MPE  MAPE
#>   <chr>              <dbl>  <dbl> <dbl> <dbl>
#> 1 ARIMA(Population) 0.194  0.0789 0.277 0.509
#> 2 ETS(Population)   0.0774 0.0543 0.112 0.327

  # In this case the ETS model has higher accuracy on the cross-validated 
  # performance measures. Below we generate and plot forecasts for the next
  # 5 years generated from an ETS model.

aus_economy |>
  model(ETS(Population)) |>
  forecast(h = "5 years") |>
  autoplot(aus_economy |> filter(Year >= 2000)) +
  labs(title = "Australian population",
       y = "People (millions)")

 #Figure 9.28: Forecasts from an ETS model fitted to the Australian population.

# Comparing ARIMA() and ETS() on seasonal data

## In this case we want to compare seasonal ARIMA and ETS models applied to 
## the quarterly cement production data (from aus_production). Because the series
## is relatively long, we can afford to use a training and a test set rather than
## time series cross-validation. The advantage is that this is much faster.
## We create a training set from the beginning of 1988 to the end of 2007 and 
## select an ARIMA and an ETS model using the ARIMA() and ETS() functions.

cement <- aus_production |>
  select(Cement) |>
  filter_index("1988 Q1" ~ .)
train <- cement |> filter_index(. ~ "2007 Q4")

 # The output below shows the model selected and estimated by ARIMA(). 
 # The ARIMA model does well in capturing all the dynamics in the data as the
 # residuals seem to be white noise.


fit_arima <- train |> model(ARIMA(Cement))
report(fit_arima)

#> Series: Cement 
#> Model: ARIMA(1,0,1)(2,1,1)[4] w/ drift 
#> 
#> Coefficients:
#>          ar1      ma1   sar1     sar2     sma1  constant
#>       0.8886  -0.2366  0.081  -0.2345  -0.8979     5.388
#> s.e.  0.0842   0.1334  0.157   0.1392   0.1780     1.484
#> 
#> sigma^2 estimated as 11456:  log likelihood=-463.5
#> AIC=941   AICc=942.7   BIC=957.4


fit_arima |> gg_tsresiduals(lag_max = 16)

# Figure 9.29: Residual diagnostic plots for the ARIMA model fitted to the 
# quarterly cement production training data.


augment(fit_arima) |>
  features(.innov, ljung_box, lag = 16, dof = 5)

#> # A tibble: 1 × 3
#>   .model        lb_stat lb_pvalue
#>   <chr>           <dbl>     <dbl>
#> 1 ARIMA(Cement)    6.37     0.847\

# The output below also shows the ETS model selected and estimated by ETS().
# This model also does well in capturing all the dynamics in the data, as the 
# residuals similarly appear to be white noise.


fit_ets <- train |> model(ETS(Cement))
report(fit_ets)

#> Series: Cement 
#> Model: ETS(M,N,M) 
#>   Smoothing parameters:
#>     alpha = 0.7534 
#>     gamma = 1e-04 
#> 
#>   Initial states:
#>  l[0]  s[0] s[-1] s[-2]  s[-3]
#>  1695 1.031 1.045 1.011 0.9122
#> 
#>   sigma^2:  0.0034
#> 
#>  AIC AICc  BIC 
#> 1104 1106 1121

fit_ets |>
  gg_tsresiduals(lag_max = 16)

# Figure 9.30: Residual diagnostic plots for the ETS model fitted to the 
# quarterly cement production training data.

augment(fit_ets) |>
  features(.innov, ljung_box, lag = 16)

#> # A tibble: 1 × 3
#>   .model      lb_stat lb_pvalue
#>   <chr>         <dbl>     <dbl>
#> 1 ETS(Cement)    10.0     0.865


  ## The output below evaluates the forecasting performance of the two competing
  ## models over the test set. In this case the ARIMA model seems to be the 
  ## slightly more accurate model based on the test set RMSE, MAPE and MASE.

# Generate forecasts and compare accuracy over the test set

bind_rows(
  fit_arima |> accuracy(),
  fit_ets |> accuracy(),
  fit_arima |> forecast(h = 10) |> accuracy(cement),
  fit_ets |> forecast(h = 10) |> accuracy(cement)
) |>
  select(-ME, -MPE, -ACF1)

#> # A tibble: 4 × 7
#>   .model        .type     RMSE   MAE  MAPE  MASE RMSSE
#>   <chr>         <chr>    <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 ARIMA(Cement) Training  100.  79.9  4.37 0.546 0.582
#> 2 ETS(Cement)   Training  103.  80.0  4.41 0.547 0.596
#> 3 ARIMA(Cement) Test      216. 186.   8.68 1.27  1.26 
#> 4 ETS(Cement)   Test      222. 191.   8.85 1.30  1.29


# Below we generate and plot forecasts from the ARIMA model for the next 3 years.


cement |>
  model(ARIMA(Cement)) |>
  forecast(h="3 years") |>
  autoplot(cement) +
  labs(title = "Cement production in Australia",
       y = "Tonnes ('000)")

# Figure 9.31: Forecasts from an ARIMA model fitted to all of the available
# quarterly cement production data since 1988.

# As already noted, comparing information criteria is only valid for ARIMA 
# models of the same orders of differencing.↩︎
#____________________________________END_________________________________

