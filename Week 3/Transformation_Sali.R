
#NAME: JUDISMA SALI


library(fpp3)


food <- aus_retail |>
  filter(Industry == "Food retailing") |>
  summarise(Turnover = sum(Turnover))

food |> autoplot(Turnover^(1/3)) +
labs(y = "Cube root turnover")

food |> autoplot(log(Turnover)) +
labs(y = "Log turnover")

food |>
features(Turnover, features = guerrero)

food |> autoplot(box_cox(Turnover, 0.0895)) +
labs(y = "Box-Cox transformed turnover")


food |> autoplot(sqrt(Turnover)) +
labs(y = "Squareroot transformed turnover")

food |> autoplot(box_cox(Turnover, 0.5)) +
labs(y = "Box-Cox transformed turnover")

############################END######################
