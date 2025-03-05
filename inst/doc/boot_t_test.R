## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(boot.pval)

## ----message=FALSE------------------------------------------------------------
boot_t_test(extra ~ group, data = sleep)

## ----message=FALSE, eval = FALSE----------------------------------------------
#  sleep |> boot_t_test(extra ~ group)

## ----message=FALSE, eval = FALSE----------------------------------------------
#  sleep |> boot_t_test(extra ~ group, type = "perc") # Percentile interval
#  sleep |> boot_t_test(extra ~ group, type = "bca") # BCa interval

## ----message=FALSE, eval = FALSE----------------------------------------------
#  sleep |> boot_t_test(extra ~ group, R = 999, alternative = "less")

## ----message=FALSE, eval = FALSE----------------------------------------------
#  # Reshape to wide format:
#  sleep2 <- reshape(sleep, direction = "wide",
#                    idvar = "ID", timevar = "group")
#  
#  # Traditional interface:
#  boot_t_test(sleep2$extra.1, sleep2$extra.2, paired = TRUE)
#  
#  # Using pipes:
#  sleep2 |> boot_t_test(Pair(extra.1, extra.2) ~ 1)

## ----message=FALSE, eval = FALSE----------------------------------------------
#  # Traditional interface:
#  boot_t_test(sleep$extra, mu = 1)
#  
#  # Using pipes:
#  sleep |> boot_t_test(extra ~ 1, mu = 1)

## ----message=FALSE, eval = FALSE----------------------------------------------
#  boot_median_test(extra ~ group, data = sleep, type = "perc")
#  
#  sleep |> boot_median_test(extra ~ group, R = 999, alternative = "less")
#  
#  boot_median_test(sleep$extra, mu = 1)

