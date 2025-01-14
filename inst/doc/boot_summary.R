## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(boot.pval)

## ----message=FALSE------------------------------------------------------------
# Bootstrap summary of a linear model for mtcars:
model <- lm(mpg ~ hp + vs, data = mtcars)
boot_summary(model)

# Use 9999 bootstrap replicates and adjust p-values for
# multiplicity using Holm's method:
boot_summary(model, R = 9999, adjust.method = "holm")

# Export results to a gt table:
boot_summary(model, R = 9999) |>
  summary_to_gt()

## ----eval = FALSE-------------------------------------------------------------
#  # Export results to a Word document:
#  library(flextable)
#  boot_summary(model, R = 9999) |>
#    summary_to_flextable() |>
#    save_as_docx(path = "my_table.docx")

## ----eval = FALSE-------------------------------------------------------------
#  library(lme4)
#  model <- glmer(TICKS ~ YEAR + (1|LOCATION),
#             data = grouseticks, family = poisson)
#  boot_summary(model, R = 99)

## ----eval = FALSE-------------------------------------------------------------
#  model <- glmer(TICKS ~ YEAR + (1|LOCATION),
#             data = grouseticks, family = poisson)
#  boot_summary(model, R = 999, parallel = "multicore", ncpus = 10)

## ----message = FALSE----------------------------------------------------------
library(survival)
# Weibull AFT model:
model <- survreg(formula = Surv(time, status) ~ age + sex, data = lung,
                dist = "weibull", model = TRUE)
# Table with exponentiated coefficients:
censboot_summary(model)

# Cox PH model:
model <- coxph(formula = Surv(time, status) ~ age + sex, data = lung,
               model = TRUE)
# Table with hazard ratios:
censboot_summary(model)
# Table with original coefficients:
censboot_summary(model, coef = "raw")

## ----message = FALSE----------------------------------------------------------
# Hypothesis test for the city data
# H0: ratio = 1.4
library(boot)
ratio <- function(d, w) sum(d$x * w)/sum(d$u * w)
city.boot <- boot(city, ratio, R = 999, stype = "w", sim = "ordinary")
boot.pval(city.boot, theta_null = 1.4)

# Studentized test for the two sample difference of means problem
# using the final two series of the gravity data.
diff.means <- function(d, f)
{
  n <- nrow(d)
  gp1 <- 1:table(as.numeric(d$series))[1]
  m1 <- sum(d[gp1,1] * f[gp1])/sum(f[gp1])
  m2 <- sum(d[-gp1,1] * f[-gp1])/sum(f[-gp1])
  ss1 <- sum(d[gp1,1]^2 * f[gp1]) - (m1 *  m1 * sum(f[gp1]))
  ss2 <- sum(d[-gp1,1]^2 * f[-gp1]) - (m2 *  m2 * sum(f[-gp1]))
  c(m1 - m2, (ss1 + ss2)/(sum(f) - 2))
}
grav1 <- gravity[as.numeric(gravity[,2]) >= 7, ]
grav1.boot <- boot(grav1, diff.means, R = 999, stype = "f",
                   strata = grav1[ ,2])
boot.pval(grav1.boot, type = "stud", theta_null = 0)

