# Noam Ross' GAM course
# https://noamross.github.io/gams-in-r-course/

library(tidyverse)
library(MASS)
library(mgcv)


# Chapter 1 ----
mcycle <- MASS::mcycle

# Examine the mcycle data frame
head(mcycle)
plot(mcycle)

# Fit a linear model
lm_mod <- stats::lm(accel ~ times, data = mcycle)

# Visualize the model
stats::termplot(lm_mod, partial.resid = TRUE, se = TRUE)

# GAM
gam_mod <- mgcv::gam(accel ~ s(times), data = mcycle, method = "REML")

plot(gam_mod, residuals = TRUE, pch = 1)

stats::coef(gam_mod)

gam_mod_k3 <- mgcv::gam(accel ~ s(times, k = 3), data = mcycle)
gam_mod_k20 <- mgcv::gam(accel ~ s(times, k = 20), data = mcycle)

par(mfrow = c(1, 2))
plot(gam_mod_k3, residuals = TRUE, pch = 1)
plot(gam_mod_k20, residuals = TRUE, pch = 1)

gam_mod_s1 <- mgcv::gam(accel ~ s(times), data = mcycle, sp = 0.1)
gam_mod_s2 <- mgcv::gam(accel ~ s(times), data = mcycle, sp = 0.0001)

par(mfrow = c(2, 1))
plot(gam_mod_s1, residuals = TRUE, pch = 1)
plot(gam_mod_s2, residuals = TRUE, pch = 1)

gam_mod_sk <- mgcv::gam(accel ~ s(times, k = 50), data = mcycle, sp = 0.0001)
plot(gam_mod_sk, residuals = TRUE, pch = 1)

library(gamair)
data("mpg", package = "gamair")
head(mpg)
str(mpg)

mod_city <- mgcv::gam(city.mpg ~ s(weight) + s(length) + s(price), data = mpg, method = "REML")
plot(mod_city, pages = 1)

mod_city2 <-
  mgcv::gam(
    city.mpg ~ s(weight) + s(length) + s(price) + fuel + drive + style,
    data = mpg,
    method = "REML"
  )
plot(mod_city2, all.terms = TRUE, pages = 1)

mod_city3 <-
  mgcv::gam(
    city.mpg ~ s(weight, by = drive) + s(length, by = drive) + s(price, by = drive) + fuel + drive + style,
    data = mpg,
    method = "REML"
  )
plot(mod_city3, all.terms = TRUE, pages = 1)


# Chapter 2 ----
mod_city4 <- gam(city.mpg ~ s(weight) + s(length) + s(price) + s(rpm) + s(width),
                 data = mpg, method = "REML")

summary(mod_city4)

mod <- gam(accel ~ s(times), data = mcycle, method = "REML")
plot(mod, residuals = TRUE, pch = 1, cex = 2)

mod <- gam(hw.mpg ~ s(weight) + s(rpm) + s(price) + comp.ratio,
           data = mpg, method = "REML")
plot(mod, select = 3)
plot(mod, pages = 1, all.terms = TRUE, shade = TRUE)
plot(mod, select = 1, shade = TRUE, shade.col = "hotpink")

set.seed(0)
dat <- gamSim(1,n=600, scale = 0.6, verbose = FALSE)
mod <- gam(y ~ s(x0, k = 5) + s(x1, k = 5) + s(x2, k = 5) + s(x3, k = 5),
           data = dat, method = "REML")
mod <- gam(y ~ s(x0, k = 5) + s(x1, k = 5) + s(x2, k = 20) + s(x3, k = 3),
           data = dat, method = "REML")
gam.check(mod)

mod <- gam(hw.mpg ~ s(length) + s(width) + s(height) + s(weight),
           data = mpg, method = "REML")
mgcv::concurvity(mod, full = FALSE)


# Chapter 3 ----
data(meuse, package="sp")
head(meuse)
str(meuse)

mod2d <- mgcv::gam(cadmium ~ s(x, y), data = meuse, method = "REML")
summary(mod2d)
coef(mod2d)

mod2da <- mgcv::gam(cadmium ~ s(x, y) + s(elev) + s(dist), data = meuse, method = "REML")
summary(mod2da)

plot(mod2da, pages = 1)
plot(mod2da, pages = 1, scheme = 1)
plot(mod2da, pages = 1, scheme = 2)

par(mfrow = c(1, 1))
mgcv::vis.gam(mod2d, view = c("x", "y"), plot.type = "persp", se = 2, theta = 135)

mgcv::vis.gam(mod2d, view = c("x", "y"), plot.type = "contour")

mgcv::vis.gam(mod2d, view = c("x", "y"), plot.type = "contour", too.far = 0.05)
points(meuse)

mgcv::vis.gam(mod2d, view = c("x", "y"), plot.type = "contour", too.far = 0.25)
points(meuse)

mod_sep <- mgcv::gam(copper ~ s(dist, by = landuse) + landuse, data = meuse, method = "REML")
summary(mod_sep)

mod_fs <- gam(copper ~ s(dist, landuse, bs = "fs"),
              data = meuse, method = "REML")
summary(mod_fs)

plot(mod_sep, pages = 1)
plot(mod_fs, pages = 1)

vis.gam(mod_sep, view = c("dist", "landuse"), plot.type = "persp")
vis.gam(mod_fs, view = c("dist", "landuse"), plot.type = "persp")

tensor_mod <- mgcv::gam(cadmium ~ te(x, y, elev), data = meuse, method = "REML")
summary(tensor_mod)
plot(tensor_mod)

tensor_mod2 <- gam(cadmium ~ s(x, y) + s(elev) + ti(x, y, elev),
                   data = meuse, method = "REML")

summary(tensor_mod2)
plot(tensor_mod2, pages = 1)


# Chapter 4 ----