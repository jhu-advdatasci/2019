## Some time series

library(tidyverse)
library(lubridate)
library(splines)
library(tsModel)
library(broom)
suppressPackageStartupMessages(library(quantmod))
options("getSymbols.warning4.0" = FALSE, 
        "getSymbols.yahoo.warning" = FALSE)

today <- Sys.Date()
start <- today - 365 * 5
sym <- "AAPL"
g <- getSymbols(sym, src = "yahoo", auto.assign = FALSE,
                from = start) %>%
        as.data.frame() %>%
        as_tibble(rownames = "date") %>%
        mutate(date = ymd(date)) %>%
        select(date, AAPL.Adjusted)

g %>%
        ggplot(aes(date, AAPL.Adjusted)) + 
        geom_line() + 
        ylab("AAPL Adjusted Closing Price") + 
        xlab("Date")

## Polynomials
dfr <- 6
fit <- lm(AAPL.Adjusted ~ poly(date, 6), g)
tidy(fit)

B <- poly(g$date, dfr) %>%
        as_tibble() %>%
        mutate(date = g$date) %>%
        slice(seq(1, n(), 5)) %>%
        gather(degree, value, -date)

B %>%
        ggplot(aes(date, value)) + 
        geom_line() + 
        facet_wrap(vars(degree))

g$pred <- predict(fit)
g %>%
        ggplot(aes(date, AAPL.Adjusted)) + 
        geom_line() + 
        geom_line(aes(date, pred), color = "red", lwd = 2)


## B-splines
dfr <- 12
fit <- lm(AAPL.Adjusted ~ bs(date, dfr), g)
tidy(fit)

g$pred <- predict(fit)
g %>%
        select(date, AAPL.Adjusted, pred)  %>%
        gather(model, value, -date) %>%
        ggplot(aes(date, value)) + 
        geom_line(aes(color = model))

B <- bs(g$date, dfr) %>%
        as_tibble() %>%
        mutate(date = g$date) %>%
        slice(seq(1, n(), 5)) %>%
        gather(basis, value, -date) %>%
        mutate(basis = factor(basis, levels = 1:dfr))

B %>%
        ggplot(aes(date, value)) + 
        geom_line(aes(color = basis))

B %>%
        ggplot(aes(date, value)) + 
        geom_line() + 
        facet_wrap(vars(degree))

B <- bs(g$date, dfr)
Bm <- sweep(B, 2, coef(fit)[-1], "*")
Bm %>%
        as_tibble() %>%
        mutate(date = g$date) %>%
        slice(seq(1, n(), 5)) %>%
        gather(basis, value, -date) %>%
        mutate(basis = factor(basis, levels = 1:dfr)) %>%
        ggplot(aes(date, value)) + 
        geom_line(aes(color = basis))

tidy(fit) %>%
        slice(-1) %>%
        mutate(term = 1:dfr) %>%
        ggplot(aes(term, estimate)) + 
        geom_point()



## Discontinuous function with B-splines
dfr <- 12
p <- tibble(x = 1:1000,
            y = 0 + 5 * (x > 500) + rnorm(1000))
p %>%
        ggplot(aes(x, y)) + 
        geom_line() + 
        xlab("Time") + 
        ylab("Level")

fit <- lm(y ~ bs(x, dfr), p)
tidy(fit)
p$pred <- predict(fit)
p %>%
        select(x, y, pred)  %>%
        ggplot(aes(x, y)) + 
        geom_line() + 
        geom_line(aes(x, pred), color = "red", lwd = 3)


tidy(fit) %>%
        slice(-1) %>%
        mutate(term = 1:dfr) %>%
        ggplot(aes(term, estimate)) + 
        geom_point()


## Discontinuous function, more noise
dfr <- 12
set.seed(100)
p <- tibble(x = 1:1000,
            y = 0 + 5 * (x > 400) + rnorm(1000, sd = 20))
p %>%
        ggplot(aes(x, y)) + 
        geom_line() + 
        xlab("Time") + 
        ylab("Level")
fit <- lm(y ~ bs(x, dfr), p)
tidy(fit)
p$pred <- predict(fit)
p %>%
        select(x, y, pred)  %>%
        ggplot(aes(x, y)) + 
        geom_line() + 
        geom_line(aes(x, pred), color = "red", lwd = 3)

tidy(fit) %>%
        slice(-1) %>%
        mutate(term = 1:dfr) %>%
        ggplot(aes(term, estimate)) + 
        geom_point()

B <- bs(p$x, dfr)
Bm <- sweep(B, 2, coef(fit)[-1], "*")
Bm %>%
        as_tibble() %>%
        mutate(x = p$x) %>%
        slice(seq(1, n(), 5)) %>%
        gather(basis, value, -x) %>%
        mutate(basis = factor(basis, levels = 1:dfr)) %>%
        ggplot(aes(x, value)) + 
        geom_line(aes(color = basis))

## Baltimore Temperature
d <- read_csv("balt_tempF.csv", col_types = c("Dd")) %>%
        mutate(year = year(date),
               time = 1:n())
d %>%
        ggplot(aes(date, tempF)) + 
        geom_line() + 
        ylab("Temperature (F)") + 
        xlab("") + 
        ggtitle("Baltimore Temperature, 1987-2005")

d %>%
        filter(year >= 2005) %>%
        ggplot(aes(date, tempF)) + 
        geom_line() + 
        ylab("Temperature (F)") + 
        xlab("") + 
        ggtitle("Baltimore Temperature, 2005")

n <- length(d$tempF)
xpts <- seq_len(n)
f <- fft(d$tempF) / n
f1 <- f[1]  ## Mean of the series
ff <- f[2:(1 + floor(length(f) / 2))]
head(ff)

r <- tibble(R2 = Re(ff)^2 + Im(ff)^2,
            freq = 1:length(ff))
r %>%
        ggplot(aes(freq, R2)) + 
        geom_point()

r %>%
        ggplot(aes(freq, R2)) + 
        geom_point() + 
        scale_y_log10()
r %>%
        slice(1:40) %>%
        ggplot(aes(freq, R2)) + 
        geom_point() + 
        scale_y_log10()

curve(sin(2 * pi * 2 * x / n), 1, n, n = 1000)

lm(tempF ~ time + sin(2*pi*19*time/n) + cos(2*pi*19*time/n), d) %>%
        tidy()

fit <- lm(tempF ~ harmonic(time, 40, nrow(d)), d)
tidy(fit) %>%
        filter(!grepl("Intercept", term)) %>%
        mutate(freq = 1:(40 * 2)) %>%
        ggplot(aes(freq, estimate)) + 
        geom_point()


## AAPL data

today <- Sys.Date()
start <- today - 365 * 5
sym <- "AAPL"
g <- getSymbols(sym, src = "yahoo", auto.assign = FALSE,
                from = start) %>%
        as.data.frame() %>%
        as_tibble(rownames = "date") %>%
        mutate(date = ymd(date)) %>%
        select(date, AAPL.Adjusted) %>%
        mutate(time = seq_len(n()))

dfr <- 20
fit <- lm(AAPL.Adjusted ~ harmonic(time, dfr, nrow(g)), g)
tidy(fit) %>%
        filter(!grepl("Intercept", term)) %>%
        mutate(freq = 1:(dfr * 2)) %>%
        ggplot(aes(freq, estimate)) + 
        geom_point()

g$pred <- predict(fit)
g %>%
        ggplot(aes(date, AAPL.Adjusted)) + 
        geom_line() + 
        geom_line(aes(date, pred), col = "red")

f <- fft(g$AAPL.Adjusted)
ff <- f[2:(1 + floor(length(f) / 2))]
r <- tibble(R2 = Re(ff)^2 + Im(ff)^2,
            freq = 1:length(ff))
r %>%
        ggplot(aes(freq, R2)) + 
        geom_point() + 
        scale_y_log10()

## Discontinuous data, continued
p <- tibble(x = 1:1000,
            y = 0 + 5 * (x > 500) + rnorm(1000))
p %>%
        ggplot(aes(x, y)) + 
        geom_line() + 
        xlab("Time") + 
        ylab("Level")
dfr <- 20
fit <- lm(y ~ harmonic(x, dfr, nrow(p)), p)
p$pred <- predict(fit)
p %>%
        ggplot(aes(x, y)) + 
        geom_line() + 
        geom_line(aes(x, pred), col = "red", lwd = 2)


set.seed(100)
p <- tibble(x = 1:1000,
            y = 0 + 5 * (x > 400) + rnorm(1000, sd = 20))
p %>%
        ggplot(aes(x, y)) + 
        geom_line() + 
        xlab("Time") + 
        ylab("Level")

wv <- c(rep(-1, 20), rep(1, 20))
w <- stats::filter(p$y, wv)
plot(w, pch = 20)        
        
        
        
        