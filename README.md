# pam

## Features

### wrapper pdf

- create test for data that produces NA

## waiting

- legende für kombidiagramm das mit den vielen linien

## Test

### test all

```
library(devtools);
devtools::test();
```

### test specific file

```
library(devtools);
devtools::load_all();
library(testthat);
test_file('$$path')"
```

## Linux dependecies for devtools

Ubuntu:
libxml2-dev
libssl-dev
libcurl4-openssl-dev
libfontconfig1-dev
libharfbuzz-dev
libfribidi-dev
libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev

Debian:
libxml2-dev libssl-dev libcurl4-openssl-dev libfontconfig1-dev libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev libjpeg-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev

install.packages("data.table")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("minpack.lm")
install.packages("SciViews")
install.packages("ggthemes")
install.packages("gridExtra")
