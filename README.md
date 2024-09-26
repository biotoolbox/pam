# pam

# Features:

### wrapper pdf

- create test for data that produces NA

## waiting....

- alle Berechnungen aus original Quellen prüfen
- legende für kombidiagramm das mit den vielen linien
- plot method rework

# Test

## test all

```
library(devtools);
devtools::test();
```

## test specific file

```
library(devtools);
devtools::load_all();
library(testthat);
test_file('$$path')"
```

# Linux dependecies for devtools:
libxml2-dev
libssl-dev
libcurl4-openssl-dev
libfontconfig1-dev
libharfbuzz-dev
libfribidi-dev
libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev 