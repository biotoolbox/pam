# Developer documentation

## Branches
- The [main](https://github.com/biotoolbox/pam/tree/main) branch is always up to date with the version published on [cran](https://cran.r-project.org/web/packages/pam/index.html).
- The [dev](https://github.com/biotoolbox/pam/tree/dev) branch is stable but under active development and will eventually merged into the [main](https://github.com/biotoolbox/pam/tree/main) branch.

## Makefile
- In the [Makefile](../Makefile) are some shortcuts for commonly used commands (install, test, build).

## Required R packages
- To install all required R package dependencies for development you can use those commands:
```
packages <- readLines("packages.txt")
install.packages(packages)
```

## Custom helper functions
If additionally helper functions are needed (e.g. removing certain data points), it is possible to intercept the intermediate_table between the read and the generate_regression function.

<p align="center">
  <img src="../img/flow.png" alt="Processing pipeline overview" width="400">
</p>