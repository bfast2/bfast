# bfast

This fork aims at reducing computation times in the R package [bfast](https://cran.r-project.org/web/packages/bfast/index.html). Parts of the improvements rely on modifications of the [strucchange](https://cran.r-project.org/web/packages/strucchange/index.html) package [(see here)](https://github.com/appelmar/strucchange). You can install both packages with:

```
library(devtools)
install_github("appelmar/strucchange")
install_github("kongdd/bfast")
```


## Use package optimizations

By default, the package will not make use of any modifications. Two functions change the behaviour:

```
set_fast_options()    # use modifications
set_default_options() # use default implementation
```

The example below runs the first example of the `bfastmonitor()` documentation for both settings.


```
library(bfast)
NDVIa <- as.ts(zoo(som$NDVI.a, som$Time))
f <- function() bfastmonitor(NDVIa, start = c(2010, 13)) 

set_default_options()
x = f() 
system.time(replicate(100, f()))

set_fast_options()
y = f()
system.time(replicate(100, f()))

par(mfrow = c(1,2))
plot(x) ; plot(y)
```



## Generate evaluation reports

The package comes with R Markdown reports to evaluate the modifications thoroughly. The following reports can be found in `inst/reports`:

* `report.test.Rmd` runs test cases to make sure that modifications will return equal results,
* `report.benchmark.Rmd` evaluates the speedup of a set of functions, and
* `report.profiling.Rmd` profiles some test functions to identify computational bottlenecks.

To generate `html` reports, run:

```
library(rmarkdown)
outdir = getwd()
rmarkdown::render(system.file("reports/report.test.Rmd",package = "bfast"),output_file = paste(outdir,"/report.test.html",sep=""))
rmarkdown::render(system.file("reports/report.benchmark.Rmd",package = "bfast"),output_file = paste(outdir,"/report.benchmark.html",sep=""))
rmarkdown::render(system.file("reports/report.profiling.Rmd",package = "bfast"),output_file = paste(outdir,"/report.profiling.html",sep=""))
```

Notice that generating the last two reports might take some time.


## Details

Most important modifications include:

* using [RcppArmadillo](https://cran.r-project.org/web/packages/RcppArmadillo/index.html) for computationally intensive operations in strucchange
* avoiding expensive calls of `model.frame()` and `model.matrix` and using the design matrix and response vector instead of a data.frame and a formula
* using `lm.fit()` instead of `lm()` when possible



## Altered by Martin Jung - 2017

Changes made:

- Altered "bfast01":  
  -- Add functionality to support different regressors such as robust linear regression (MASS package)  
  -- Switched to stlplus (stlplus package) as default to allow missing data in time series
- "bfast"  
  -- Switched to stlplus (stlplus package) as default to allow missing data in time series
