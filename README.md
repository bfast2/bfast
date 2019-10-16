# bfast

This fork aims to consolidate all the forks of the `bfast` package to have a unified, updated package that can be submitted back to CRAN eventually and become the new upstream. The features integrated into this fork so far are:

* Documentation ported to `roxygen2` to make for easier development of functions and package building ([GreatEmerald/bfast](https://github.com/GreatEmerald/bfast))
* Optimised performance by porting critical paths to C++ ([appelmar/bfast](https://github.com/appelmar/bfast))
* Making use of the `stlplus` package for handling time series with NA values, and optional use of robust regression ([Martin-Jung/bfast](https://github.com/Martin-Jung/bfast))

Parts of the improvements rely on modifications of the [strucchange](https://cran.r-project.org/web/packages/strucchange/index.html) package [(see here)](https://github.com/appelmar/strucchange). You can install both packages with:

```
library(devtools)
install_github("appelmar/strucchange")
install_github("GreatEmerald/bfast")
```


## Enabling performance optimisations

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



### Generating evaluation reports

The package comes with R Markdown reports to evaluate the performance modifications thoroughly. The following reports can be found in `inst/reports`:

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


### Details

Most important performance modifications include:

* using [RcppArmadillo](https://cran.r-project.org/web/packages/RcppArmadillo/index.html) for computationally intensive operations in strucchange
* avoiding expensive calls of `model.frame()` and `model.matrix` and using the design matrix and response vector instead of a data.frame and a formula
* using `lm.fit()` instead of `lm()` when possible

## Contribution guidelines:
We always welcome contributions to the package. If you would like to propose a new functionality and/or report an issue, please use GitHub's tracker called [Issues](https://github.com/bfast2/bfast/issues).

For development we use the [GitHub Flow](https://guides.github.com/introduction/flow/) branching model.

Key steps:

1. Create a GitHub issue in this repository with description of the work that you plan to do.
2. Assign yourself to the GitHub issue you are working on, to inform other developers that you are working on it.
3. Create your own working fork or branch based on the `dev` branch.
4. Make your changes in that branch.
5. Commit your changes to your working branch as long as you are not finished with your development.
6. Make sure that all tests pass (e.g., in Travis or Circle CI).
7. Once your work is finished, create a pull request so that another developer can review your changes before merging them with the `dev` (or `main`) branch.

Additional steps for [preparing a new release](https://guide.esciencecenter.nl/best_practices/releases.html):

8. Update the `NEWS.Rd` file with most notable changes.
9. Add new contributors to the `DESCRIPTION` file if applicable.
10. Release the package and make it citable (add `CITATION.cff` including DOI).
