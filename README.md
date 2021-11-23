# simFrame: Simulation Framework

[![CRAN](https://www.R-pkg.org/badges/version/simFrame)](https://CRAN.R-project.org/package=simFrame) 


A general framework for statistical simulation, which allows researchers to make use of a wide range of simulation designs with minimal programming effort.  The package provides functionality for drawing samples from a distribution or a finite population, for adding outliers and missing values, as well as for visualization of the simulation results.  It follows a clear object-oriented design and supports parallel computing to increase computational performance.

To cite package `simFrame` in publications, please use:

A. Alfons, M. Templ & P. Filzmoser (2010). An Object-Oriented Framework for Statistical Simulation: The R Package simFrame. *Journal of Statistical Software*, 37(3), 1-36. DOI [10.18637/jss.v037.i03](https://doi.org/10.18637/jss.v037.i03).


## Installation

Package `simFrame` is on CRAN (The Comprehensive R Archive Network), hence the latest release can be easily installed from the `R` command line via

```
install.packages("simFrame")
```


## Vignettes

CRAN requires that *suggested* packages, such as those only used in examples and vignettes, are only used conditionally with `requireNamespace("<package>")` so that `R CMD check` still goes through even if those package are not installed.  The vignettes rely heavily on packages `laeken`, `mvtnorm`, and `robCompositions`, and using them conditionally would greatly impair the readability of the vignettes.

Therefore the vignettes are no longer included in `simFrame` version 0.5.4.  Instead, they can be found here in folder `vignettes`:

 * `simFrame-intro.pdf`: The general introduction to the package based on the 
   above mentioned article in the *Journal of Statistical Software*.
 * `simFrame-eusilc.pdf`: Additional examples for design-based simulations using    synthetic EU-SILC population data.


## Report issues and request features

If you experience any bugs or issues or if you have any suggestions for additional features, please submit an issue via the [*Issues*](https://github.com/aalfons/simFrame/issues) tab of this repository.  Please have a look at existing issues first to see if your problem or feature request has already been discussed.


## Contribute to the package

If you want to contribute to the package, you can fork this repository and create a pull request after implementing the desired functionality.
