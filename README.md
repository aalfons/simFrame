# simFrame: Simulation Framework

A general framework for statistical simulation. More information can be found in our article:

[A. Alfons, M. Templ & P. Filzmoser (2010). An Object-Oriented Framework for Statistical Simulation: The R Package simFrame. *Journal of Statistical Software*, 37(3), 1-36. doi: 10.18637/jss.v037.i03.](https://doi.org/10.18637/jss.v037.i03)


## Installation

The `R` package `simFrame` is on CRAN (The Comprehensive R Archive Network), hence it can be easily installed from the `R` command line via

```
install.packages("simFrame")
```


## Vignettes

CRAN requires that *suggested* packages, such as those only used in examples and vignettes, are only used conditionally with `requireNamespace("<package>")` so that `R CMD check` still goes through even if those package are not installed.  The vignettes rely heavily on packages `laeken`, `mvtnorm`, and `robCompositions`, and using them conditionally would greatly impair the readability of the vignettes.

Therefore the vignettes are no longer included in `simFrame` version 0.5.4.  Instead, they can be found here in folder `vignettes`:

 * `simFrame-intro.pdf`: The general introduction to the package based on the 
   above mentioned article in the *Journal of Statistical Software*.
 * `simFrame-eusilc.pdf`: Additional examples for design-based simulations using    synthetic EU-SILC population data.
