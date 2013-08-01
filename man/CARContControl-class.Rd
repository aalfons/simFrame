\encoding{utf8}
\name{CARContControl-class}
\Rdversion{1.1}
\docType{class}
\alias{CARContControl-class}
\alias{CARContControl}
\alias{getDots,CARContControl-method}
\alias{getFun,CARContControl-method}

\title{Class \code{"CARContControl"}}
\description{
  Class for controlling contamination in a simulation experiment.  The values 
  of the contaminated observations will be contaminated at random (\emph{CAR}), 
  i.e., they will depend on on the original values.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{CARContControl(\dots)} or 
\code{ContControl(\dots, type="CAR")}.
}
\section{Slots}{
  \describe{
    \item{\code{target}:}{Object of class \code{"OptCharacter"}; a character 
      vector specifying specifying the variables (columns) to be contaminated, 
      or \code{NULL} to contaminate all variables (except the additional ones 
      generated internally).}
    \item{\code{epsilon}:}{Object of class \code{"numeric"} giving the 
      contamination levels.}
    \item{\code{tuning}:}{Object of class \code{"ListOrDataFrame"} giving 
      tuning parameters for the contamination.  Lists are converted to data 
      frames with \code{\link{expand.grid}} during initialization.}
    \item{\code{indices}:}{Object of class \code{"NumericMatrix"} in which the 
      first column contains the indices of the contamination levels and the 
      second column contains the row indices of the tuning parameters for the 
      different contamination settings.}
    \item{\code{grouping}:}{Object of class \code{"character"} specifying a 
      grouping variable (column) to be used for contaminating whole groups 
      rather than individual observations.}
    \item{\code{aux}:}{Object of class \code{"character"} specifying an 
      auxiliary variable (column) whose values are used as probability weights 
      for selecting the items (observations or groups) to be contaminated.}
    \item{\code{fun}:}{Object of class \code{"function"} generating 
      the values of the contamination data.  The original values of the 
      observations to be contaminated will be passed as its first argument.  
      Furthermore, it should return an object that can be coerced to a 
      \code{data.frame}, containing the contamination data.}
    \item{\code{dots}:}{Object of class \code{"list"} containing additional 
      arguments to be passed to \code{fun}.}
  }
}
\section{Extends}{
Class \code{"\linkS4class{RandomContControl}"}, directly.
Class \code{"\linkS4class{ContControl}"}, distance 2.
Class \code{"\linkS4class{VirtualContControl}"}, distance 3.
Class \code{"\linkS4class{OptContControl}"}, distance 4.
}
\section{Details}{
  With this control class, contamination is modeled as a two-step process.  The 
  first step is to randomly select observations to be contaminated, the second 
  is to model the distribution of the outliers.  In this case, the original 
  values will be modified by the function given by slot \code{fun}, i.e., 
  values of the contaminated observations will depend on on the original 
  values.
}
\section{Accessor methods}{
  The following accessor methods are available in addition to the inherited 
  ones:
  
  \describe{
    \item{\code{getFun}}{\code{signature(x = "CARContControl")}: get slot 
      \code{fun}.}
    \item{\code{getDots}}{\code{signature(x = "CARContControl")}: get slot 
      \code{dots}.}
  }
}
\section{Methods}{
  Methods are inherited.
}
\section{UML class diagram}{
  A slightly simplified UML class diagram of the framework can be found in 
  Figure 1 of the package vignette \emph{An Object-Oriented Framework for 
  Statistical Simulation: The \R Package \code{simFrame}}.  Use 
  \code{vignette("simFrame-intro")} to view this vignette.
}
\author{Andreas Alfons}
\references{
Alfons, A., Templ, M. and Filzmoser, P. (2010) An Object-Oriented Framework for 
Statistical Simulation: The \R Package \pkg{simFrame}. \emph{Journal of 
Statistical Software}, \bold{37}(3), 1--36. URL 
\url{http://www.jstatsoft.org/v37/i03/}.

Alfons, A., Templ, M. and Filzmoser, P. (2010) Contamination Models in the \R 
Package \pkg{simFrame} for Statistical Simulation. In Aivazian, S., Filzmoser, 
P. and Kharin, Y. (editors) \emph{Computer Data Analysis and Modeling: Complex 
Stochastic Data and Systems}, volume 2, 178--181. Minsk. ISBN 978-985-476-848-9.

\enc{Béguin}{Beguin}, C. and Hulliger, B. (2008) The BACON-EEM Algorithm for 
Multivariate Outlier Detection in Incomplete Survey Data. \emph{Survey 
Methodology}, \bold{34}(1), 91--103.

Hulliger, B. and Schoch, T. (2009) Robust Multivariate Imputation with Survey 
Data. \emph{57th Session of the International Statistical Institute}, Durban.
}
\seealso{
  \code{"\linkS4class{CCARContControl}"}, \code{\link{contaminate}}
}
\examples{
library("mvtnorm")
mean <- rep(0, 2)
sigma <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
foo <- generate(size = 10, distribution = rmvnorm, 
    dots = list(mean = mean, sigma = sigma))
cc <- CARContControl(target = "V2",
    epsilon = 0.2, fun = function(x) x * 10)
contaminate(foo, cc)
}
\keyword{classes}
