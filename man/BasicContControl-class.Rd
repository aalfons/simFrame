\name{BasicContControl-class}
\Rdversion{1.1}
\docType{class}
\alias{BasicContControl-class}
\alias{BasicContControl}
\alias{getDots,BasicContControl-method}
\alias{getFun,BasicContControl-method}

\title{Class \code{"BasicContControl"}}
\description{
  Simple class for controlling contamination in a simulation experiment.  The 
  values of the first observations will be contaminated at random (\emph{CAR}), 
  i.e., they will depend on on the original values.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{BasicContControl(\dots)} or 
\code{ContControl(\dots, type="Basic")}.
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
Class \code{"\linkS4class{ContControl}"}, directly.
Class \code{"\linkS4class{VirtualContControl}"}, distance 2.
Class \code{"\linkS4class{OptContControl}"}, distance 3.
}
\section{Details}{
  With this control class, the original values of the first observations will 
  be modified by the function given by slot \code{fun}, i.e., the contaminated 
  values will depend on on the original values.
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
  The following methods are available in addition to the inherited ones:
  
  \describe{
    \item{\code{contaminate}}{\code{signature(x = "data.frame", 
      control = "BasicContControl")}: contaminate data.}
  }
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
}
\seealso{
  \code{\link{contaminate}}
}
\examples{
library("mvtnorm")
mean <- rep(0, 2)
sigma <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
foo <- generate(size = 10, distribution = rmvnorm, 
    dots = list(mean = mean, sigma = sigma))
cc <- BasicContControl(target = "V2",
    epsilon = 0.2, fun = function(x) x * 10)
contaminate(foo, cc)
}
\keyword{classes}
