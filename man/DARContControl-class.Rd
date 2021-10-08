\encoding{utf8}
\name{DARContControl-class}
\Rdversion{1.1}
\docType{class}
\alias{DARContControl-class}
\alias{DARContControl}
\alias{getFun,DARContControl-method}
\alias{setFun,DARContControl-method}
\alias{getDots,DARContControl-method}
\alias{setDots,DARContControl-method}
%% aliases to avoid confusion due to capitalization
\alias{DARcontControl}
\alias{DARcontcontrol}
\alias{DARContcontrol}
\alias{DARcontControl-class}
\alias{DARcontcontrol-class}
\alias{DARContcontrol-class}
\alias{darcontControl}
\alias{darcontcontrol}
\alias{darContcontrol}
\alias{darcontControl-class}
\alias{darcontcontrol-class}
\alias{darContcontrol-class}

\title{Class "DARContControl"}
\description{
  Class for controlling contamination in a simulation experiment.  The values
  of the contaminated observations will be distributed at random (\emph{DAR}),
  i.e., they will depend on on the original values.
}
\section{Objects from the Class}{
Objects can be created by calls of the form
\code{new("DARContControl", \dots)}, \code{DARContControl(\dots)} or
\code{ContControl(\dots, type="DAR")}.
}
\section{Slots}{
  \describe{
    \item{\code{target}:}{Object of class \code{"OptCharacter"}; a character
      vector specifying specifying the variables (columns) to be contaminated,
      or \code{NULL} to contaminate all variables (except the additional ones
      generated internally).}
    \item{\code{epsilon}:}{Object of class \code{"numeric"} giving the
      contamination levels.}
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
Class \code{"\linkS4class{ContControl}"}, directly.
Class \code{"\linkS4class{VirtualContControl}"}, by class "ContControl", distance 2.
Class \code{"\linkS4class{OptContControl}"}, by class "ContControl", distance 3.
}
\section{Details}{
  With this control class, contamination is modeled as a two-step process.  The
  first step is to select observations to be contaminated, the second is to
  model the distribution of the outliers.  In this case, the original values
  will be modified by the function given by slot \code{fun}, i.e., values of
  the contaminated observations will depend on on the original values.
}
\section{Accessor and mutator methods}{
  In addition to the accessor and mutator methods for the slots inherited from
  \code{"\linkS4class{ContControl}"}, the following are available:

  \describe{
    \item{\code{getFun}}{\code{signature(x = "DARContControl")}: get slot
      \code{fun}.}
    \item{\code{setFun}}{\code{signature(x = "DARContControl")}: set slot
      \code{fun}.}
    \item{\code{getDots}}{\code{signature(x = "DARContControl")}: get slot
      \code{dots}.}
    \item{\code{setDots}}{\code{signature(x = "DARContControl")}: set slot
      \code{dots}.}
  }
}
\section{Methods}{
  Methods are inherited from \code{"\linkS4class{ContControl}"}.
}
\section{UML class diagram}{
  A slightly simplified UML class diagram of the framework can be found in
  Figure 1 of the package vignette \emph{An Object-Oriented Framework for
  Statistical Simulation: The \R Package \code{simFrame}}.  Use
  \code{vignette("simFrame-intro")} to view this vignette.
}
\note{
  The slot \code{grouping} was named \code{group} prior to version 0.2.
  Renaming the slot was necessary since accessor and mutator functions were
  introduced in this version and a function named \code{getGroup} already
  exists.
}
\author{Andreas Alfons}
\references{
Alfons, A., Templ, M. and Filzmoser, P. (2010) An Object-Oriented Framework for
Statistical Simulation: The \R Package \pkg{simFrame}. \emph{Journal of
Statistical Software}, \bold{37}(3), 1--36. \doi{10.18637/jss.v037.i03}.

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
  \code{"\linkS4class{DCARContControl}"}, \code{"\linkS4class{ContControl}"},
  \code{"\linkS4class{VirtualContControl}"}, \code{\link{contaminate}}
}
\examples{
require(mvtnorm)
mean <- rep(0, 2)
sigma <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
foo <- generate(size = 10, distribution = rmvnorm,
    dots = list(mean = mean, sigma = sigma))
cc <- DARContControl(target = "V2",
    epsilon = 0.2, fun = function(x) x * 100)
contaminate(foo, cc)
}
\keyword{classes}
