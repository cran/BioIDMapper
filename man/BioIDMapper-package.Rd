\name{BioIDMapper-package}
\alias{BioIDMapper-package}
\alias{BioIDMapper}
\docType{package}
\title{
Mapping biological ids and linking to external data source
}
\description{
This package will map ids among different biological ids from NCBI and Uniprot. 
Ids will be from gene level, protein level, functional level and metabolomic level.
}
\details{
\tabular{ll}{
Package: \tab BioIDMapper\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2007-09-05\cr
License: \tab What license is it under?\cr
}
The main interface is bio.convert() function
}
\author{
Xiaoyong Sun
Maintainer: johnsunx1<johnsunx1@gmail.com>
}

\keyword{ package }

\examples{
data(glist)
bio.convert(glist, 1, 6)
}
