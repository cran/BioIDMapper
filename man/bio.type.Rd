\name{bio.type}
\alias{bio.type}

\title{ Get the biokey number this package support}
\description{
  show the biokey number this package support
}
\usage{
bio.type(type_to_id)
}

\arguments{
  \item{type_to_id}{ If no parameter is used, return tables for all biokey numbers;
if parameter is number from bio.type(), return the corresponding biological type; if 
parameter is biological type, return the corresponding biokey number}

}


\value{

}

\author{Xiaoyong Sun}

\examples{

bio.type()
bio.type(5)
bio.type("PDB id")

}
\keyword{  methods  }

