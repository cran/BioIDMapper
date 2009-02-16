\name{bio.convert}
\alias{bio.convert}

\title{ Main Interface for Mapping }
\description{
  This function is main interface for mapping service. It can support NCBI 
  mapping and UniProt mapping, from gene level, protein level, functional level,
  and metabolomic level. 
}
\usage{
bio.convert(id_list = "character", from = "numeric", to = "numeric")
}

\arguments{
  \item{id_list}{ the list of biological ids}
  \item{from}{ Biokey number for source biological type. It can be found by bio.type()}
  \item{to}{ Biokey number for destination biological type. It can be found by bio.type()}
}

\details{
It converts a list of ids from one biological type to another. For detail, please check vegnette "demonstration" section.
}
\value{
a result matrix will be returned with source type in the first column and the destination type
in the last column
}

\author{Xiaoyong Sun}

\examples{

data(glist)
bio.type(1)
bio.type(5)
bio.convert(glist, 1, 5)->done
}
\keyword{ methods}

