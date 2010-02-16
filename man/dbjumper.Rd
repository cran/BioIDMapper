\name{dbjumper}
\alias{dbjumper}

\title{ Parse mapping pathway }
\description{
parse mapping pathway for gene, protein, metabolomic
}
\usage{
dbjumper(id_list, from, to, pathMatrix)
}

\arguments{
  \item{id_list}{the list of biological ids}
  \item{from}{Biokey number for source biological type. It can be found by bio.type()}
  \item{to}{Biokey number for destination biological type. It can be found by bio.type()}
  \item{pathMatrix}{mapping schema}
}

\value{
return a matrix with mapped result
}

\author{ Xiaoyong Sun }

\examples{

}
\keyword{ methods }

