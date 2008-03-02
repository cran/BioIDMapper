\name{bio.select}
\alias{bio.select}
\title{ select subset from mapped matrix}
\description{
  use id as paramter, this function can select subste from mapping result
}
\usage{
bio.select(result_matrix, colno, myid)
}

\arguments{
  \item{result_matrix}{ the returned matrix from bio.convert() }
  \item{colno}{ the column number from returned matrix, which contains the id you are interesed }
  \item{myid}{  id you are interesed }
}

\author{Xiaoyong Sun}

\examples{
data(glist)
bio.convert(glist, 1, 5)->done
bio.select(done, 1, "200529" )
}

\keyword{  methods  }

