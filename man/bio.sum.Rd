\name{bio.sum}
\alias{bio.sum}
\title{ analyze mapped ids}
\description{
  analyze the mapped ids from bio.convert()
}
\usage{
bio.sum(final_matrix, start_matrix, option)
}

\arguments{
  \item{final_matrix}{ the returned matrix from bio.convert()}
  \item{start_matrix}{ the original vector or matrix for mapping }
  \item{option}{ a logical value. If TRUE, all summary results are returned. If FALSE, only basic summary are returned. default value is FALSE }
}

\author{Xiaoyong Sun}

\examples{


data(glist)
bio.convert(glist, 1, 6)->done
bio.sum(done)
bio.sum(done, glist)
bio.sum(done, glist, TRUE)
}

\keyword{  methods  }

