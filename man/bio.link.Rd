\name{bio.link}
\alias{bio.link}

\title{ Link Between BioIDMapper and External Data Sources}
\description{
Link to external data sources through specified ID with web browser
}
\usage{
bio.link(id, to)
}

\arguments{
  \item{id}{ one biolgoical id }
  \item{to}{biokey number. check bio.type()}
}
\details{
It opens web browser and links directly to the specified database. For detail, please check vignette "demonstration" section.

}

\author{Xiaoyong Sun }

\examples{
bio.type(1)
\dontrun{bio.link("200529", 1)}
}

\keyword{ methods }

