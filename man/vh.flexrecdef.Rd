\name{vh.flexrecdef}
\alias{vh.flexrecdef}
\title{ Vhayu Flex Record Definitions }
\description{
  Discover and list Vhayu Flex records and their defintions.
}
\usage{
vh.flexrecdef(x = NULL)
}
\arguments{
  \item{x}{ \code{NULL} or character vector}
}
\value{
If \code{x} is \code{NULL} return a character vector of 
flex record definition names; otherwise, return an unformatted flex record
definition as a character vector.}
\examples{
\dontrun{
vh.flexrecdef()
vh.flexrecdef("VhTrade")
}
}
\keyword{ ts }

