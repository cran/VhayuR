\name{VhayuR-internal}

\alias{vh.Announce}
\alias{vh.fileSearch}
\alias{vh.getFileArgs}
\alias{vh.getVhayu.data.frame}
\alias{vh.getVhayu.list}
\alias{vh.getVhayu.matrix}
\alias{vh.getVhayu.zoo}
\alias{vh.getFile.data.frame}
\alias{vh.getFile.zoo}
\alias{vh.install}

\alias{VhExecuteQuery}
\alias{VhGetRecords}
\alias{VhRollBySchedule}
\alias{VhRollBySymbolList}
\alias{VhRollBySymbology}
\alias{VhRollSetup}
\alias{VhSetServer}

\title{Internal VhayuR objects}
\description{
  Internal chron objects.
}
\usage{
vh.fileSearch(file, datapath = vh.getOption("datapath"), ...)
}
\arguments{
 \item{file}{file name searched or filepath. In latter case just return it.}
 \item{datapath}{character vector of directories to search for file.}
 \item{\dots}{not used}
}
\details{
  These functions are not normally called by the user.
}
\keyword{ts}
\keyword{internal}

