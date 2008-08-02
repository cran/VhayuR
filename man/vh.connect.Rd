\name{vh.connect}
\alias{vh.connect}
\alias{vh.findDLL}
\alias{vh.loadDLL}
\title{ Connect to Vhayu server. }
\description{
  Connecting to Vhayu.
}
\usage{
vh.connect(server = vh.getOption("server"), port = vh.getOption("port"), ...)
vh.findDLL(dllpath = vh.getOption("dllpath"), dllname = "VhayuR.dll", verbose = vh.getOption("verbose"))
vh.loadDLL(dllpath = vh.getOption("dllpath"), verbose = vh.getOption("verbose"))
}

\arguments{
  \item{server}{String representing IP address of Vhayu server.}
  \item{port}{Port.  Normally not set explicitly.  Default value is 
    usually fine.}
  \item{\dots}{ Not used. }
  \item{dllpath}{ Character vector of directories searched for dll. }
  \item{dllname}{ The name of the dll. }
  \item{verbose}{ Give additional messages when running. }
}
\details{
\code{vh.findDLL} locates the dynamic load library (a file with
extension dll) on the dllpath and returns the complete pathname to it.
\code{vh.loadDLL} loads the dynamic load library, \code{VhayuR.dll}.
It sets the Vhayu option
\code{"loaded"} to \code{TRUE}.

\code{vh.connect} first calls \code{vh.loadDLL} if it is not already
loaded and then sets the the correct IP.  It sets the Vhayu option
\code{"connected"} to \code{TRUE}.

Normally neither of these commands are used by users since they
are automatically invoked the first time the package attempts to
access the database.
}
\examples{
\dontrun{
vh.loadDLL()
vh.connect()
}
vh.findDLL()
str(vh.options())
}
\keyword{ ts }
\keyword{ internal }

