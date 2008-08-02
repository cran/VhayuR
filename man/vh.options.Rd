\name{vh.options}
\alias{vh.defaultOptions}
\alias{vh.options}
\alias{vh.getOption}
\title{ Vhayu Options }
\description{
  Options in the VhayuR package.
}
\usage{
vh.defaultOptions(set = c("all", "set", "unset"))
vh.getOption(name)
vh.options(...)
}
\arguments{
  \item{set}{ Return all default options, just those which are set to a 
non-NULL value in the Vhayu options or just those which are currently NULL.}
  \item{name}{ The name of a Vhayu option. }
  \item{\dots}{ See below. }
}
\details{
Vhayu options are like \R options except they are stored in
the VhayuR package and not with regular \R options. \code{vh.options}
is similar to the \R \code{\link[base]{options}} command 
and \code{vh.getOption}
is similar to the \R \code{\link[base]{getOption}} command.

Invoking \code{vh.options()} with no arguments returns a list with the
  current values of the options.  Note that not all options listed below
  are set initially.  To access the value of a single option, one should
  use \code{getOption("demo")}, e.g., rather than
  \code{options("demo")} which is a \emph{list} of length one.
  \code{vh.defaultOptions()} 
returns a named list of 
the default, rather than current, values of all options (or of just the
default options whose current values are set or not set).

Each time the VhayuR package is loaded, a new empty list is defined to hold
the options and the default values are loaded into that list.

}
\value{
\code{"vh.options"} and \code{"vh.defaultOptions"} each returns a named list.
\code{"vh.getOption"} returns the value of a
single Vhayu option. 

}


\section{Options used in VhayuR package}{
  \describe{

    \item{\code{loaded}:}{\code{TRUE} if the \code{VhayuR.dll} 
      dynamic load library (dll) is loaded. Automatically set. Not normally
      changed by user.}

	\item{\code{port}:}{This option is the port address that is used to 
		contact the
	  Vhayu server. Usually the default is acceptable so the user normally
      need not to modify this value.}

	\item{\code{server}:}{This option should be set by the user to the 
		IP address of their Vhayu server during each session.}

    \item{\code{verbose}:}{This option provides
	additional messages when running the 
		\code{VhayuR}
	  package.  When the \code{VhayuR} package is loaded \code{verbose}
      is loaded with the value of the \R option of the same name.} }

Other options are discussed in \code{\link{vh.get.data.frame}}.
}
\seealso{ \code{\link[base]{options}} }
\examples{
# compactly list Vhayu options
str(vh.options())

# set an option and then restore to original state
vh.getOption("x")
op <- vh.options(x = 4)
vh.getOption("x")
vh.options(op)
vh.getOption("x")

\dontrun{
# reset all options which have defaults to their defaults
vh.options(vh.defaultOptions())
# reset all options which have defaults and are unset
vh.options(vh.defaultOptions("unset"))

vh.options(server = "10.10.1.50")
}

}
\keyword{ ts }


