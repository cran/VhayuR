\name{VhayuR-package}
\alias{VhayuR-package}
\docType{package}
\title{ R interface to Vhayu time series database }
\description{
R interface to Vhayu time series database
}
\details{
\code{VhayuR} allows one to read 
the \code{VhayuR Velocity} time series database from within \R.  The package
utilizes lower level dynamic load libraries provided by Vhayu.
These dynamic load libraries are necessary to run this package with
Vhayu and must
be separately obtained from Vhayu and installed.
(It is also possible to run the package in a
demo mode with files simulating Vhayu and in that mode the Vhayu libraries are
not required.)

Objects are retrieved from the Vhayu data base 
as \code{data.frame} or \code{zoo} class \R objects.
The class of the returned time component may also be specified or
is \code{POSIXct} if not specified.

In \code{"demo"} mode files are used in place of
\code{Vhayu}.  If care is taken with option settings then
it may be possible to simulate a \code{Vhayu} session 
using files such that only the options change between the
\code{Vhayu} and file-based sessions.  This can be useful 
for demonstrations where \code{Vhayu} is not available.

The primary user functions are \code{\link{vh.get.data.frame}}
and \code{\link{vh.get.zoo}}.

There is an option system 
that can be used to set the server.  It can also be used
to set the default arguments of
\code{\link{vh.get.data.frame}}
and \code{\link{vh.get.zoo}}.  Other options are also
available.
The option system is very similar
to the option system in \R but stores its options in the
\code{VhayuR} package.  See \code{\link{vh.options}}.

The first time the user attempts to access Vhayu in any session
\code{VhayuR} automatically loads the required dynamic load
library (dll) and connects.  Thus the only explicit tasks the user
must do is install the dynamic load libraries from \code{Vhayu}
on the machine
(which is a one-time operation) and set the server IP address.
The server must be set each session but this can be automated by
placing the set server instructions in your \R \code{\link[base]{Startup}}
file which is typically \code{Rprofile.site} found in the \code{etc}
subdirectory of your \R distribution.  The examples below illustrate this.

Examples can be found below, in the accompanying demo
accessible via the \R \code{\link{demo}}
command and in further examples in the help files.

}
\references{ 
Vahyu Velocity Vhayu R Integration Guide.
Vhayu Velocity API Reference Guide.
}
\examples{
\dontrun{

# change this to your server IP address
# (You don't need this if you set up your startup file 
# appropriately as discussed below.)
vh.options(server = "10.10.1.50")

str(vh.options()) # show options

vh.flexrecdef() # check available flex record definitions
vh.flexrecdef("VhTrade") # view definition for VhTrade

goog.df <- vh.get.data.frame("GOOG", 
	frDef = "VhTrade",  
	startTime = "20051201 09:30:00", 
	endTime = "20051201 16:00:00", 
	fieldNames = "VhExchgTime VhPrice",
	FUN = as.chron)

### This R command gives the location of your Rprofile.site file
file.path(R.home(), "etc", "Rprofile.site")

### Place these lines in your R Startup file
### Replace IP address shown with the one you use to connect to Vhayu.
setHook(packageEvent("VhayuR", "attach"),
	function(...) vh.options(server = "10.10.1.50")       
)

}
}
\keyword{ts}

