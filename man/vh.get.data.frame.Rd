\name{vh.get.data.frame}
\alias{vh.get.data.frame}
\alias{vh.get.zoo}
\alias{vh.tail1}
\title{ Get data from Vhayu. }
\description{
    Retrieve data from Vhayu server (or file if in demo mode).
}
\usage{
vh.get.data.frame(x, ...)
vh.get.zoo(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ Name of Vhayu table (or if in demo mode then the name or 
   the name and filepath of the file). }
  \item{\dots}{ Other arguments. }
}
\details{
  If the Vhayu option \code{"demo"} is \code{FALSE}, which it is by default,
then these functions
retrieve data from the Vhayu server.  In that case the arguments 
\code{"frDef"}, \code{"startTime"}, \code{"endTime"}, \code{"fieldNames"},
\code{"filter"} and \code{"maxRows"} may be specified or will be taken
from the Vhayu options if not specified.  As discussed in the Vhayu R Integration Guide, \code{"frDef"} is the name of a flex record definition (see the command \code{\link{vh.flexrecdef}} which 
can display the possible names usable here), \code{"startTime"} and \code{"endTime"} are each strings of the form \code{"yyyymmdd HH:MM:SS"}, \code{filter} is the name of a Vhayu filter and maxRows is the maximum number of rows to retrieve or 0 if all records are desired.  

If the Vhayu option \code{"demo"} is \code{TRUE} then \code{vh.get.zoo} will
call \code{\link[zoo]{read.zoo}} instead of Vhayu 
and \code{vh.get.data.frame}  will call \code{\link[utils]{read.table}}.
Any arguments known to these two 
functions may be passed while the remaining arguments will be ignored.
The \code{x} argument is interpreted as a file name which is looked up along the path defined by the Vhayu option \code{"datapath"}.  
If not found it will also search a file of the same name suffixed with
\code{".csv"}, \code{".txt"} or \code{".dat"}.  If a pathname and filename are specified rather than just a filename then that path is used.
If the \code{\link[zoo]{read.zoo}} and \code{\link[utils]{read.table}} 
arguments of \code{"sep"}
or \code{"header"} are not specified then there will be an 
attempt to infer them from
the first line of the file assuming a file with a comma in the first line 
should have \code{sep = ","} and a file with an alphabetic in its first line
should have \code{header = TRUE}.

In the case of \code{vh.get.zoo} the \code{aggregate} argument can
be used to specify
an \code{R} function such as \code{vh.tail1}, \code{mean} or \code{median} to
aggregate data values corresponding to a single time value.   If
\code{aggregate} is not specified 
and if there are multiple
values for any time then a warning will be issued; however, a \code{zoo}
object will still be produced.  Such an object may not participate in merges but
may be printed, plotted or aggregated. In the case of \code{vh.get.data.frame}
aggregate is not used and these comments do not apply.  } 

\value{
\code{vh.get.data.frame} returns a data frame and \code{vh.get.zoo} returns a
zoo object. The times are returned as \code{POSIXct} objects
unless the \code{FUN} argument is specified.  In this latter case,
\code{FUN} is 
an \R function that converts times from \code{POSIXct} to whatever class is
desired.  See \code{\link[base]{DateTimeClasses}}.
}
\references{ 
Vahyu Velocity Vhayu R Integration Guide.
Vhayu Velocity API Reference Guide.
}
\examples{
\dontrun{
vh.flexrecdef() # check which flex record definitions are available
vh.flexrecdef("VhTrade") # view definition for VhTrade

goog.df <- vh.get.data.frame("GOOG", 
	frDef = "VhTrade",  
	startTime = "20051201 09:30:00", 
	endTime = "20051201 16:00:00", 
	fieldNames = "VhExchgTime VhPrice",
	FUN = as.chron)

goog.zoo <- vh.get.zoo("GOOG", 
	frDef = "VhTrade",  
	startTime = "20051201 09:30:00", 
	endTime = "20051201 16:00:00", 
	fieldNames = "VhExchgTime VhPrice",
	FUN = as.chron,
	aggregate = vh.tail1)

## same but set arguments through options
## We have also set the format option which is not application and ignored.

vh.options(frDef = "VhTrade",  
	startTime = "20051201 09:30:00", 
	endTime = "20051201 16:00:00", 
	fieldNames = "VhExchgTime VhPrice",
	FUN = as.chron,
	aggregate = vh.tail1,
	format = "\%Y\%m\%d")


# Assume there is a csv formatted file called GOOG.csv
# with yyyymmdd dates and assume the vh.options is still as above.
# Since only options format, FUN and aggregate 
# apply to read.zoo only those will be used.
# Thus by setting options appropriate the same script may work
# with files and Vhayu.
vh.options(demo = TRUE)
goog.file <- goog <- vh.get.zoo("GOOG")

}
}
\keyword{ ts }
