\name{rp.table}

\alias{rp.table}
\alias{rp.table.value.get}
\alias{rp.table.value.set}
\alias{rp.table.location}
\alias{rp.table.get}

\title{Tables/spreadsheet support for rpanel}

\description{
  Add a table to the panel.}

\usage{
rp.table(panel, variable, initval, action_click=NULL, action_press=NULL, action_return=NULL, width=400, height= 250, 
  ncols=NULL, nrows=NULL, colwidth=10, foreground=NULL, background=NULL, font=NULL, pos=NULL,
  parentname=deparse(substitute(panel)), name=paste("table", .nc(), sep=""), ...)
rp.table.value.get(panel, name, x, y)
rp.table.value.set(panel, name, y, x, value)
rp.table.location(panel, name)
rp.table.get(panel, name)
}

\arguments{
  \item{panel}{the panel in which the table appears.}
  \item{variable}{the name of the variable that supplies the content of the table.}
  \item{initval}{the initial value of variable (optional). The initial value can also 
    be specified in the call to \code{rp.control}.}
  \item{action_click}{function called on click.}
  \item{action_press}{function called on a keypress.}
  \item{action_return}{function called on pressing return (note that cells do allow multiple lines in cells).}
  \item{width}{width in pixels.}
  \item{height}{height in pixels.}
  \item{ncols}{number of columns displayed (scrollbars are automatically generated).}
  \item{nrows}{number of rows displayed (scrollbars are automatically generated).}
  \item{colwidth}{width of a column in pixels.}
  \item{foreground}{colour of the text}
  \item{background}{colour of the text background}
  \item{font}{font to be used}
  \item{pos}{the layout instructions. Please see the \code{\link{rp.pos}} example 
    and help for full details.}  
  \item{parentname}{this specifies the widget inside which the table should 
    appear.}
  \item{name}{name assigned to the table, used for disposing of the widget}
  \item{...}{...}
  \item{x}{column}
  \item{y}{row}
  \item{value}{new value for cell}
}

\note{
New functions for version 2.0.
}

\details{
\code{rp.table} creates the table.
\code{rp.table.value.get} picks up the content of a specific cell.
\code{rp.table.value.set} sets up the content of a specific cell.
\code{rp.table.location} gives the location of the currently selected cell.
\code{rp.table.get} returns the entire table as a matrix.

  The function \code{action_click}, \code{action_press} and \code{action_return} 
  should take one argument, which should be the panel to which the slider is attached. 
  See \code{\link{rp.grid}} for details of the grid layout system.
}

\section{Warning}{
  The \code{action} functions should return the panel. 
  Without this assignment any widgets added or alterations made to panel parameters within 
  the \code{action} functions will be lost.
}

\references{
   rpanel: Simple interactive controls for R functions using the tcltk package.
      Journal of Statistical Software, 17, issue 9.
   }

\examples{
\dontrun{
panel <- rp.control()
rp.table(
  panel=panel,
  theexampletable, 
  matrix(
    c(1,2,3, 11,12,13, 4,5,6, 7,8,9), nrow=4, ncol=3, byrow=TRUE, 
    dimnames=list(c("row1", "row2", "row3", "row4"), c("C.1", "C.2", "C.3"))),
  action_click=function(panel) { print("click"); print(panel$theexampletable); panel },
  action_press=function(panel) { print("press"); print(panel$theexampletable); panel },
  action_return=function(panel) { print("return"); print(panel$theexampletable); panel },
  name="exampletable")
print(rp.table.value.get(panel, "exampletable", 1, 2))
rp.table.value.set(panel, "exampletable", 1,2,10)
print(rp.table.value.get(panel, "exampletable", 1, 2))
print(rp.table.get(panel, "exampletable"))
}}

\keyword{iplot}
\keyword{dynamic}
