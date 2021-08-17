\name{w.window}

  \alias{w.window} 
  \alias{w.window.settitle} 
  \alias{w.window.dispose} 
  \alias{w.window.focus} 
  \alias{w.assign}
  \alias{w.menu}
  \alias{w.notebook} 
  \alias{w.notebook.add} 
  \alias{w.notebook.raise}
  \alias{w.grid}
  \alias{w.button}
  \alias{w.checkbox}
  \alias{w.text} 
  \alias{w.text.change}
  \alias{w.doublebutton}
  \alias{w.slider} 
  \alias{w.slider.change}
  \alias{w.listbox}
  \alias{w.combo}
  \alias{w.radiogroup}
  \alias{w.textentry}
  \alias{w.table} 
  \alias{w.table.value.get} 
  \alias{w.table.value.set} 
  \alias{w.table.location} 
  \alias{w.table.get}
  \alias{w.image} 
  \alias{w.line} 
  \alias{w.deleteline} 
  \alias{w.deletelinebyid} 
  \alias{w.clearlines}
  \alias{w.tkrplot} 
  \alias{w.tkrreplot}
  \alias{w.widget.dispose}
  \alias{w.timer}

\title{w.window}

\description{
New programmatical style functions with improved speed and memory usage. See the demos for each function and note 
the similarities and differences from the existing rp. functions. Important in this is the concept of keeping variables
in the rp environment, not in the panel. Indeed callback functions are provided with the changed value, not the panel
with a changed value encapsulated in it - a concept which should be familiar to many programmers.
}

\usage{
w.window(title="", size=c(100,100), background=NULL, ftype="new", ...)
w.window.settitle(panel, title)
w.window.dispose(panel)
w.window.focus(panel)
w.assign(...)
w.menu(panel, labels, action=I, foreground=NULL, background=NULL, font=NULL) 
w.notebook(parent, width=NULL, height=NULL, pos=NULL, foreground=NULL, background="lightgray", font=NULL)
w.notebook.add(parent, label)
w.notebook.raise(parent, label)
w.grid(parent, pos=NULL, background=NULL)

w.button(parent, action=I, title=deparse(substitute(action)), repeatdelay=0, repeatinterval=0, pos=NULL, 
  foreground=NULL, background=NULL, font=NULL)
w.checkbox(parent, action=I, title, labels, initval=rep(FALSE, length(labels)), pos=NULL, foreground=NULL, 
  background="white", font=NULL)
w.text(parent, text, action=NULL, pos=NULL, foreground=NULL, background=NULL, font=NULL, width=NULL)
w.text.change(widget, text)
w.doublebutton(parent, step, title, action=I, initval, range=c(NA, NA), 
  log=FALSE, showvalue=FALSE, showvaluewidth=4, repeatinterval=100, repeatdelay=100, pos="left", 
  foreground=NULL, background=NULL, font=NULL) 
w.slider(parent, initval, from, to, action=I, title=NULL, label = deparse(substitute(initval)), log=FALSE, 
 showvalue=FALSE, showvaluewidth=4, resolution=0, pos=NULL, horizontal=TRUE, foreground=NULL, background=NULL, font=NULL)
w.listbox(parent, title=NA, labels, rows=length(labels), initval=labels[1], action=I, pos=NULL, 
  foreground=NULL, background="white", font=NULL)
w.combo(parent, prompt=NULL, values, pos=NULL, action=I, foreground=NULL, background=NULL, font=NULL, editable=FALSE)
w.radiogroup(parent, title, labels, initval=labels[1], action=I, pos=NULL, foreground=NULL, 
  background="white", font=NULL)
w.textentry(parent, label, text, action=I, pos=NULL, foreground=NULL, background=NULL, font=NULL, 
  width=20, keydown=FALSE)

w.table(parent, dat, action_click=NULL, action_press=NULL, action_return=NULL, width=400, height=250, 
  ncols=length(dimnames(dat)[[2]])+1, nrows=length(dimnames(dat)[[1]])+1, colwidth=10,
  foreground=NULL, background=NULL, font=NULL, pos=NULL)
w.table.get(widget)
w.table.value.get(widget, x, y)
w.table.value.set(widget, x, y, value)
w.table.location(widget)

w.image(parent, filename, action=NA, mousedrag=NA, mouseup=NA, pos=NULL)
w.line(canvas, x1, y1, x2, y2, color="black", width=2, id="rpline") 
w.deleteline(line) 
w.deletelinebyid(image, id) 
w.clearlines(image)

w.tkrplot(parent, plotfun, action=NA, mousedrag=NA, mouseup=NA, hscale=1, vscale=1, pos=NULL, foreground=NULL, 
  margins=c(0,0,0,0), name=paste("plot", .nc(), sep=""), mar)
w.tkrreplot(img) 

w.widget.dispose(widget)
w.timer(microseconds, action, where) 
}

\arguments{
  \item{parent}{this can be a window, grid, notebook etc.}
  \item{title}{the label of a widget.}
  \item{size}{width and height of window in pixels.}
  \item{background}{background colour of the widget, e.g. "white".}
  \item{ftype}{Internal use only - whether to put parameters into the list or into the environment.}
  \item{\dots}{additional arguments which are treated as variable initialisations and are stored within the .rpenv - unless ftype is altered.}
  \item{panel}{the panel window - for title setting etc.}
  \item{labels}{labels for a widget, e.g. a listbox.}
  \item{action}{function to be called on activation of the widget.}
  \item{foreground}{text color for the widget, e.g. "black".}
  \item{font}{font for the text, e.g. "Arial".}
  \item{width}{width in pixels.}
  \item{height}{height in pixels.}
  \item{pos}{the layout instructions. Please see the \code{\link{rp.pos}} example and help for full details.}
  \item{label}{for raising a particular tab in a notebook.}
  \item{repeatdelay}{microseconds before events when a button is held down.}
  \item{repeatinterval}{microseconds between events when a button is held down.}
  \item{initval}{initial value for a \code{variable}.}
  \item{text}{text for a text label.}
  \item{widget}{the widget which is to be altered.}
  \item{step}{step for a variable alteration.}
  \item{range}{range over which the widget's variable is altered.}
  \item{log}{whether the widget's variable range is logarithmic - default is not logarithmic.}
  \item{showvalue}{whether to show the value of the widget's variable.}
  \item{showvaluewidth}{width, in pixels of the shown the value of the widget's variable - very useful to stop number "jumping" during alteration.}
  \item{from}{lower value of widget's variable range.}
  \item{to}{upper value of widget's variable range.}
  \item{resolution}{resolution of a slider.}
  \item{horizontal}{whether a slider is shown horizontally (default).}
  \item{rows}{number of rows shown in a listbox (slider created if number of rows is less than number of entries).}
  \item{prompt}{prompt for the widget.}
  \item{values}{values shown in a listbox etc.}
  \item{editable}{whether a combobox is editable.}
  \item{keydown}{event on a keydown.}
  \item{dat}{data for a widget, like initval, but allows matrices.}
  \item{action_click}{event for a click on the widget.}
  \item{action_press}{event for a keypress on the widget.}
  \item{action_return}{event when a return is pressed on a widget.}
  \item{ncols}{number of columns on a table.}
  \item{nrows}{number of rows on a table.}
  \item{colwidth}{width of columns in pixels.}
  \item{x}{column number.}
  \item{y}{row number.}
  \item{value}{value to set to a cell.}
  \item{filename}{filename for an image.}
  \item{mousedrag}{event when a mouse is dragged over an image.}
  \item{mouseup}{event when a mouse is released from an image.}
  \item{canvas}{picture for a line to be drawn on.}
  \item{x1}{for drawing a line.}
  \item{y1}{for drawing a line.}
  \item{x2}{for drawing a line.}
  \item{y2}{for drawing a line.}
  \item{color}{for drawing a line.}
  \item{id}{a tag for a line - for optional later removal of the line.}
  \item{line}{to identify the line.}
  \item{image}{to identify the image.}
  \item{plotfun}{the function which generates a plot.}
  \item{hscale}{to expand/reduce a plot.}
  \item{vscale}{to expand/reduce a plot.}
  \item{margins}{margins around a plot.}
  \item{name}{name for a plot.}
  \item{img}{background image for a plot, transparent is ideal.}
  \item{microseconds}{time between timer ticks on a repeating event.}
  \item{where}{conditional statement to keep a timer running.}  
  \item{mar}{margins round a plot.}
}

\note{
New functions for version 2.0.
}

\details{
  These are the new functions - see the demos for each of them.
}

\keyword{iplot}
\keyword{dynamic}
