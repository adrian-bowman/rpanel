w.table <- function(parent, dat, action_click=NULL, action_press=NULL, action_return=NULL, width=400, height=250, 
  ncols=length(dimnames(dat)[[2]])+1, nrows=length(dimnames(dat)[[1]])+1, colwidth=10,
  foreground=NULL, background=NULL, font=NULL, pos=NULL)
{
  widget <- w.createwidget(parent, pos, background)
  widget$.type = "table"

# table preparations
  handshake(tclRequire, "Tktable")
  titles <- dimnames(dat)[[2]]
  rows <- dimnames(dat)[[1]]

  widget$.ncols <- length(titles)
  widget$.nrows <- length(rows)
  
  widget$.tclarray <- handshake(tclArray)

  for(i in 1:length(titles)) widget$.tclarray[[0,i]] <- titles[i]
  for(j in 1:length(rows)) widget$.tclarray[[j,0]] <- rows[j]
  for (i in (1:length(titles))) for (j in (1:length(rows))) { widget$.tclarray[[j,i]] <- dat[[j,i]] }

  widget$.widget <- handshake(tkwidget, parent$.handle, "table", variable=widget$.tclarray, rows=nrows,
    cols=ncols, titlerows=1, titlecols=1, selectmode="extended", colwidth=colwidth, background="white",
    maxheight = height, maxwidth = width,
    xscrollcommand = function(...) handshake(tkset, xscr, ...),
    yscrollcommand = function(...) handshake(tkset, yscr, ...))
  xscr <- handshake(tkscrollbar, parent$.handle, orient = "horizontal", command = function(...) handshake(tkxview, widget$.widget, ...))
  yscr <- handshake(tkscrollbar, parent$.handle, command = function(...) handshake(tkyview, widget$.widget, ...))
  widget$.xscr <- list(xscr)
  widget$.yscr <- list(yscr)

  f_click <- function(...) { action_click() }
  f_press <- function(...) { action_press() }
  f_return <- function(...) { action_return() }

  if (is.function(action_click)) { handshake(tkbind, widget$.widget, "<Button-1>", f_click) }
  if (is.function(action_press)) { handshake(tkbind, widget$.widget, "<ButtonRelease-1>", f_press) }
  if (is.function(action_return)) { handshake(tkbind, widget$.widget, "<Return>", f_return) }

  handshake(tcl, widget$.widget,"width",0,5) # set width of left hand column (number 0)

  w.appearancewidget(widget, font, foreground, background)

  handshake(tkgrid, widget$.widget, yscr, "in"=widget$.handle)
  handshake(tkgrid.configure, yscr, sticky="nsw", "in"=widget$.handle)
  handshake(tkgrid, xscr, sticky="new", "in"=widget$.handle)

  handshake(tkactivate, widget$.widget, "0,0")
  handshake(tktag.configure, widget$.widget, "active", background="lightgray", foreground="navy")

  invisible(widget)
}

w.table.get <- function(widget)
{
  tab <- matrix(ncol=widget$.ncols, nrow=widget$.nrows)
  for (i in (1:widget$.nrows)) 
  {    
    for (j in (1:widget$.ncols)) 
    { 
      tab[i,j] <- handshake(tclvalue, widget$.tclarray[[i,j]])
    }
  }
  tab
}

w.table.value.get <- function(widget, x, y)
{
  handshake(tclvalue, widget$.tclarray[[y,x]])
}

w.table.value.set <- function(widget, x, y, value)
{
  widget$.tclarray[[y,x]] = value
}

w.table.location <- function(widget)
{
  location <- handshake(tclvalue, handshake(tcl, widget$.widget,"curselection"))
  splitloc <- strsplit(location,",")
  c(as.integer(splitloc[[1]][[2]]), as.integer(splitloc[[1]][[1]]))
}

rp.table <- function(panel, variable, initval, action_click=NULL, action_press=NULL, action_return=NULL, width=400, height= 250, 
  ncols=NULL, nrows=NULL, colwidth=10, foreground=NULL, background=NULL, font=NULL, pos=NULL,
  parentname=deparse(substitute(panel)), name=paste("table", .nc(), sep=""), ...)
{
  if (is.na(charmatch("window", panel$panelname))) # if the panelname is not set then
  { 
    panelname = deparse(substitute(panel)) # the panel name should be the panel deparse subst'ed
# 13/03/2012 these lines are not commented out in previous version
#    panel <- rp.control.get(panelname, panel) # now get the panel
#    panel$panelname = panelname # now set the panelname properly
#    assign(panelname, panel, envir=.rpenv) # now send back the panel
  } 
  else 
  { 
    panelname = panel$panelname 
# 13/03/2012 these lines are not commented out in previous version
#    panel <- rp.control.get(panelname, panel) # now get the panel
  }
  
  varname = deparse(substitute(variable))
  if (!rp.isnull(panelname, varname)) { variable = rp.var.get(panelname, varname) } 
  else { variable = initval } 
  
  if (is.null(pos)) { if (length(list(...))>0) { pos <- list(...) } }
  if (is.list(pos)) { if (!is.null(pos$grid)) { parent <- rp.widget.get(panelname, pos$grid) } }

  if (is.null(ncols)) { ncols=length(dimnames(variable)[[2]])+1 }
  if (is.null(nrows)) { nrows=length(dimnames(variable)[[1]])+1 }

  fc <- function()
  {
    rp.matrix.put(panelname, varname, w.table.get(widget), widget$.ncols, widget$.nrows)
# 13/03/2012 this next line was commented out in the previous version
    panel <- rp.control.get(panelname)
    panel <- action_click(panel)
    rp.control.put(panelname, panel)
  }
  
  fp <- function()
  {
    rp.matrix.put(panelname, varname, w.table.get(widget), widget$.ncols, widget$.nrows)
# 13/03/2012 this next line was commented out in the previous version
    panel <- rp.control.get(panelname)
    panel <- action_press(panel)
    rp.control.put(panelname, panel)
  }

  fr <- function()
  {
    rp.matrix.put(panelname, varname, w.table.get(widget), widget$.ncols, widget$.nrows)
# 13/03/2012 this next line was commented out in the previous version
    panel <- rp.control.get(panelname, panel)
    panel <- action_return(panel)
    rp.control.put(panelname, panel)
  }
  
  if (rp.widget.exists(panelname, parentname)) { parent <- rp.widget.get(panelname, parentname) }
  else { parent <- panel }
  
  dat=variable
  
  widget <- w.table(parent, dat=variable, action_click=fc, action_press=fp, action_return=fr, width, height, 
    ncols, nrows, colwidth, foreground=NULL, background=NULL, font=NULL, pos=NULL)  
  rp.widget.put(panelname, name, widget)

  if (.rpenv$savepanel) { rp.control.put(panelname, panel) } # put the panel back into the environment
  invisible(panelname)
}

rp.table.value.get <- function(panel, name, x, y)
{
  panelname <- deparse(substitute(panel))
  w.table.value.get(rp.widget.get(panelname, name), x, y)
}

rp.table.value.set <- function(panel, name, y, x, value)
{
  panelname <- deparse(substitute(panel))
  w.table.value.set(rp.widget.get(panelname, name), x, y, value)
}

rp.table.location <- function(panel, name)
{
  panelname <- deparse(substitute(panel))
  w.table.location(rp.widget.get(panelname, name))
}

rp.table.get <- function(panel, name)
{
  panelname <- deparse(substitute(panel))
  w.table.get(rp.widget.get(panelname, name))
}
