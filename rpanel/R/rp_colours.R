#     Set standard colours to be used by other rpanel functions

rp.colours <- c(estimate  = '#B3CDE3', estline = '#0093FF',
                reference = '#FBB4AE', refline = '#FF7F00',
                points    = 'grey50',  notch   = 'white',
                density   = 'grey75',  bgdcol  = 'grey85')

# rp.colours <- function(x) {
#    cols <- c(estimate  = '#B3CDE3', estline = '#0093FF',
#              reference = '#FBB4AE', refline = '#FF7F00',
#              points    = 'grey50',  notch   = 'black',
#              density   = 'grey75',  bgdcol  = 'grey85')
#    if (missing(x)) return(cols)
#    if (!is.character(x))
#       stop('x must be a character variable.')
#    ind <- which(names(cols) %in% x)
#    if (length(ind) < length(x))
#       warning('some elements of x are invalid and have been omitted.')
#    return(cols[ind])
# }
