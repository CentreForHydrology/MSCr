win.eol <- function(){
if ('win' %in% .Platform$OS.type)
  win.eol <- '\n'
if (!('win' %in% .Platform$OS.type))
  win.eol <- '\r\n'
  return(win.eol)
}
