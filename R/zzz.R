.onUnload = function(libpath) {
  for (server in .globals$servers) {
    try(server$stop(), silent = TRUE)
  }
}
