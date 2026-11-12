.onUnload = function(libpath) {
  for (server in .globals$servers) {
    try(suppressMessages(server$stop()), silent = TRUE)
  }
  for (follower in .globals$followers) {
    try(suppressMessages(follower$stop()), silent = TRUE)
  }
}
