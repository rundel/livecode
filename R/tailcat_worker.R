# The tailcat broadcast runs in its own R process (see tailcat_broadcast.R),
# so a crash of tailcat's embedded Go runtime, or a flood of connections,
# costs the broadcast and not the instructor's session.

# The set of follower connections and the send policy. Connections are only
# ever written to, never read: a follower has nothing to say, and a read
# would be a way in. A follower that is still busy with the previous write
# when the next one is due, or whose write fails or times out, is dropped. A
# timed out write may have sent part of a line, so the stream is not usable
# afterwards anyway. A dropped follower reconnects and gets the full state.
tc_hub = R6::R6Class(
  "livecode_tailcat_hub",
  cloneable = FALSE,
  public = list(
    initialize = function(max_clients = 150L, write_timeout = tc_write_timeout,
                          on_change = function() NULL) {
      private$max_clients = max_clients
      private$write_timeout = write_timeout
      private$on_change = on_change
      private$clients = new.env(parent = emptyenv())
    },

    add = function(conn, greeting) {
      if (self$n() >= private$max_clients) {
        tc_close_quietly(conn)
        private$refused = private$refused + 1L
        return(invisible(FALSE))
      }
      private$next_id = private$next_id + 1L
      id = paste0("c", private$next_id)
      private$clients[[id]] = list(conn = conn, pending = FALSE)
      private$on_change()
      private$send_to(id, greeting)
      invisible(TRUE)
    },

    send = function(data) {
      for (id in ls(private$clients))
        private$send_to(id, data)
      invisible(self)
    },

    n = function() {
      length(ls(private$clients))
    },

    n_pending = function() {
      sum(vapply(ls(private$clients), function(id) private$clients[[id]]$pending, logical(1)))
    },

    n_refused = function() {
      private$refused
    },

    n_dropped = function() {
      private$dropped
    },

    close_all = function() {
      for (id in ls(private$clients))
        private$remove(id)
      invisible(self)
    }
  ),

  private = list(
    clients = NULL,
    max_clients = 150L,
    write_timeout = 5,
    on_change = NULL,
    next_id = 0L,
    refused = 0L,
    dropped = 0L,

    send_to = function(id, data) {
      client = private$clients[[id]]
      if (is.null(client))
        return(invisible())
      if (isTRUE(client$pending)) {
        private$drop(id)
        return(invisible())
      }

      p = tryCatch(
        client$conn$write_async(data, timeout = private$write_timeout),
        error = function(e) NULL
      )
      if (is.null(p)) {
        private$drop(id)
        return(invisible())
      }

      private$clients[[id]]$pending = TRUE
      promises::then(
        p,
        onFulfilled = function(n) {
          if (!is.null(private$clients[[id]]))
            private$clients[[id]]$pending = FALSE
        },
        onRejected = function(e) private$drop(id)
      )
      invisible()
    },

    drop = function(id) {
      if (is.null(private$clients[[id]]))
        return(invisible())
      private$dropped = private$dropped + 1L
      private$remove(id)
    },

    remove = function(id) {
      client = private$clients[[id]]
      if (is.null(client))
        return(invisible())
      rm(list = id, envir = private$clients)
      tc_close_quietly(client$conn)
      private$on_change()
      invisible()
    }
  )
)

tc_close_quietly = function(conn) {
  try(conn$close(), silent = TRUE)
  invisible()
}

# The broadcast's view of the session: what a follower needs on connect.
tc_state = function() {
  state = new.env(parent = emptyenv())
  state$content = ""
  state$selection = ""
  state$sticky = NULL
  state
}

# Applies spool frames to the state and returns the line to send to the
# connected followers, or NULL when there is nothing to send.
tc_apply_frames = function(state, frames) {
  out = list()
  msgs = list()
  for (f in frames) {
    if (!is.null(f[["content"]])) {
      state$content = f[["content"]]
      out[["content"]] = f[["content"]]
    }
    if (!is.null(f[["selection"]])) {
      state$selection = f[["selection"]]
      out[["selection"]] = f[["selection"]]
    }
    msgs = c(msgs, f[["messages"]])
    if (!is.null(f[["sticky"]])) {
      state$sticky = f[["sticky"]]
      msgs = c(msgs, list(f[["sticky"]]))
    }
  }
  if (length(msgs) > 0)
    out[["messages"]] = msgs
  if (length(out) == 0)
    return(NULL)
  tc_encode(out)
}

# The hello line and the full current state, for a follower that has just
# connected or reconnected.
tc_greeting = function(hello, state) {
  frame = list(content = state$content, selection = state$selection)
  if (!is.null(state$sticky))
    frame[["messages"]] = list(state$sticky)
  paste0(tc_encode(hello), tc_encode(frame))
}

# Spool directory shared by the parent and the worker. It and every file in
# it are readable by the owner only: the status file holds the address.

tc_spool_create = function() {
  dir = tempfile("livecode-tailcat-")
  old = Sys.umask("077")
  on.exit(Sys.umask(old), add = TRUE)
  dir.create(dir, mode = "0700")
  Sys.chmod(dir, "0700", use_umask = FALSE)
  dir
}

# Written to a temporary name and renamed, so a reader never sees a partial
# file.
tc_spool_write = function(dir, name, text) {
  old = Sys.umask("077")
  on.exit(Sys.umask(old), add = TRUE)
  tmp = file.path(dir, paste0(".tmp-", name))
  con = file(tmp, open = "wb")
  writeBin(charToRaw(enc2utf8(text)), con)
  close(con)
  Sys.chmod(tmp, "0600", use_umask = FALSE)
  file.rename(tmp, file.path(dir, name))
}

tc_spool_frame_name = function(seq) {
  paste0("frame-", formatC(seq, width = 12, format = "d", flag = "0"), ".json")
}

# Reads, deletes, and returns the pending frames in order.
tc_spool_take = function(dir) {
  files = sort(list.files(dir, pattern = "^frame-[0-9]+\\.json$"))
  lapply(files, function(f) {
    path = file.path(dir, f)
    frame = tryCatch(
      jsonlite::fromJSON(path, simplifyVector = FALSE),
      error = function(e) list()
    )
    unlink(path)
    frame
  })
}

tc_status_write = function(dir, status) {
  tc_spool_write(dir, "status.json", jsonlite::toJSON(status, auto_unbox = TRUE, null = "null"))
}

tc_status_read = function(dir) {
  path = file.path(dir, "status.json")
  if (!file.exists(path))
    return(NULL)
  tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(e) NULL)
}

tc_parent_alive = function(handle) {
  if (is.null(handle))
    return(TRUE)
  isTRUE(tryCatch(ps::ps_is_running(handle), error = function(e) FALSE))
}

# The worker's main loop, run in the child process. It exits when asked to
# (a stop file), when the parent process is gone, or when the spool
# directory is removed.
tailcat_worker_run = function(dir, file, lang, interval, port = tc_default_port,
                              max_clients = 150L, parent_pid = NULL, poll = 0.1) {
  Sys.umask("077")
  parent = if (is.null(parent_pid)) NULL else ps::ps_handle(as.integer(parent_pid))

  st = new.env(parent = emptyenv())
  st$status = list(state = "starting", address = NULL, clients = 0L, dropped = 0L, error = NULL)
  update_status = function(...) {
    st$status[names(list(...))] = list(...)
    if (dir.exists(dir))
      tc_status_write(dir, st$status)
  }
  update_status()

  key_file = file.path(dir, "keys.rds")
  key = NULL
  if (file.exists(key_file)) {
    keys = readRDS(key_file)
    unlink(key_file)
    key = structure(keys[c("private_key", "public_key", "preshared_key")], class = "tailcat_keys")
  }

  srv = NULL
  lst = NULL
  hub = NULL
  on.exit({
    if (!is.null(hub)) hub$close_all()
    if (!is.null(lst)) try(lst$close(), silent = TRUE)
    if (!is.null(srv)) {
      try(srv$drain(timeout = 1), silent = TRUE)
      try(srv$close(), silent = TRUE)
    }
  }, add = TRUE)

  result = tryCatch(
    {
      srv = rtailcat::TailcatServer$new(key = key)
      key = NULL
      lst = srv$listen(port, timeout = 60)

      state = tc_state()
      hello = tc_hello(file, lang, interval)
      hub = tc_hub$new(
        max_clients = max_clients,
        on_change = function() update_status(clients = hub$n(), dropped = hub$n_dropped())
      )
      lst$serve(
        function(conn) {
          hub$add(conn, tc_greeting(hello, state))
          NULL
        },
        on_error = function(cond, conn) {
          if (!is.null(conn)) tc_close_quietly(conn)
        }
      )

      update_status(state = "ready", address = as.character(srv$address))

      last_sent = Sys.time()
      last_check = Sys.time()
      repeat {
        later::run_now(poll)

        line = tc_apply_frames(state, tc_spool_take(dir))
        now = Sys.time()
        if (!is.null(line)) {
          hub$send(line)
          last_sent = now
        } else if (difftime(now, last_sent, units = "secs") >= tc_ping_interval) {
          hub$send(tc_encode(list(ping = TRUE)))
          last_sent = now
        }

        if (difftime(now, last_check, units = "secs") >= 1) {
          last_check = now
          if (!dir.exists(dir) || !tc_parent_alive(parent))
            break
          if (file.exists(file.path(dir, "stop"))) {
            # Give the last frames, such as a shutdown message, a moment.
            final = tc_apply_frames(state, tc_spool_take(dir))
            if (!is.null(final))
              hub$send(final)
            deadline = Sys.time() + 1
            while (hub$n_pending() > 0 && Sys.time() < deadline)
              later::run_now(0.05)
            break
          }
        }
      }
      "stopped"
    },
    error = function(e) {
      update_status(state = "failed", error = conditionMessage(e))
      "failed"
    }
  )

  if (identical(result, "stopped"))
    update_status(state = "stopped", address = NULL, clients = 0L)
  invisible(result)
}
