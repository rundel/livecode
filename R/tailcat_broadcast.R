# Checked without loading the package, so the instructor's session does not
# load rtailcat's Go runtime: only the broadcast process does.
pkg_available = function(pkg) {
  nzchar(system.file(package = pkg))
}

tc_check_installed = function(pkgs, what, call = parent.frame()) {
  missing = pkgs[!vapply(pkgs, pkg_available, logical(1))]
  if (length(missing) == 0)
    return(invisible(TRUE))

  bullets = c("x" = paste0(what, " needs the {.pkg {missing}} package{?s}."))
  if ("rtailcat" %in% missing) {
    bullets = c(
      bullets,
      "i" = "Install rtailcat with {.code pak::pak(\"rundel/rtailcat\")}.",
      "i" = "Building rtailcat needs a Go toolchain, see {.url https://go.dev/doc/install}."
    )
  }
  others = setdiff(missing, "rtailcat")
  if (length(others) > 0)
    bullets = c(bullets, "i" = "Install {.pkg {others}} with {.code install.packages()}.")

  cli::cli_abort(bullets, class = "livecode_missing_package", call = call)
}

tc_broadcast_packages = function() {
  c("rtailcat", "callr", "promises", "ps")
}

# The worker is started with callr, which runs the function in a fresh R
# process. Under pkgload (devtools::load_all() and devtools::test()) the
# package is not installed, or an older copy is, so the worker loads the
# same source tree instead.
tc_dev_path = function() {
  if (!isNamespaceLoaded("pkgload") || !pkgload::is_dev_package("livecode"))
    return(NULL)
  getNamespaceInfo(asNamespace("livecode"), "path")
}

tc_worker_entry = function(pkg_path, args) {
  if (!is.null(pkg_path))
    pkgload::load_all(pkg_path, quiet = TRUE, helpers = FALSE, export_all = FALSE)
  run = get("tailcat_worker_run", envir = asNamespace("livecode"))
  do.call(run, args)
}

# Keys accepted for a broadcast, as the plain list the worker reads.
tc_key_list = function(key, call = parent.frame()) {
  if (inherits(key, "livecode_tailcat_keys") || inherits(key, "tailcat_keys")) {
    parts = lapply(unclass(key)[c("private_key", "public_key", "preshared_key")], function(x) {
      if (is.null(x)) NA_character_ else as.character(unclass(x))
    })
    if (all(vapply(parts, function(x) length(x) == 1 && !is.na(x) && nzchar(x), logical(1))))
      return(parts)
  }
  cli::cli_abort(
    c(
      "{.arg tailcat} must be {.code TRUE}, {.code FALSE}, or a set of keys.",
      "i" = "Create keys with {.fn livecode::tailcat_keys} or {.fn rtailcat::tailcat_keygen}."
    ),
    call = call
  )
}

# Parent side of a tailcat broadcast: starts the worker process, writes each
# frame to the spool directory for it, and reads its status.
tc_broadcast = R6::R6Class(
  "livecode_tailcat_broadcast",
  cloneable = FALSE,
  public = list(
    initialize = function(file, lang, interval, key = NULL, port = tc_default_port,
                          max_clients = 150L, timeout = 60) {
      private$dir = tc_spool_create()

      if (!is.null(key)) {
        old = Sys.umask("077")
        key_file = file.path(private$dir, "keys.rds")
        saveRDS(key, key_file)
        Sys.chmod(key_file, "0600", use_umask = FALSE)
        Sys.umask(old)
      }

      args = list(
        dir = private$dir, file = basename(file), lang = lang, interval = interval,
        port = as.integer(port), max_clients = as.integer(max_clients),
        parent_pid = Sys.getpid()
      )
      log = file.path(private$dir, "worker.log")
      private$proc = callr::r_bg(
        tc_worker_entry,
        args = list(pkg_path = tc_dev_path(), args = args),
        stdout = log, stderr = "2>&1",
        supervise = TRUE
      )

      private$wait_ready(timeout)
    },

    send = function(frame) {
      if (!self$is_running())
        cli::cli_abort("The tailcat broadcast process has stopped, restart it with {.code $start_tailcat()}.")

      frame = tc_spool_frame(frame)
      if (length(frame) == 0)
        return(invisible(self))

      private$seq = private$seq + 1
      json = jsonlite::toJSON(frame, auto_unbox = TRUE)
      tc_spool_write(private$dir, tc_spool_frame_name(private$seq), json)
      invisible(self)
    },

    is_running = function() {
      !is.null(private$proc) && private$proc$is_alive() &&
        identical(self$status()[["state"]], "ready")
    },

    status = function() {
      if (is.null(private$dir))
        return(NULL)
      tc_status_read(private$dir)
    },

    address = function() {
      if (!self$is_running())
        return(NULL)
      self$status()[["address"]]
    },

    worker_pid = function() {
      if (is.null(private$proc)) NULL else private$proc$get_pid()
    },

    n_clients = function() {
      if (!self$is_running())
        return(0L)
      as.integer(self$status()[["clients"]])
    },

    stop = function(wait = 3) {
      if (!is.null(private$proc)) {
        if (private$proc$is_alive() && dir.exists(private$dir)) {
          writeLines("", file.path(private$dir, "stop"))
          deadline = Sys.time() + wait
          while (private$proc$is_alive() && Sys.time() < deadline)
            Sys.sleep(0.05)
        }
        if (private$proc$is_alive())
          private$proc$kill()
        private$proc = NULL
      }
      if (!is.null(private$dir)) {
        unlink(private$dir, recursive = TRUE)
        private$dir = NULL
      }
      invisible(self)
    }
  ),

  private = list(
    dir = NULL,
    proc = NULL,
    seq = 0,

    wait_ready = function(timeout) {
      deadline = Sys.time() + timeout
      repeat {
        status = self$status()
        state = status[["state"]]
        if (identical(state, "ready"))
          return(invisible(TRUE))

        if (identical(state, "failed") || !private$proc$is_alive() || Sys.time() > deadline) {
          reason = if (identical(state, "failed")) {
            status[["error"]]
          } else if (!private$proc$is_alive()) {
            # Nothing in the log holds the address: it only exists once the
            # worker is ready.
            log = utils::tail(readLines(file.path(private$dir, "worker.log"), warn = FALSE), 5)
            paste(c("The broadcast process exited.", log), collapse = "\n")
          } else {
            paste0("No address after ", timeout, " seconds.")
          }
          self$stop(wait = 0)
          cli::cli_abort(c("Could not start the tailcat broadcast.", "i" = "{reason}"))
        }
        Sys.sleep(0.1)
      }
    }
  )
)
