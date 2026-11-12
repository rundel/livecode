# Tests never talk to a real IDE, even when run from inside one. This lives in
# a setup file, not a helper, because devtools::load_all() sources helpers and
# would turn editor detection off in the developer's own session.
withr::local_options(livecode.editor = "none", .local_envir = testthat::teardown_env())
