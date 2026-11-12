# Prism (vendored)

The livecode page highlights the broadcast file with [Prism](https://prismjs.com) 1.29.0, vendored here so the page works without a CDN. Prism is MIT licensed (see `LICENSE`). The Prism files, the qmd grammar, and the code view are taken from the markermd package by the same author, with markermd's names replaced by livecode's.

| File                     | Origin                                                                                          |
|--------------------------|-------------------------------------------------------------------------------------------------|
| `prism-core.min.js`      | Prism 1.29.0, `components/prism-core.min.js`                                                    |
| `components/*.min.js`    | Prism 1.29.0 language components                                                                |
| `LICENSE`                | Prism's MIT license                                                                             |
| `prism-config.js`        | Sets `Prism.manual` before the core loads so Prism does not scan the page                       |
| `livecode-qmd.js`        | The `qmd` grammar (alias `rmd`) for Quarto and R Markdown documents                             |
| `livecode-code-view.js`  | Renders the code view: one element per source line, line numbers, and the selection highlight   |
| `livecode-code-view.css` | Layout, line numbers, the selection band, and the token colors                                  |

No Prism plugins are used. `livecode_page()` in `R/util_code_view.R` writes the script tags, loading the components in the order of `prism_components()`. A component must come after the components it extends (for example `clike` before `javascript`, `yaml` before `markdown`, `markup` before `markdown`). `file_code_language()` in the same file maps a file's extension to the language the page uses.

## Updating Prism

1. Set the new version in `prism_version()` in `R/util_code_view.R`, and in this README.
2. Download `components/prism-core.min.js` and each component in `prism_components()` from `https://cdnjs.cloudflare.com/ajax/libs/prism/<version>/components/`, replacing the files here, and the release's `LICENSE` from the Prism repository.
3. To add a language, add its component name to `prism_components()` (after anything it depends on, per Prism's `components.json`), map file extensions to it in `file_code_language()`, and add chunk engines for it to `ENGINES` in `livecode-qmd.js`.
4. Run the tests: `tests/testthat/test-code-view.R` loads every script under V8 and checks the grammars and the code view helpers, and `test-page.R` checks that the page references only files that exist.
