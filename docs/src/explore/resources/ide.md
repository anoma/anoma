# IDE

## VsCode

Some handy extensions (output of `code --list-extensions`):

```shell
aaron-bond.better-comments
be5invis.toml
bodil.file-browser
bungcip.better-toml
DavidAnson.vscode-markdownlint
jacobdufault.fuzzy-search
kahole.magit
matklad.rust-analyzer
oderwat.indent-rainbow
# easy to see if crates are up-to-date and update if not
serayuzgur.crates
streetsidesoftware.code-spell-checker
vscodevim.vim
# this is like https://www.spacemacs.org/ but in VsCode
VSpaceCode.vspacecode
VSpaceCode.whichkey
# org-mode
vscode-org-mode.org-mode
publicus.org-checkbox
```

Add these to your settings.json to get rustfmt and clippy with the nightly version that we use:

```json
"rust-analyzer.checkOnSave.overrideCommand": [
    "cargo",
    "+{{#include ../../../../rust-nightly-version}}",
    "clippy",
    "--workspace",
    "--message-format=json",
    "--all-targets"
],
"rust-analyzer.rustfmt.overrideCommand": [
    "rustup",
    "run",
    "{{#include ../../../../rust-nightly-version}}",
    "--",
    "rustfmt",
    "--edition",
    "2018",
    "--"
],
```

When editing the wasms source (i.e. `wasm/wasm_source/src/..`), open the `wasm/wasm_source` as a workspace to get rust-analyzer working (because the crate is excluded from the root cargo workspace) and then active `--all-features` for it in the preferences.

## Emacs

two main mode:

- [rust-mode](https://github.com/rust-lang/rust-mode)
  official mode supported by rust dev
- [rustic-mode](https://github.com/brotzeit/rustic)
  forked with more option and better integration/default value

## config example with rustic and use-package

```elisp
    ;; all flycheck not mandatory not mandatory
  (use-package flycheck
    :commands flycheck-mode
    :init (global-flycheck-mode))

  (use-package flycheck-color-mode-line
    :after flycheck
    :hook
    (flycheck-mode . flycheck-color-mode-line-mode))

  (use-package flycheck-pos-tip
    :after flycheck)
  (use-package lsp-mode
    :after flycheck
    :bind-keymap
    ("C-c i" .  lsp-command-map)
    :hook
    (lsp-mode . lsp-enable-which-key-integration) ;; if wichkey installed
    :commands (lsp lsp-deferred)
    :custom
    (lsp-eldoc-render-all t)
    (lsp-idle-delay 0.3)
    )

  (use-package lsp-ui
    :after lsp-mode
    :commands lsp-ui-mode
    :custom
    (lsp-ui-peek-always-show t)
    (lsp-ui-sideline-show-hover t)
    (lsp-ui-doc-enable nil)
    (lsp-ui-doc-max-height 30)
    :hook (lsp-mode . lsp-ui-mode))

    ;; if ivy installed installed
  (use-package lsp-ivy
    :after lsp-mode ivy
    :commands lsp-ivy-workspace-symbol)

    ;; if company installed
  (use-package company-lsp
    :after lsp-mode company
    :init
    (push 'company-lsp company-backend))

  (use-package rustic
    :bind (:map rustic-mode-map
                ("M-j" . lsp-ui-imenu)
                ("M-?" . lsp-find-references)
                ("C-c C-c ?" . lsp-describe-thing-at-point)
                ("C-c C-c !" . lsp-execute-code-action)
                ("C-c C-c r" . lsp-rename)
                ("C-c C-c TAB" . lsp-rust-analyzer-expand-macro)
                ("C-c C-c q" . lsp-workspace-restart)
                ("C-c C-c Q" . lsp-workspace-shutdown)
                ("C-c C-c s" . lsp-rust-analyzer-status)
                ("C-c C-c C-a" . rustic-cargo-add)
                ("C-c C-c C-d" . rustic-cargo-rm)
                ("C-c C-c C-u" . rustic-cargo-upgrade)
                ("C-c C-c C-u" . rustic-cargo-outdated))
    :hook
    (rustic-mode . lsp-deferred)
    :custom
    (lsp-rust-analyzer-cargo-watch-command "clippy")
    :config
    (rustic-doc-mode t)
  )
```
