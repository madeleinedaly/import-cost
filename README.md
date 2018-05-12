# import-cost (pre-alpha) [![Build Status](https://travis-ci.org/madeleinedaly/import-cost.svg?branch=master)](https://travis-ci.org/madeleinedaly/import-cost)

Minor mode for displaying JavaScript module sizes inline.

This is an Emacs port of the [Import Cost](https://github.com/wix/import-cost/tree/master/packages/vscode-import-cost) plugin for Visual Studio Code.

## Requirements

- `emacs >=24.4`
- `node >=8.0.0`

## Installation

### Manually

First clone this project:

``` shellsession
$ git clone git@github.com:madeleinedaly/import-cost.git
```

Then add something like this to your Emacs config:

``` emacs-lisp
(add-to-list 'load-path "path/to/import-cost.el")
(require 'import-cost)

;; you can access config options once the package has loaded:
(with-eval-after-load 'import-cost
  (setq import-cost-bundle-size-decoration 'gzipped))
```

## Usage

You can enable `import-cost-mode` for the current buffer interactively with `M-x import-cost-mode`.

Or hook into your preferred JavaScript modes:

``` emacs-lisp
(add-hook 'js2-mode-hook #'import-cost-mode)
(add-hook 'rjsx-mode-hook #'import-cost-mode)
```

## Configuration

The following variables are customizable (see `M-x customize-group import-cost`):

#### `import-cost-lighter` : string

Lighter used in the mode-line while `import-cost-mode` is active.

Default: `" $"`

#### `import-cost-small-package-size` : number

Upper size limit, in KB, that will count a package as a small package.

* Default: `50`

#### `import-cost-medium-package-size` : number

Upper size limit, in KB, that will count a package as a medium package. Any package size above this limit will be considered large.

* Default: `100`

#### `import-cost-small-package-color` : color | face

Decoration color for small packages.

* Default: `"#7cc36e"`

#### `import-cost-medium-package-color` : color | face

Decoration color for medium packages.

* Default: `"#7cc36e"`

#### `import-cost-large-package-color` : color | face

Decoration color for large packages.

* Default: `"#d44e40"`

#### `import-cost-typescript-extensions` : list (regexp)

File extensions to be parsed by the TypeScript parser.

* Default: `'("\\.tsx?$")`

#### `import-cost-javascript-extensions` : list (regexp)

File extensions to be parsed by the JavaScript parser.

* Default: `'("\\.jsx?$")`

#### `import-cost-bundle-size-decoration` : symbol

Which bundle size to display.

* Default: `'both`

* Possible values:
  * `'both`
  * `'minified`
  * `'gzipped`

#### `import-cost-cache-filename` : string

The name of the file in which import size decoration data gets persisted across sessions.

* Default: `"import-cost.cache"`

#### `import-cost-cache-filepath` : string

The path to the file where import size decoration data gets persisted across sessions.

* Default: `user-emacs-directory`

## License

MIT
