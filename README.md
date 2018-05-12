# import-cost (pre-alpha)
[![Build Status](https://travis-ci.org/madeleinedaly/import-cost.svg?branch=master)](https://travis-ci.org/madeleinedaly/import-cost)

Minor mode for displaying JavaScript module sizes inline.

This is an Emacs port of the [Import Cost](https://github.com/wix/import-cost/tree/master/packages/vscode-import-cost) plugin for Visual Studio Code.

## Requirements

- `emacs >=24.4`
- `node >=8.0.0`

## Usage

You can enable `import-cost-mode` for the current buffer interactively with `M-x import-cost-mode`.

Or hook into your preferred JavaScript modes:

``` emacs-lisp
(add-hook 'js2-mode-hook #'import-cost-mode)
(add-hook 'rjsx-mode-hook #'import-cost-mode)
```

## Configuration

The following variables are customizable (see `M-x customize-group import-cost`):

### UI options

#### `import-cost-small-package-size` : number

Upper size limit, in KB, that will count a package as a small package.
* Default: `50`

#### `import-cost-medium-package-size` : number

Upper size limit, in KB, that will count a package as a medium package. Any package size above this limit will be considered large.

* Default: `100`

#### `import-cost-small-package-color` : color|face

Decoration color for small packages.

* Default: `"#7cc36e"`

#### `import-cost-medium-package-color` : color|face

Decoration color for medium packages.

* Default: `"#7cc36e"`

#### `import-cost-large-package-color` : color|face

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

### Environment settings

#### `import-cost-node-executable` : string

The path or name of the `node` binary that will be used to run this package's Node.js subprocess.

* Default: `"node"`

#### `import-cost-npm-executable` : string

The path or name of the `npm` binary that will be used to install this package's JavaScript dependencies.

* Default: `"npm"`

#### `import-cost-cache-filename` : string

The name of the file in which import size decoration data gets persisted across sessions.

* Default: `"import-cost.cache"`

#### `import-cost-cache-filepath` : string

The path to the file where import size decoration data gets persisted across sessions.

* Default: `user-emacs-directory`

## License

MIT
