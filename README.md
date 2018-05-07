# import-cost.el [![Build Status](https://travis-ci.org/madeleinedaly/import-cost.el.svg?branch=master)](https://travis-ci.org/madeleinedaly/import-cost.el)

## About

An Emacs port of the [Import Cost](https://github.com/wix/import-cost/) VSCode extension as a minor mode.

## Requirements

- `emacs >=24.4`
- `node >=8.0.0`
- `npm >=5.0.0`

## Configuration

The following variables are customizable:

#### `import-cost-small-package-size`

Upper size limit, in KB, that will count a package as a small package. Default: `50`

#### `import-cost-medium-package-size`

Upper size limit, in KB, that will count a package as a medium package. Any package size above this limit will be considered large. Default: `100`

#### `import-cost-small-package-color`

Decoration color for small packages. Default: `"#7cc36e"`

#### `import-cost-medium-package-color`

Decoration color for medium packages. Default: `"#7cc36e"`

#### `import-cost-large-package-color`

Decoration color for large packages. Default: `"#d44e40"`

#### `import-cost-typescript-extensions`

File extensions to be parsed by the TypeScript parser. Default: `'("\\.tsx?$")`

#### `import-cost-javascript-extensions`

File extensions to be parsed by the JavaScript parser. Default: `'("\\.jsx?$")`

#### `import-cost-bundle-size-decoration`

Which bundle size to display. Possible values: `'both`, `'minified`, and `'gzipped`. Default: `'both`
