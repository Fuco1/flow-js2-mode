# flow-js2-mode

This package adds support for [Flow](https://flow.org/), a static typechecking extension for javascript, to [js2-mode](https://github.com/mooz/js2-mode), a popular javascript editing mode for Emacs.

It extends the `js2-mode` parser to understand Flow syntax and exposes it in its AST which you can use to build your own refactoring tools.  This understanding of syntax also allows us to add proper syntax highlighting.

This project is heavily WIP at the moment, things might break and change.  Please report all the issues, even minor ones so we can polish this as soon as possible.

# Usage

Enable `flow-js2-mode` in the `js2-mode` buffer.

# List of features (incomplete)

- [x] In object literals, `{foo: bar}` the type is parsed even thought it is not a type.  This breaks fontification and semantics of the object literals.  I don't see a way yet to distinguish if the `foo` is a key or a variable.
- [x] `import type {Config} from 'types/Config'` does not work.
- [x] Typing a destructured object does not work: `function send(res, {body, content}: Response)`
- [x] Parse `function<T>` generic constructs.
  - [x] Does not work with multiple generic arguments (separated by commas)
  - [x] Does not work with typed generics such as `function <T: number>`
- [x] key type specification in objects should work: `clients: { [client_id: string]: Client }`

# Related projects

The [flow-minor-mode](https://github.com/an-sh/flow-minor-mode) project integrates flow features such as navigation to definition, eldoc support for printing types of expressions at point and similar into Emacs.  It is best to use it alongside this package.  In fact, it already is a dependency so you will get it automatically when you install this package.

# Acknowledgements

The work was started by @antifuchs in [this fork](https://github.com/antifuchs/rjsx-mode) and expanded by @Fuco1 in [this pull request](https://github.com/antifuchs/rjsx-mode/pull/1).  It was decided to move this into a separate pacakge as there was no direct relation with `rjsx-mode`.

Built on top of the powerful features of `js2-mode`.
