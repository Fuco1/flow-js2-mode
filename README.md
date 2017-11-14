# flow-js2-mode

This package adds support for [Flow](https://flow.org/), a static typechecking extension for javascript, to [js2-mode](https://github.com/mooz/js2-mode), a popular javascript editing mode for Emacs.

This project is heavily WIP at the moment, things might break and change.  Please report all the issues, even minor ones so we can polish this as soon as possible.

# Usage

Enable `flow-js2-minor-mode` in the `js2-mode` buffer.

# List of features (incomplete)

- [x] In object literals, `{foo: bar}` the type is parsed even thought it is not a type.  This breaks fontification and semantics of the object literals.  I don't see a way yet to distinguish if the `foo` is a key or a variable.
- [x] `import type {Config} from 'types/Config'` does not work.
- [x] Typing a destructured object does not work: `function send(res, {body, content}: Response)`
- [x] Parse `function<T>` generic constructs.
  - [x] Does not work with multiple generic arguments (separated by commas)
  - [x] Does not work with typed generics such as `function <T: number>`
- [x] key type specification in objects should work: `clients: { [client_id: string]: Client }`

# Acknowledgements

The work was started by @antifuchs in [this fork](https://github.com/antifuchs/rjsx-mode) and expanded by @Fuco1 in [this pull request](https://github.com/antifuchs/rjsx-mode/pull/1).  It was decided to move this into a separate pacakge as there was no direct relation with `rjsx-mode`.

Built on top of the powerful features of `js2-mode`.
