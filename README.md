# elm-bootforms

This package tries to provide a very simple and powerful interface to HTML(5) forms powered by [bootstrap](http://getbootstrap.com/). It uses [evancz/elm-html](http://package.elm-lang.org/packages/evancz/elm-html/latest/) as HTML rendering backend.

## Example

```elm
foo : H.Html
foo =
    textInput
    { id: "foo"
    , label: "The foo field"
    , helpBlock: Nothing
    , value: ""
    , hasError: False
    , onValue: \val -> Signal.message addr (SetValue val)
    , onError: \err -> Signal.message addr (SetError err)
    }
```

## Install

Install using [elm-package](https://github.com/elm-lang/elm-package).

# Roadmap

* Support generating complete forms with error handling
* ...