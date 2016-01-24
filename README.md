# elm-bootforms

This package tries to provide a very simple and powerful interface to HTML(5) forms powered by [bootstrap](http://getbootstrap.com/). It uses [evancz/elm-html](http://package.elm-lang.org/packages/evancz/elm-html/latest/) as HTML rendering backend.

## Docs

See [agrafix/elm-bootforms](http://package.elm-lang.org/packages/agrafix/elm-bootforms/latest/) on the central elm package repo.

## Example

```elm
foo : Address Action -> Model -> H.Html
foo addr m =
    textInput
    { id = "username"
    , label = "Benutzername or Email"
    , helpBlock = Nothing
    , value = stringFormVal m.usernameOrEmail
    , onValue = \val ->
        Signal.message addr <|
        case val.value of
            Ok x -> SetLogin x
            Err _ -> Nop
    }
```

## Install

Install using [elm-package](https://github.com/elm-lang/elm-package).

# Roadmap

* Support generating complete forms with error handling
* ...