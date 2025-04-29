# riml

Rust's maud Inspired htML DSL

## Example

this snippet:

```
html lang="id" {
    head {
        title { "Judul" }
        link rel="stylesheet" src="style.css";
    }
    body {
        h1 .main #root visible="true" {
            "Halo Semuanya"
        }

        div .container #app {
            p .bg-red-300 .note .card #ppp data-day="sunday" { "Sunday" }
            br;
            img .logo .image #logo src="static/logo.png";
        }
        div .swap-here {
            ! this is a comment
            ! this element is controlled by the script
        }
    }
}
```

currently generates:

```html
<!DOCTYPE html>
<html lang="id">
    <head>
        <title>
            Judul
        </title>
        <link rel="stylesheet" src="style.css">
    </head>
    <body>
        <h1 class="main" id="root" visible="true">
            Halo Semuanya
        </h1>
        <div class="container" id="app">
            <p class="bg-red-300 note card" id="ppp" data-day="sunday">
                Sunday
            </p>
            <br>
            <img class="logo image" id="logo" src="static/logo.png">
        </div>
        <div class="swap-here">
            <!-- this is a comment -->
            <!-- this element is controlled by the script -->
        </div>
    </body>
</html>
```

## general syntax

the general syntax is:

- `tag [.class]* [#id] [attr="val"]* { [childern]* }` for elements with ending tag `</tag>`
- `tag [.class]* [#id] [attr="val"]* ;` for elements without
- `! line wise comment` until newline

where:

- `[...]` is optional
- `*` is zero or more
