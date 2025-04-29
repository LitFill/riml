# riml

Rust's maud Inspired htML

## Example

this snippet:

```
html lang="id" {
    head {
        title { "Judul" }
        link rel="stylesheet" src="style.css" {}
    }
    body {
        h1 .main #root visible="true" {
            "Halo Semuanya"
        }

        div .container #app {
            p .bg-red-300 .note .card #ppp data-day="sunday" { "Sunday" }
        }
    }
}
```

currently generates:

```html
<html lang="id">
    <head>
        <title>
            Judul
        </title>
        <link rel="stylesheet" src="style.css"></link>
    </head>
    <body>
        <h1 visible="true" class="main" id="root">
            Halo Semuanya
        </h1>
        <div class="container" id="app">
            <p data-day="sunday" class="bg-red-300 note card" id="ppp">
                Sunday
            </p>
        </div>
    </body>
</html>
```
