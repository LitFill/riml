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

generate (currently):

```html
<html lang="id"><head><title>"Judul"</title><link rel="stylesheet" src="style.css"></link></head><body><h1 class="main" id="root" visible="true">"Halo Semuanya"</h1><div class="container" id="app"><p class="bg-red-300 note card" id="ppp" data-day="sunday">"Sunday"</p></div></body></html>
```
