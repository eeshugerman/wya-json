# Write Yourself A JSON

## What
This [Guile Scheme](https://www.gnu.org/software/guile/) script parses JSON into native Guile data structures (s-expressions). It's tested under Guile 2.x.

## Why
I wrote this purely for educational purposes. It's the first program of any substance that I've written in a lisp.

## Acknowledgments
This code is heavily inspired by @aconchillo's [guile-json](https://github.com/aconchillo/guile-json). My process was basically to read the `guile-json` source until it mostly made sense, then rewrite it "in my own words", consulting the specification given [here](https://www.json.org/json-en.html) as needed.

## Limitations
Escaped unicode (`\uXXXX`) is not supported. Otherwise, the intention is to be fully compliant with the spec linked above.

## Example
```bash
$ cat example.json
{
    "widget": {
        "debug": true,
        "window": {
            "title": "Sample Widget",
            "name": "main_window",
            "size": [500, 500],
            "aspect": -1.5
        },
        "text": {
            "data": "Click Here",
            "size": 36,
            "style": "bold",
            "name": "text1",
        },
    }
}

$ cat example.json | guile -s parser.scm
(("widget"
  ("debug" . #t)
  ("window"
   ("title" . "Sample Widget")
   ("name" . "main_window")
   ("size" . #(500 500))
   ("aspect" . -1.5))
  ("text"
   ("data" . "Click Here")
   ("size" . 36)
   ("style" . "bold")
   ("name" . "text1"))))
```
