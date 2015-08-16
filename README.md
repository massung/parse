# The PARSE Package

The parse package is a simple, monadic parsing library for Common Lisp. It is based on the Haskell [Parsec](http://hackage.haskell.org/package/parsec-3.1.9/docs/Text-Parsec.html) library, but with macros making it a bit more accessible to Lisp. If you don't understand Parsec, [this](http://book.realworldhaskell.org/read/using-parsec.html) might be a good primer to help a bit.

It is designed to use my [`lexer`](https://github.com/massung/lexer) packages as well, but doesn't require it. All the examples in this README will use it, though.

## Combinatory Parsing

[Combinatory parsing](https://en.wikipedia.org/wiki/Parser_combinator) is a type of recursive-decent, top-down parsing.

This is **not** a tutorial on monads. There are lots of tutorials out there attempting to teach them. Most are terrible, some are quite good; [here's one](http://www.learnyouahaskell.com/a-fistful-of-monads).

To give an example of how to think about it, let's consider a CSV file. If you were to try and write a parser for this by hand, you might begin by writing some pseudo code like so:

    (defun parse-csv (string)
      (parse-csv-lines))

    (defun parse-csv-lines ()
      (loop for line = (parse-csv-line) while line collect line))

    (defun parse-csv-line ()
      (loop for cell = (parse-csv-cell) while cell collect cell))

    (defun parse-csv-cell ()
      (parse-string-or-until-comma))

Notice how we began at the top-level and kept breaking down each element that needed parsed until done. Combinatory parsing allows us to actually do the above very easily and expressively.

Now let's learn how to use that to create a monadic parser...

## Quickstart

...Let's make the CSV parser from above.

First, we identify each major part of the format that will be parsed:

* CSV (many records delimited by newlines)
* Record (many cells delimited by commas)
* Cell (quoted string or value)

Assuming we have a [lexer](https://github.com/massung/lexer) that can tokenize our input stream (which we don't need quite yet), let's create some parsers that do what we want.

First, the CSV parser:

    (defparser csv-parser
      (.sep-by1 'csv-record (.is :end)))

Now, the record:

    (defparser csv-record
      (.sep-by1 'csv-cell (.is :comma)))

And a cell:

    (defparser csv-cell
      (.one-of (.is :cell) 'csv-string))

Finally, a string:

    (defparser csv-string
      (.let (cs (>> (.is :quote) (.many-until (.is :chars) (.is :quote))))
        (format nil "~{~a~}" cs)))

Hopefully the above code reads pretty close to English:

* A *csv-parser* parses many *csv-records* (at least 1), each being separated by a `:end` token.

* A *csv-record* parses many *csv-cells* (at least 1), each being separated by a `:comma` token.

* A *csv-cell* is either a `:cell` token or a *csv-string*.

* A *csv-string* is a `:quote` token, followed by many `:chars` tokens until another `:quote` token is reached, and joins them all together.

We started at the top, and slowly defined each unit that needed to be parsed and defined a monadic parse combinator function for each.

## Parsing

Now that we have built up our parsers, we can use the `parse` function to actually parse tokens.

    (parse parser token-reader)

The *parser* is one of our defined parse combinator functions. The *token-reader* is a function that the parser can call to fetch a new token. It is expected that it return *nil* when there are no more tokens, otherwise it returns 2 values: a token class (typically a keyword) and an optional value for the token.

For example, let's create a token-reader function that will return characters from a string.

    CL-USER > (defun char-token-reader (s)
                (let ((i 0))
                  #'(lambda ()
                      (when (< i (length s))
                        (multiple-value-prog1
                            (values :char (char s i))
                          (incf i))))))

Now, let's define a parser that will read all the characters from that token reader.

    CL-USER > (defparser char-parser (.many (.is :char)))

Finally, let's parse a string with it.

    CL-USER > (parse 'char-parser (char-token-reader "Hello"))
    (#\H #\e #\l #\l #\o)
    T

It returned the list of all characters parsed. The second value returned indicates whether or not everything was parsed (e.g. the token reader returned NIL and there are no more tokens left).

Done!

## Parsing With Lexer

If you use the [`lexer`](https://github.com/massung/lexer) package to tokenize, you can use the `with-token-reader` macro to create your *token-reader* function for a parser. Assuming you have a lexer created, you can parse like so:

    CL-USER > (with-token-reader (next-token lexer)
                (parse 'my-parser next-token))

For a simple example, check out some of these libraries I've created that use `lexer` and `parse` together:

* [URL](http://github.com/massung/url)
* [JSON](http://github.com/massung/json)
* [CSV](http://github.com/massung/csv)

## Combinator Functions

Here are all the parse combinator functions that come with the `parse` package:

**>>=** *p f*

Bind the result of parsing *p* by passing it to the function *f*. The result of *f* is expected to be a parse monad (using `.ret`).

**>>** ***&rest** ps*

Parse each combinator in *ps*, properly binding them together, but ignoring the intermediate results. Returns the final result. Similar to `progn`.

**.ret** *x*

Returns a value, transforming it into the monad.

**.fail** *datum **&rest** arguments*

Signals an error. Use this instead of *error* because it will not be evaluated unless the parse combinator is called.

**.opt** *p default*

Optionally parses *p*. If successful, returns the token value, otherwise returns *default* and does not consume the token.

**.satisfy** *pred*

Ensures that the current token's class satisfies the *pred* predicate function. Returns the value of the token if successful.

**.any**

Matches any token. Returns the value of the token.

**.eof**

Matches if at the end of the token stream. Returns `nil`.

**.is** *class*

Matches the current token against *class*. Returns the value of the token.

**.is-not** *class*

Ensures that the current token is not of *class*. Returns the value of the token.

**.one-of** ***&rest** ps*

Match the current token against one of the parse combinators in *ps*. Returns the first match's value.

**.none-of** ***&rest** ps*

Ensure that the current token does not match one of the parse combinators in *ps*. Returns the value of the token.

**.many** *p*

Parse zero or more occurrences of *p*. Return the list of parsed values.

**.many1** *p*

Parse one or more occurrences of *p*. Return the list of parsed values.

**.many-until** *p term*

Parse zero or more occurrences of *p* until *term* is successfully parsed. Return a list of all *p*'s parsed.

**.sep-by** *p sep*

Parse zero or more occurrences of *p* separated by *sep*. Return the list of all *p*'s parsed.

**.sep-by1** *p sep*

Parse one or more occurrences of *p* separated by *sep*. Return the list of all *p*'s parsed.

**.skip** *p*

Parse zero or more occurrences of *p*, ignore them and return `nil`.

**.between** *open-guard close-guard p*

Parse *open-guard*, then *p*, binding the result of *p*. Parse the *close-guard* and then return the result of *p*.

**.let** (*var p*) ***&body** body*

Parse *p* and bind the result into *var*. Execute *body* and return its result with `.ret`.

**.let*** ((***&rest** bindings*) ***&body** body*

Each binding is expected to be in the form of (*var p*). Each variable will be bound to the result of parsing *p*. Finally, *body* is executed and its result is returned with `.ret`.

**.prog1** *p **&body** ps*

Parse *p*, save the result, then parse *ps* and finally return the result of *p*.

**.progn** ***&body** body*

A synonym for `>>` to be more "Lisp-y".
