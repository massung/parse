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

    (define-parser csv-parser
      (.sep-by1 'csv-record (.is :end)))

Now, the record:

    (define-parser csv-record
      (.sep-by1 'csv-cell (.is :comma)))

And a cell:

    (define-parser csv-cell
      (.one-of (.is :cell) 'csv-string))

Finally, a string:

    (define-parser csv-string
      (.let (cs (>> (.is :quote) (.many-until (.is :chars) (.is :quote))))
        (.ret (format nil "~{~a~}" cs))))

Hopefully the above code reads pretty close to English:

* A *csv-parser* parses many *csv-records* (at least 1), each being separated by a `:end` token.

* A *csv-record* parses many *csv-cells* (at least 1), each being separated by a `:comma` token.

* A *csv-cell* is either a `:cell` token or a *csv-string*.

* A *csv-string* is a `:quote` token, followed by many `:chars` tokens until another `:quote` token is reached, and joins them all together.

We started at the top, and slowly defined each unit that needed to be parsed and defined a monadic parse combinator function for each.

## Basic Parsing

Now that we have built up our parsers, we can use the `parse` function to actually parse tokens.

    (parse parser token-reader &key initial-state errorp error-value)

The *parser* is one of our defined parse combinator functions. The *token-reader* is a function that the parser can call to fetch a new token. It is expected that it return *nil* when there are no more tokens, otherwise it returns 2 values: a token class (typically a keyword) and an optional value for the token.

The *initial-state* will be covered later.

The *errorp* and *error-value* parameters can be thought of like the similar parameters to [*read*](http://www.lispworks.com/documentation/HyperSpec/Body/f_rd_rd.htm#read). If the parse fails *errorp* indicates whether or not an error is signaled. If *errorp* is nil, then *error-value* will be returned instead. The default for *error* is T and *error-value* is nil.

For example, let's create a token-reader function that will return characters from a string.

    CL-USER > (defun char-token-reader (s)
                (let ((i 0))
                  #'(lambda ()
                      (when (< i (length s))
                        (multiple-value-prog1
                            (values :char (char s i))
                          (incf i))))))

Now, let's define a parser that will read all the characters from that token reader.

    CL-USER > (define-parser char-parser (.many (.is :char)))

Finally, let's parse a string with it.

    CL-USER > (parse 'char-parser (char-token-reader "Hello"))
    (#\H #\e #\l #\l #\o)
    NIL
    T

It returned the list of all characters parsed. The second value indicates whether or not the parse was successful (since NIL is a valid return value from your parser).

## Parsing With State

While parsing, the parse monad has state data associated with it. This data can be gotten, set, modified, etc. It is yours to do with as you please. For example, let's create a simple parser that will parse numbers from a list until a non-number is reached, and add them all together.

    CL-USER > (defun numeric-token-reader (list)
                #'(lambda ()
                    (let ((x (pop list)))
                      (when (numberp x)
                        (values :number x)))))
    NUMERIC-TOKEN-READER

Now let's create a parser that will return all the numbers parsed, but also accumulate them into the parse data as it goes. Finally, it will get the parse state and return it.

    CL-USER > (define-parser acc-parser
                (.many (.let (n (.is :number))
                         (.modify #'(lambda (x) (+ x n)))))
                (>>= (.get) '.ret))

Let's give it a whirl...

    CL-USER > (parse 'acc-parser (numeric-token-reader '(1 2 3)) :initial-state 0)
    6
    T

The result of the final parse combinator is `6` and `T` indicates that the parse combinator was successful.

Parse state data can be useful for all sorts of things. For example, while parsing XML, you may use the parse state for a stack of tags being parsed.

## Parsing With A Lexer

If you use the [`lexer`](https://github.com/massung/lexer) package to tokenize, you can use the `with-token-reader` macro to create your *token-reader* function for a parser. Assuming you have a lexer created, you can parse like so:

    CL-USER > (with-token-reader (next-token lexer)
                (parse 'my-parser next-token))

For a simple example, check out some of these libraries I've created that use `lexer` and `parse` together:

* [URL](http://github.com/massung/url)
* [JSON](http://github.com/massung/json)
* [CSV](http://github.com/massung/csv)
* [HTML](http://github.com/massung/html)

## Combinator Functions

Here are all the parse combinator functions that come with the `parse` package:

**>>=** *p f*

Bind the result of parsing *p* by passing it to the function *f*. The result of *f* is expected to be a parse combinator.

**>>** *&rest ps*

Parse each combinator in *ps*, properly binding them together, but ignoring the intermediate results. Returns the final result. Similar to `progn`.

**.ret** *x*

Returns a value, transforming it into the monad.

**.fail** *datum &rest arguments*

Signals an error. Use this instead of *error* because it will not be evaluated unless the parse combinator is called.

**.get**

Each parse state has data associated with it. This parse combinator always succeeds and returns that data.

**.put** *x*

Replaces the current parse state data with *x*. Returns *x*.

**.modify** *function*

Gets the current parse state data and passes it to *function*. The return value is then put back into the parse state data.

**.push** *x*

Assumes the current parse state data is a list, and pushes *x* onto the head of the list. Returns the new parse state data.

**.pop**

Assumes the parse state data is a list and pops the top value off the list. Returns the value popped, and puts the rest of the list back into the parse state data.

**.opt** *x p*

Optionally parses *p*. If successful, returns the token value, otherwise returns *x* and does not consume the token.

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

**.one-of** *&rest ps*

Match the current token against one of the parse combinators in *ps*. Returns the first match's value.

**.none-of** *&rest ps*

Ensure that the current token does not match one of the parse combinators in *ps*. Returns the value of the token.

**.ignore** *p*

Parse *p*, but ignore the value. The `>>` function ignores intermediate results from parse combinators, but will always `.ret` the final value. The `.ignore` parse combinator will always `.ret nil`.

**.maybe** *p*

Try and parse *p*. If it's there, ignore it (.ret nil). If it's not there, that's okay.

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

**.let** (*var p*) *&body body*

Parse *p* and bind the result into *var*. Execute *body* and return its result with `.ret`.

**.let*** (*&rest bindings*) *&body body*

Each binding is expected to be in the form of (*var p*). Each variable will be bound to the result of parsing *p*. Finally, *body* is executed and its result is returned with `.ret`.

**.prog1** *p &body body*

Parse *p*, save the result, then executes *body* and finally returns the result of *p* with `.ret`.

**.progn** *&body body*

Executes the body and returns the result with `.ret`. Basically `(.ret (progn ,@body))`.
