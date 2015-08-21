# The PARSE Package

The parse package is a simple, backtracking, recursive-descent, top-down, monadic parsing library for Common Lisp. It is based on the Haskell [Parsec](http://hackage.haskell.org/package/parsec-3.1.9/docs/Text-Parsec.html) library, but with macros making it a bit more accessible to Lisp. If you don't understand Parsec, [this](http://book.realworldhaskell.org/read/using-parsec.html) might be a good primer to help a bit.

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

For now, let's create a simple token reader function that we can use throughout the rest of the examples. It will simply return elements from a list, one at a time, with the element being the token's value, and the type being the class of the token.

    CL-USER > (defun make-token-reader (list)
                #'(lambda ()
                    (let ((x (pop list)))
                      (when x
                        (typecase x
                          (string (values :string x))
                          (character (values :char x))
                          (number (values :number x))
                          (symbol (values :symbol x)))))))

Let's now create our first parser. It will read as many characters as it can from the parser and collect them into a list.

    CL-USER > (define-parser char-parser (.many (.is :char)))

Finally, let's give it a try...

    CL-USER > (parse 'char-parser (make-token-reader '(#\a #\b #\c)))
    (#\a #\b #\c)
    T

It returned the list of all characters parsed. The second value indicates whether or not the parse was successful (since NIL is a valid return value from your parser).

## Backtracking

The `parse` package allows for backtracking. This means that if one parse combinator fails, it can roll-back its state and try another combinator instead. For example:

	CL-USER > (define-parser backtracking
                (.one-of (>> (.is :number) (.many (.is :symbol)) (.is :char))
                         (>> (.is :number) (.many (.is :symbol)) (.is :string))))
	BACKTRACKING

Notice how if we parsed a number, then a series of symbols, and a character wasn't parsed next, it would require the parser to completely roll-back and try and parse a number again? Well, let's try...

	CL-USER > (parse 'backtracking (make-token-reader '(1 a b c #\z)))
	#\z
	T

	CL-USER > (parse 'backtracking (make-token-reader '(1 a b c "Success!")))
	"Success!"
	T

## Parsing With State

While parsing, the parser has state data associated with it. This data can be gotten, set, modified, etc. It is yours to do with as you please. For example, let's create a parser that sums all the numbers it finds in our token stream.

    CL-USER > (define-parser 'accumulator
                (.many (.one-of (.let (n (.is :number))
                                  (.modify #'(lambda (x) (+ x n))))
                                (.any)))
                (.get))

It will parse as many tokens as are in the stream, first trying to parse a number. If it finds one, add it to the current state (with `.modify`), otherwise parse any other token (`.any`). Finally, return the state with `.get`.

Let's give it a whirl...

    CL-USER > (parse 'accumulator (make-token-reader '(1 "ignore" 2 #\a #\z 3 foo bar)) :initial-state 0)
    6
    T

Success!

The functions for working with the parse state are `.get`, `.put`, `.modify`, `.push`, and `.pop`.

*Notice how we had to pass an `:initial-state` to the `parse` function!*

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
