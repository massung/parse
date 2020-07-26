# The PARSE Package

The parse package is a simple token parsing library for Common Lisp.

It is based on Haskell's [Parsec](http://hackage.haskell.org/package/parsec-3.1.9/docs/Text-Parsec.html) library, but using macros to make more accessible to Lisp.

In a few technical bullet points, it is:

* [Top-down](https://en.wikipedia.org/wiki/Top-down_parsing)
* [Recursive-descent](https://en.wikipedia.org/wiki/Recursive_descent_parser)
* [Backtracking - LL(k)](https://en.wikipedia.org/wiki/LL_parser)
* [Combinatory](https://en.wikipedia.org/wiki/Parser_combinator)
* [Monadic](https://en.wikipedia.org/wiki/Monad_%28category_theory%29)

## Token Generation

Before we can parse, we need to generate tokens.

The `parse` function must be given a function with 0-arity that can be called whenever it needs to read another token. This *next-token* function should return 2 values for each token:

* The token class (typically a keyword)
* The token value (optional)

When there are no more tokens in the source stream, it should return `nil`. *Note: it may be called several times at the end of the token stream, so it should handle that condition.*

Let's create a simple token function we can use for the rest of our parsing examples. It will simply read the next the next value from a list and return that value as the token's value, and use the type as the token's class.

    (defun make-token-reader (list)
      #'(lambda ()
          (let ((x (pop list)))
            (when x
              (etypecase x
                (string    (values :string x))
                (character (values :character x))
                (number    (values :number x))
                (symbol    (values :symbol x)))))))

*Note: this example shows that tokens can be generated many ways. However, the most common method of generating tokens would be with a [`lexer`](http://github.com/massung/lexer) package.*

## Quickstart

Now that we can generate tokens, let's take a look at the `parse` function and try some simple examples.

    (parse parser next-token &key initial-state (errorp t) error-value)

Ignoring the keyword arguments for now, simply note that the parse function requires both a parse combinator function (*parser*) and a token generator function (*next-token*).

Let's parse a symbol...

    CL-USER > (parse (.is :symbol) (make-token-reader '(a b c)))
    A
    T

The first value returned is the result of the parse combinator function (in this case the value of the token parsed), and `T` indicating that the parse was successful.

However, our parse functions are *combinatory*, so let's chain some more in there to make it a bit more interesting.

    CL-USER > (parse (.many1 (.is :symbol)) (make-token-reader '(a b c)))
    (A B C)
    T

Excellent! What if we supply the wrong tokens, though?

    CL-USER > (parse (.many1 (.is :symbol)) (make-token-reader '(1 2 3)))
    Parse failure

Our parser expected symbols and was given numbers. Good. But, maybe we want to read symbols or numbers?

    CL-USER > (parse (.many1 (.either (.is :symbol) (.is :number))) (make-token-reader '(a 1 b 2 c 3)))
    (A 1 B 2 C 3)
    T

Okay, but now our parser is getting to be a bit unwieldy. Let's actually define our parser outside the REPL. In addition, let's parse symbols or numbers, but only keep the numbers in the resulting list.

    (define-parser number-parser
      "Parse a list of numbers, ignoring all symbols."
      (.many1 (.either (.is :number)
                       (.do (.skip-many1 (.is :symbol))
                            (.is :number)))))

Looking at this combinator, it tries (1 or more times) to either parse a number, or - if that fails - skip 1 or more symbols, and then parse a number.

Now, let's plug it in and see what we get.

    CL-USER > (parse 'number-parser (make-token-reader '(a z 1 b 2 c 3)))
    (1 2 3)
    T

## Returning Values

Until now, we've been using the `.is` parse combinator, which always returns the value of the token parsed. But, sometimes it's useful to return other values instead. This is done with the `.ret` parse combinator.

    CL-USER > (parse (.ret 10) (make-token-reader nil))
    10
    T

It's important to use `.ret`, as the values being returned must be put into a parse combinator function.

For another example, let's create a parser that returns the character code of any characters parsed.

    (define-parser char-code-parser
      "Parses a character, returns its character code."
      (.let (c (.is :character))
        (.ret (char-code c))))

And try it...

    CL-USER > (parse 'char-code-parser (make-token-reader '(#\!)))
    33
    T

## Backtracking

The `parse` package supports arbitrarily deep backtracking. We can test this with a simple parse combinator...

    (define-parser backtracker
      "Test backtracking."
      (.either (.do (.skip-many1 (.is :number)) (.is :char))
               (.do (.skip-many1 (.is :number)) (.is :symbol))))

The above combinator should parse a bunch of numbers and then either a character or a symbol. Once it has parsed a list of numbers, though, if it fails to parse a character it needs to backtrack in order to try the next combinator.

    CL-USER > (parse 'backtracker (make-token-reader '(1 2 3 a)))
    A
    T

Success!

*Note: it's more efficient to write your parsers to be predictive when possible. The above parser can be re-written to be predictive like so:*

    (define-parser predictive
      "A predictive version of the backtracker combinator."
      (.do (.skip-many1 (.is :number))
           (.either (.is :char)
                    (.is :symbol))))

With the predictive version, the parser can just keep moving forward and not have to rewind state, and parse the same tokens again.

## Error Handling

Remember the *errorp* and *error-value* keyword arguments to the `parse` function? They will control what happens from a parse failure.

If *errorp* it's set to `nil`, then instead of signaling a parse failure error, *error-value* will be returned, along with `nil` indicating that the parse failed.

    CL-USER > (parse 'number-parser (make-token-reader '(#\a)) :errorp nil :error-value 'ack)
    ACK
    NIL

There is also a `.fail` parse combinator function that can be used to report an error in parsing. It will signal an error during parsing. If *errorp* is `nil` and `.fail` is tripped, the error will be ignored and the parse will fail.

## Parsing With State

The parse monad also has state data associated with it. This data can be gotten (`.get`), set (`.put`), etc. It is yours to do with as you please.

Let's create a parse combinator that will accumulate all the numbers it comes across in a token stream.

    (define-parser sum-parser
      "Add all number tokens together."
      (.do (.many (.or (.let (n (.is :number))
                         (.modify #'(lambda (x) (+ x n))))
                       (.any)))
           (.get)))

Let's give it a whirl...

    CL-USER > (parse 'sum-parser (make-token-reader '(1 a 2 b 3 c)) :initial-state 0)
    6
    T

*Note: we needed to set the `:initial-state` of the parse monad!*

Parse state data can be useful for all sorts of things. For example, while parsing a markup language (e.g. XML), the parse state might hold a stack of tags.

## Gotchyas

It's important to remember that while the parse combinators are functional, Common Lisp is neither a [purely functional](https://en.wikipedia.org/wiki/Purely_functional) nor [lazy](https://en.wikipedia.org/wiki/Lazy_evaluation) language!

#### Side Effects in Combinators

*All your parse combinators should be 100% free of side-effects!*

Due to backtracking and the eagerness of building the parse combinators, code can execute during parsing that you didn't think would parse. For example:

    (define-parser oops-parser
      "Show an example of a bad parser."
      (.either (.do (.is :number)
                    (.ret (print 'ack)))
               (.let (s (.is :string))
                 (.ret (print s)))))

The above parser looks like it will print "ACK" *only* if a number is parsed. But, let's see what actually happens...

    CL-USER > (parse 'oops-parser (make-token-reader '("Test")))
    ACK
    "Test"

The "ACK" was printed anyway, because the `.ret` function evaluated its arguments in order to build the parse combinator.

#### Shared State

While the parse state is copied between combinators - allowing for backtracking - if the state is an instance of an object, then the state data will be shallow copy.

This means that it's possible to be walking down one parse branch that will eventually fail, modify the parse state, then backtrack to a correct branch, which is now working with an invalid parse state.

## Real-World Examples

Some real-world examples that use a [`lexer`](http://github.com/massung/lexer) to tokenize as well, check out the code in the following respositories:

* [URL](http://github.com/massung/url)
* [XML](http://github.com/massung/xml)
* [TOML](http://github.com/sgarciac/sawyer)

## Documentation

Here are all the built-in parse combinator functions:

**>>=** *p f*

Bind the result of parsing *p* by passing it to the function *f*. The result of *f* is expected to be a parse combinator.

**>>** *p m*

Ignore the result of parsing *p* and immediately chain the parse combinator *m*.

**.prog1** *form &body body* (macro)

Just like **prog1**, except that the results of the first form are returned to the parse monad with **.ret**. Useful for executing random Lisp code inside a parse combinator (beware of side-effects!).

**.progn** *&body body* (macro)

Just like **progn**, except that the results of the last form in *body* are returned to the parse monad with **.ret**. Useful for executing random Lisp code inside a parse combinator (beware of side-effects!).

**.let** (*var p*) *&body body* (macro)

Parse *p* and bind the result into *var*. Execute *body*. The final value of *body* needs to be a parse combinator to continue execution.

**.let*** (*binding &rest bindings*) *&body body* (macro)

Create bindings and chain then together. Similar to **.let**.

**.do** (*p &rest ps*) (macro)

Parse *p* and each combinator in *ps* in order. Ignore all the intermediate results and return the last one. This is just a wrapper around chaining **>>** combinators.

**.or** (*p &rest ps*) (macro)

Attempts to parse *p* and each combinator in *ps*. Returns the first successful result and ignores the rest. This is a wrapper around chaining **.either** combinators.

**.ret** *x*

Returns the value *x*.

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

**.any**

Matches any token. Returns the value of the token.

**.eof**

Matches if at the end of the token stream. Returns `nil`.

**.is** *class*

Matches the current token against *class*. Returns the value of the token.

**.either** *p1 p2*

Attempts to parse *p1*. If that fails, tries *p2*.

**.opt** *x p*

Optionally parses *p*. If successful, returns the token value, otherwise returns *x* and does not consume the token.

**.ignore** *p*

Parse *p*, but ignore the value (always returns `nil`).

**.all** *p &rest ps*

Parse *p* and then the *ps* combinators in order, returns a list of all the values parsed in-order.

**.maybe** *p*

Tries to parse *p*. If successful, returns `nil`. If it fails, returns `nil` anyway.

**.many** *p*

Parse zero or more occurrences of *p*. Return the list of parsed values.

**.many1** *p*

Parse one or more occurrences of *p*. Return the list of parsed values.

**.many-until** *p end*

Parse *p* zero or more times and then parse *end* returning a list of all parsed values (excluding *end*).

**.sep-by** *p sep*

Parse zero or more occurrences of *p* separated by *sep*. Return the list of all *p*'s parsed.

**.sep-by1** *p sep*

Parse one or more occurrences of *p* separated by *sep*. Return the list of all *p*'s parsed.

**.skip-many** *p*

Parse zero or more occurrences of *p*, ignores the results and returns `nil`.

**.skip-many1** *p*

Parse one or more occurrences of *p*, ignores the results and returns `nil`.

**.between** *open-guard close-guard p*

Parse *open-guard*, then *p*, binding the result of *p*. Parses the *close-guard* and then return the result of *p*.
