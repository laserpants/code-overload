> A work-in-progress tutorial that explores functional programming techniques and technologies to build a modern, mobile web application using JavaScript and a Haskell-powered RESTful JSON API-server (server-API???). 

## Overview of technologies

### Functional programming

Functional programming finds its origins in Alonzo Church’s Lambda Calculus (λ-calculus) — a logical theory of computable functions that predates the electronic computer. The first language based on ideas from λ-calculus was LISP; invented by John McCarthy in 1958.

As a form of declarative programming, code written in functional style carries no immediate sense of sequentiality. This makes it particularly well-suited for parallel and asynchronous programming, since it enables us to express the formal logic of a computation without thinking of it in terms of control flow or sequential progression.

In this context, conventional (non-functional) programming languages (C, C++, Java, PHP, etc.) are sometimes referred to as *imperative*, or prescriptive languages. Intuitively, we can think of the difference as,

* imperative: a sequence of instructions that outline what a program should do;
* declarative: a collection of definitions that declare what things are (where a *"thing"* more formally refers to a mathematical object of some sort). 

The important point to note here is that a declaration, by itself does not imply any computation. Another key difference is that the function, as one could expect, has a much more central role. Functional programming languages treat functions as first-class values. They can be passed around as arguments, and used in declarations, similar to class-type objects in OOP.

We say that the state of an object is *mutable* if the operational semantics allow its data to change as it interacts with program logic. Functional programming favors construction over mutation, and we think of a program not so much as a sequence of explicit state manipulations, but rather as a series of transformations. Thus, an important tool in functional programming is function composition, i.e., the process of applying one function to the output of another. 

    f = a . b . c

In the traditional, *stateful* model of computation, we evaluate the result of an algorithm by examining the configuration of some part of memory when the program halts (if it does). When programming with implicit, or immutable state, the values of objects' attributes are invariant over time — we do not allow their state to change. Instead, state is implicitly "threaded" in a direct way between procedure calls. To obtain the result of a computation, an expression is reduced to its *normal form*, i.e., a representation in which it is fully evaluated.

(Examples borrowed from here: http://stackoverflow.com/questions/6872898/haskell-what-is-weak-head-normal-form)

###### Normal form:

    42                   -- (the answer to the ultimate question of life) already fully evaluated
    \x -> (x + 1)        -- an anonymous function that takes a value x, and returns x + 1

###### *Not* normal form:

    1 + 2                -- evaluates to 3
    "he" ++ "llo"        -- (++ denotes string concatenation) can be reduced to "hello"
    (\x -> (x + 1)) 4    -- here, the function is applied to the value 4, hence this expression evaluates to 5
    
For someone who comes from an imperative language background, programming in pure functional style can require some change in perspective.

#### Pure functions

The term *pure* is often used to describe a property of expressions, relevant to this discussion. For a function to be considered pure, 

* it is not allowed to exhibit any *side effects*, and
* it must be *referentially transparent*.

Recall the black-box metaphor, found in numerous mathematical textbooks (illustration?), and according to which a function's internals are completely sealed off from the outside world. A side-effect is when a function or expression violates this principle — that is, the procedure is allowed to communicate in some way with other program units (e.g. to share and exchange information).

A function is said to be referentially transparent if (and only if) it, given the same input parameters, always produces the same output (return value). If one is looking for a raison d'être for pure functional programming, referential transparency is a good candidate. When reasoning with formulae in algebra, arithmetic, and logic, this property — also called *substitutivity of equals for equals* — is so fundamentally important that it is usually taken for granted.

##### Equational reasoning

Consider a simple example:

    x = 42

In functional programming, the left-hand and right-hand side of the equals sign are substitutable for each other both ways. That is, unlike in a language like C, the above notation truly asserts an equality. A consequence of this is that we can apply *equational reasoning* to program code written in a pure functional language. To contrast this, the type of operation performed by C-like languages is sometimes referred to as a *destructive assignment*.  

### Haskell

Haskell is a modern, compiled, strongly typed, purely functional, programming language based on the (Girard-Reynolds) polymorphic lambda calculus, also known as System-F. The Haskell language standard is the result of over twenty years of research in programming language design. The language is named after the famous logician Haskell Curry.

GHC is the most widely used Haskell compiler. It comes bundled with the Haskell platform — a comprehensive development environment available for Windows, Mac, and Linux.

Haskell belongs to a language family commonly referred to as *lazy* functional programming languages. Lazy evaluation is a technique used by compilers to implement an underlying language property known as non-strictness. Strict and non-strict semantics differ in how evaluation of expressions propagates:

* In the more common, strict semantics, expressions are evaluated from the inside-out. In a strict language, the function arguments are evaluated before the body. (example)
* Under non-strict semantics, evaluation starts from the outside and unfolds towards the center. Sub-expressions are only computed when needed (call-by-need).

(example)

Although Haskell encompasses a sophisticated type system that supports polymorphism and an advanced form of systematic overloading; clever use of type inference allows for a very minimalistic and elegant syntax. Surprisingly, since type annotations are rarely necessary, many times even more succinct than that of a dynamically typed scripting language:

    -- Insertion sort implementation

    import Data.List

    insort [] = []
    insort (x:xs) = insert x $ insort xs

This function accepts any list as input, as long as the type of its elments is an instance of the `Ord` type class:

    insort [4,3,1,5,8]
    [1,3,4,5,8]

    insort ['f','b','e','e']
    "beef"

### JavaScript

### JSON

### REST

### SQLite

## What we are building

The idea of the application is to allow users to post their code snippets, and let other users improve, refactor, and optimize — in other words "remix" — the program without changing its semantics.

## Part I. Server

### Happstack

Happstack is the **H**askell **app**lication server **stack**.

http://happstack.com

I highly recommend The Happstack Crashcourse, written by Jeremy Shaw: http://happstack.com/docs/crashcourse/index.html

#### Installation

#### Hello World!

    module Main where

    import Happstack.Server (nullConf, simpleHTTP, toResponse, ok)
 
    main :: IO ()
    main = simpleHTTP nullConf $ ok "Hello, World!"

...

    nullConf = Conf
        { port      = 8000
        , validator = Nothing
        , logAccess = Just logMAccess
        , timeout   = 30
        }

Running this example will launch the server on port 8000 — the default port. We can now access the server from a browser (`http://localhost:8000/`) or from the command line, using for example

    $ curl http://localhost:8000

To override a specific setting, we could then write, e.g.,

    main = simpleHTTP nullConf { port = 8080 } $ ok "Hello, World!"
    
Hello world with routes

    module Main where

    import Happstack.Server
    import Control.Monad                  ( mzero, msum )

    main :: IO ()
    main = do
       simpleHTTP nullConf $ msum 
          [ dir "one" $ ok $ toResponse "You said one"
          , dir "two" $ ok $ toResponse "Two it is"
          , mzero
          ]

We can now run

    $ curl http://localhost:8000/one
    You said one

In this example, we are combining a list of url paths into a single value using `msum`, defined by the `MonadPlus` type class.

    msum :: MonadPlus m => [m a] -> m a
    msum = foldr mplus mzero

> The behaviour of MonadPlus is to try each ServerPartT in succession, until one succeeds.

`dir` is used to match on static path components.

> Pop a path element and run the ServerPartT if it matches the given string. 

### Language extensions

#### FlexibleContexts

#### Template Haskell

#### OverloadedStrings

#### RecordWildCards

...

The RecordWildCards extension introduces a simple notation that makes pattern matching over records a bit less verbose, in cases where many fields are involved.

### HaskellDB

HaskellDB is a combinator library for building syntactically correct, type-safe database queries, similar to the Language Integrated Query (LINQ) component in the .NET framework (or ARel for Ruby on Rails).

HaskellDB does follow an ORM approach, although I am not sure the term "object" is the most appropriate here. What is common between HaskellDB, blaze-html, Parsec, and many other Haskell libraries is that they allow you to build statements using native Haskell code, without the need for a separate (domain-specific) language, such as SQL.

As an example, here is what a simple query can look like using HaskellDB:

    getContent = do
      content <- table T.content
      project $ F.id << content!F.id
  

The original HaskellDB library was written by Daan Leijen and Erik Meijer (who is also the architect behind LINQ). It draws heavily from concepts in relational algebra, and the theory behind HaskellDB is described in Leijen's PhD thesis: “The λ Abroad - A Functional Approach to Software Components.” Fortunately, understanding HaskellDB does not require deep knowledge in relational algebra, which serves in this context more as a theoretical foundation for relational databases.

#### The new HaskellDB

The first version of HaskellDB was relying on a feature called TRex (Typed records with extensibility), only available in the Hugs interpreter. Additionally, it only supported a Windows-specific ADO-based database backend. To address these, among other issues, a more recent, portable version of the library was developed by students at Chalmers University of Technology.

###### Student paper:

http://viblo.se/pmwiki/uploads/Projects/haskelldb.pdf

##### Connecting

We will be using SQLite in this tutorial. To connect to the database we define a `withDB` function, as described in the Chalmers paper:

    withDB :: (Database -> IO a) -> IO a
    withDB = hdbcConnect generator (connectSqlite3 "db.sqlite")

##### Preparing the tables

##### Querying

HaskellDB defines a Query monad, and queries are expressed using native Haskell functions.

...

### Aeson

In Greek mythology, Aeson was the father of Jason. In Haskell folklore, Aeson is a JSON parsing and generation library optimized for ease of use and high performance.

#### Creating a simple JSON type

##### ToJSON

##### FromJSON

### Resources

### Routes

### Database and schema

### Types

### Models

### Controllers


## Part II. Client

## Possible improvements
