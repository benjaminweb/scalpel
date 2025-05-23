Scalpel
[![Build status](https://github.com/fimad/scalpel/actions/workflows/stack.yml/badge.svg)](https://github.com/fimad/scalpel/actions/workflows/stack.yml) [![Hackage](https://img.shields.io/hackage/v/scalpel.svg)](https://hackage.haskell.org/package/scalpel)
=======

Scalpel is a convenient web scraping library to extract data from HTML webpages.
It's inspired by libraries like
[Parsec](http://hackage.haskell.org/package/parsec-3.1.7/docs/Text-Parsec.html)
and Perl's [Web::Scraper](http://search.cpan.org/~miyagawa/Web-Scraper-0.38/),
and provides a declarative, monadic interface on top of the efficient
HTML parsing library [html-parse](https://hackage.haskell.org/package/html-parse)

Quickstart
----------

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<|>))
import Text.HTML.Scalpel

htmlString :: String
htmlString =
    "<html>\
    \  <body>\
    \    <div class='comments'>\
    \      <div class='comment container'>\
    \        <span class='comment author'>Sally</span>\
    \        <div class='comment text'>Woo hoo!</div>\
    \      </div>\
    \      <div class='comment container'>\
    \        <span class='comment author'>Bill</span>\
    \        <img class='comment image' src='http://example.com/cat.gif' />\
    \      </div>\
    \    </div>\
    \  </body>\
    \</html>"

main :: IO ()
main = do
    -- We can either scrape a raw html of any StringLike type (fetched before by other means):
    let scrapedCommentsFromString = scrapeStringLike htmlString comments
    -- prints: Just [TextComment "Sally" "Woo hoo!",ImageComment "Bill" "http://example.com/cat.gif"]
    print scrapedCommentsFromString

    -- or let Scalpel fetch and scrape an HTML page for us for convenience :
    scrapedCommentsFromUrl <- scrapeURL "http://example.org/article.html" comments
    -- example.org doesn't have the HTML above
    -- prints: Just []
    print scrapedCommentsFromUrl

type Author = String

data Comment
    = TextComment Author String
    | ImageComment Author URL
    deriving (Show, Eq)

comments :: Scraper String [Comment]
comments = chroots ("div" @: [hasClass "container"]) comment
  where
    comment :: Scraper String Comment
    comment = textComment <|> imageComment

    textComment :: Scraper String Comment
    textComment = do
        author <- text $ "span" @: [hasClass "author"]
        commentText <- text $ "div" @: [hasClass "text"]
        return $ TextComment author commentText

    imageComment :: Scraper String Comment
    imageComment = do
        author <- text $ "span" @: [hasClass "author"]
        imageURL <- attr "src" $ "img" @: [hasClass "image"]
        return $ ImageComment author imageURL
```

This example demonstrates the most important features of this library:
You can parse and extract data from raw HTML text or from a webpage
by providing an URL; here we use a hypothetical HTML located at
`"http://example.com/article.html"` to extract a list of all
of the comments.

More examples can be found in the
[examples](https://github.com/fimad/scalpel/tree/master/examples) folder in the
Scalpel git repository.

To understand the code it's important to know that this this library provides
two main building blocks to build web scrapers: Selectors and Scrapers.

Selectors
---------

Selectors describe a location within an HTML DOM tree. The simplest selector,
that can be written is a simple string value. For example, the selector
`"div"` matches every single div node in a DOM. Selectors can be combined
using tag combinators. The `//` operator to define nested relationships within a
DOM tree. For example, the selector `"div" // "a"` matches all anchor tags
nested arbitrarily deep within a div tag.

In addition to describing the nested relationships between tags, selectors can
also include predicates on the attributes of a tag. The `@:` operator creates a
selector that matches a tag based on the name and various conditions on the
tag's attributes. An attribute predicate is just a function that takes an
attribute and returns a boolean indicating if the attribute matches a criteria.
There are several attribute operators that can be used to generate common
predicates. The `@=` operator creates a predicate that matches the name and
value of an attribute exactly. For example, the selector `"div" @: ["id" @=
"article"]` matches div tags where the id attribute is equal to `"article"`.

Scrapers
--------

Scrapers are values that are parameterized over a selector and produce a value
from an HTML DOM tree. The `Scraper` type takes two type parameters. The first
is the string like type that is used to store the text values within a DOM tree.
Any string like type supported by `Text.StringLike` is valid. The second type
is the type of value that the scraper produces.

There are several scraper primitives that take selectors and extract content
from the DOM. Each primitive defined by this library comes in two variants:
singular and plural. The singular variants extract the first instance matching
the given selector, while the plural variants match every instance.


Tips & Tricks
-------------

The primitives provided by scalpel are intentionally minimalistic with the
assumption being that users will be able to build up complex functionality by
combining them with functions that work on existing type classes (Monad,
Applicative, Alternative, etc.).

This section gives examples of common tricks for building up more complex
behavior from the simple primitives provided by this library.

### OverloadedStrings

`Selector`, `TagName` and `AttributeName` are all `IsString` instances, and
thus it is convenient to use scalpel with `OverloadedStrings` enabled. If not
using `OverloadedStrings`, all tag names must be wrapped with `tagSelector`.

### Matching Wildcards

Scalpel has 3 different wildcard values each corresponding to a distinct use case.

- `anySelector` is used to match all tags:

    `textOfAllTags = texts anySelector`

- `AnyTag` is used when matching all tags with some attribute constraint. For
  example, to match all tags with the attribute `class` equal to `"button"`:

    `textOfTagsWithClassButton = texts $ AnyTag @: [hasClass "button"]`

- `AnyAttribute` is used when matching tags with some arbitrary attribute equal
   to a particular value. For example, to match all tags with some attribute
   equal to `"button"`:

    `textOfTagsWithAnAttributeWhoseValueIsButton = texts $ AnyTag @: [AnyAttribute @= "button"]`

### Complex Predicates

It is possible to run into scenarios where the name and attributes of a tag are
not sufficient to isolate interesting tags and properties of child tags need to
be considered.

In these cases the `guard` function of the `Alternative` type class can be
combined with `chroot` and `anySelector` to implement predicates of arbitrary
complexity.

Building off the above example, consider a use case where we would like find the
html contents of a comment that mentions the word "cat".

The strategy will be the following:

1. Isolate the comment div using `chroot`.

2. Then within the context of that div the textual contents can be retrieved
   with `text anySelector`. This works because the first tag within the current context
   is the div tag selected by chroot, and the `anySelector` selector will match the
   first tag within the current context.

3. Then the predicate that `"cat"` appear in the text of the comment will be
   enforced using `guard`. If the predicate fails, scalpel will backtrack and
   continue the search for divs until one is found that matches the predicate.

4. Return the desired HTML content of the comment div.

```haskell
catComment :: Scraper String String
catComment =
    -- 1. First narrow the current context to the div containing the comment's
    --    textual content.
    chroot ("div" @: [hasClass "comment", hasClass "text"]) $ do
        -- 2. anySelector can be used to access the root tag of the current context.
        contents <- text anySelector
        -- 3. Skip comment divs that do not contain "cat".
        guard ("cat" `isInfixOf` contents)
        -- 4. Generate the desired value.
        html anySelector
```

For the full source of this example, see
[complex-predicates](https://github.com/fimad/scalpel/tree/master/examples/complex-predicates/)
in the examples directory.

### Generalized Repetition

The pluralized versions of the primitive scrapers (`texts`, `attrs`, `htmls`)
allow the user to extract content from all of the tags matching a given
selector. For more complex scraping tasks it will at times be desirable to be
able to extract multiple values from the same tag.

Like the previous example, the trick here is to use a combination of the
`chroots` function and the `anySelector` selector.

Consider an extension to the original example where image comments may contain
some alt text and the desire is to return a tuple of the alt text and the URLs
of the images.

The strategy will be the following:

1. to isolate each img tag using `chroots`.

2. Then within the context of each img tag, use the `anySelector` selector to extract
   the alt and src attributes from the current tag.

3. Create and return a tuple of the extracted attributes.

```haskell
altTextAndImages :: Scraper String [(String, URL)]
altTextAndImages =
    -- 1. First narrow the current context to each img tag.
    chroots "img" $ do
        -- 2. Use anySelector to access all the relevant content from the the currently
        -- selected img tag.
        altText <- attr "alt" anySelector
        srcUrl  <- attr "src" anySelector
        -- 3. Combine the retrieved content into the desired final result.
        return (altText, srcUrl)
```

For the full source of this example, see
[generalized-repetition](https://github.com/fimad/scalpel/tree/master/examples/generalized-repetition/)
in the examples directory.

### Operating with other monads inside the Scraper
`ScraperT` is a monad transformer scraper: it allows lifting `m a` operations
inside a `ScraperT str m a` with functions like:

```haskell
-- Particularizes to 'm a -> ScraperT str m a'
lift :: (MonadTrans t, Monad m) => m a -> t m a

-- Particularizes to things like `IO a -> ScraperT str IO a'
liftIO :: MonadIO m => IO a -> m a
```

Example: Perform HTTP requests on page images as you scrape:

1. Isolate images using `chroots`.

2. Within that context of an `img` tag, obtain the `src` attribute containing
   the location of the file.

3. Perform an IO operation to request metadata headers from the source.

4. Use the data to build and return more complex data

```haskell
-- Holds original link and data if it could be fetched
data Image = Image String (Maybe Metadata)
  deriving Show

-- Holds mime type and file size
data Metadata = Metadata String Int
  deriving Show

-- Scrape the page for images: get their metadata
scrapeImages :: URL -> ScraperT String IO [Image]
scrapeImages topUrl = do
    chroots "img" $ do
        source <- attr "src" "img"
        guard . not . null $ source
        -- getImageMeta is called via liftIO because ScrapeT transforms over IO
        liftM (Image source) $ liftIO (getImageMeta topUrl source)
```

For the full source of this example, see
[downloading data](https://github.com/fimad/scalpel/tree/master/examples/image-sizes/)

For more documentation on monad transformers, see the [hackage page](https://hackage.haskell.org/package/transformers)

### Explicit error handling

`ScraperT` is an instance of `MonadError` which allows you to throw errors from
within parsing code to stop parsing and return an error.

When doing error handling in this way, there are 3 cases to consider:
  1. An explicitly thrown error
  2. A failed scraping without a thrown error
  3. A valid result

This can be implemented for `String` valued errors as follows:

```
type Error = String
type ScraperWithError a = ScraperT String (Either Error) a

scrapeStringOrError :: String -> ScraperWithError a -> Either Error a
scrapeStringOrError html scraper
        | Left error    <- result  = Left error
        | Right Nothing <- result  = Left "Unknown error"
        | Right (Just a) <- result = Right a
    where
    result = scrapeStringLikeT html scraper
```

To add explicit erroring you can use the <|> operator from Alternative to throw
an error when something fails:

```
comment :: ScraperWithError Comment
comment = textComment <|> imageComment <|> throwError "Unknown comment type"
```

With this approach, when you throw an error it will stop all parsing. So if you
have an expression `a <|> b` and there is a nested throwError in `a`, then the
parsing will fail. Even if `b` would be successful.

For the full source for this approach, see
[error-handling](https://github.com/fimad/scalpel/tree/master/examples/error-handling/)
in the examples directory.

Another approach that would let you accumulate errors without stopping parsing
would be to use `MonadWriter` and accumulate debugging information in a `Monoid`
like a list:

```
type Error = String
type ScraperWithError a = ScraperT String (Writer [Error]) a

scrapeStringOrError :: String -> ScraperWithError a -> (Maybe a, [Error])
scrapeStringOrError html scraper = runWriter . scrapeStringLikeT
```

Then to log an error you can use `tell`:

```
comment :: ScraperWithError Comment
comment = textComment <|> imageComment <|> (tell ["Unknown comment type"] >> empty)
```

You can also retrieve the current HTML being parsed with `html anySelector` and
incorporate that into your log message:

```
logError :: String -> ScraperWithError a
logError message = do
  currentHtml <- html anySelector
  tell ["Unknown comment type: " ++ html]
  empty

comment :: ScraperWithError Comment
comment = textComment <|> imageComment <|> logError "Unknown comment type: "
```

For the full source for this approach, see
[error-handling-with-writer](https://github.com/fimad/scalpel/tree/master/examples/error-handling-with-writer/)
in the examples directory.

### scalpel-core

The `scalpel` package depends on 'http-client' and 'http-client-tls' to provide
networking support. For projects with an existing HTTP client these dependencies
may be unnecessary.

For these scenarios users can instead depend on
[scalpel-core](https://hackage.haskell.org/package/scalpel-core) which does not
provide networking support and has minimal dependencies.

Troubleshooting
---------------

### My Scraping Target Doesn't Return The Markup I Expected

Some websites return different markup depending on the user agent sent along
with the request. In some cases, this even means returning no markup at all in
an effort to prevent scraping.

To work around this, you can add your own user agent string.

```haskell
#!/usr/local/bin/stack
-- stack runghc --resolver lts-6.24 --install-ghc --package scalpel-0.6.0
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.Scalpel
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Network.HTTP.Types.Header as HTTP


-- Create a new manager settings based on the default TLS manager that updates
-- the request headers to include a custom user agent.
managerSettings :: HTTP.ManagerSettings
managerSettings = HTTP.tlsManagerSettings {
  HTTP.managerModifyRequest = \req -> do
    req' <- HTTP.managerModifyRequest HTTP.tlsManagerSettings req
    return $ req' {
      HTTP.requestHeaders = (HTTP.hUserAgent, "My Custom UA")
                          : HTTP.requestHeaders req'
    }
}

main = do
    manager <- Just <$> HTTP.newManager managerSettings
    html <- scrapeURLWithConfig (def { manager }) url $ htmls anySelector
    maybe printError printHtml html
  where
    url = "https://www.google.com"
    printError = putStrLn "Failed"
    printHtml = mapM_ putStrLn
```

A list of user agent strings can be found
[here](http://www.useragentstring.com/pages/useragentstring.php).
