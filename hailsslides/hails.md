% Intro to WWW and Hails

# The HTTP protocol

* The protocol browsers use to talk to servers

* Browser sends a request:

    * Verb (GET, POST,...)

    * Path (/search?q=foo)
    
    * Header ("Host: www.google.com",...)

    * Request Body (form data, file upload, ...)

* Server responds:

    * Status (200 OK, 404 Not Found, ...)

    * Headers (Expiration, Set-Cookie, ...)

    * Response Body (usually HTML to display)

# HTTP Protocol in action

~~~~
$ nc www.scs.stanford.edu 80
GET / www.scs.stanford.edu
Host: www.scs.stanford.edu


~~~~

# Structure of a URL

~~~~
http://www.scs.stanford.edu/~dm/addr/
~~~~

* A URL is composed of:

  1. Protocol -- HTTP or HTTPS
  2. Host (www.scs.stanford.edu)
  3. Optional port (defaults to 80 for HTTP and 443 for HTTPS)
  4. Path -- everything after the host (/~dm/addr/)

~~~~
GET /~dm/addr/ HTTP/1.1
Host: www.scs.stanford.edu

~~~~

* By convention, everything after a "?" is treated as a `query string`:

    ~~~~
    http://www.google.com/search?q=cat+pics
    ~~~~

    * Format: "name1=value1&name2=value2&name3=value3"

# HTTP Verbs

* Two most common verbs (basically only verbs supported in browsers):

  1. `GET` - fetch a resource, like a web page
  3. `POST` - modify a resource

* Plenty of others -- `PUT`, `DELETE`, `HEAD`, `LIST`, `PROPFIND`,...

* The meaning of these verbs is just a convention, but a very strongly kept
  convention, and assumed by browsers:

    * `GET` -- whenever you click a link, load an image, etc

    * `POST` -- only when you submit a form or upload a file

# (Extremely) Basic HTML

Nothing about HTTP dictates what types of resources it serves, but the vast
majority of HTTP servers, serve web pages written in HTML.

* HTML is an XML like markup language

    ~~~~
    <html>
    <head>
      <title>My website!</title>
    </head>
    <body>
      <h1>Welcome to my website</h1>
      <p>
        I can write HTML! Here is a <blink>link tag</blink>
      </p>
    </body>
    </html>
    ~~~~

# HTML elements

* `<html></html>` - encloses an HTML document
* `<head></head>` - encloses things that do not appear on the page (like the title)
* `<body></body>` - the page itself
* `<p></p>`       - a paragraph
* `<h1></h1>`     - a top level header (also `h2`, `h3`,...)
* `<emph></emph>` - emphazised text (by default displayed in italics)
* `<strong></strong>` - strongly emphazised text (by default displayed bold)

* The link tag `a`:

    ~~~~
    <a href="http://www.scs.stanford.edu/">The SCS Home Page</a>
    ~~~~

    * Can also do relative links:

    ~~~~
    <a href="/other/page/on/same/host.html">The SCS Home Page</a>
    ~~~~


# Hails

A framework for building web applications based on LIO

* To install:

~~~~
$ cabal install hails
~~~~

* Our first application

~~~~{.haskell}
{-# LANGUAGE OverloadedStrings #-}
module MyWebApp where

import Hails.HttpServer

server :: Application
server _ _ = return $ okHtml "Hello, World"
~~~~

~~~~
$ hails -s MyWebApp
~~~~

[http://localhost:8080/](http://localhost:8080)

# Hails - static files

* Hails looks for files in the "./static/" subdirectory

* If a path matches a static file, serve that

# Hails - the Frank DSL

~~~~{.haskell}
{-# OverloadedStrings #-}
module MyWebApp where

import qualified Data.ByteString.Lazy as L

import Hails.HttpServer
import Hails.Web
import Hails.Web.Frank

server :: Application
server = mkRouter $ do
  get "/" respond $ okHtml "Hello, World"
  
  get "/echo" $ do
    mping <- queryParam "ping"
    case mping of
      Just ping -> respond $ okHtml ping
      Nothing -> badRequest

  get "/hello/:user" $ do
    (Just user) <- queryParam "user"
    respond $ okHtml $ L.fromChunks ["Hello ", user, "!"]
~~~~

