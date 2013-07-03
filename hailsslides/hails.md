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
$ hails -a MyWebApp
~~~~

[http://localhost:8080/](http://localhost:8080)

# Hails - static files

* Hails looks for files in the "./static/" subdirectory

* If a path matches a static file, serve that

# Hails - the Frank DSL

~~~~{.haskell}
{-# LANGUAGE OverloadedStrings #-}
module MyWebApp where

import qualified Data.ByteString.Lazy as L

import Hails.HttpServer
import Hails.Web
import Hails.Web.Frank

server :: Application
server = mkRouter $ do
  get "/" $ respond $ okHtml "Hello, World"
  
  get "/echo" $ do
    mping <- queryParam "ping"
    case mping of
      Just ping -> respond $ okHtml ping
      Nothing -> respond badRequest

  get "/hello/:user" $ do
    (Just user) <- queryParam "user"
    respond $ okHtml $ L.fromChunks ["Hello ", user, "!"]
~~~~

# HTML forms: handling user input 

* Interactive controls that enable a user to submit information to a
  web server

~~~~ {.html}
  <form action="..." method="(get|post)" encytype="...">

    ...

  </form>
~~~~

* `action` attribute: URL that will process the info (overriden by
  setting `formaction` attribute on `<button>` or `<input>` element)
* `method` attribute: method to submit the form as
    * `get`: append form data to URI. (Only use this when form has
      _no_ side-effects and is ASCII-only.)
    * `post`: place data in request body
* `enctype` atribute: set the MIME type of form contents.
    * `application/x-www-form-urlencoded`: default
    * `multipart/form-data`: use when allowing file uploads

# HTML forms: handling user input 

* Example:

~~~~
  ./static/name.html:
~~~~

~~~~ {.html}
  <!doctype html>
  <html>
  <head> <title> name </title> </head>
  <body>
    <form action="/echo-name" method="get">
      <input type="text" name="fname">
      <input type="text" name="lname">
      <button type="submit">Send Data</button>
    </form>
  </body>
  </html>
~~~~


  <form action="http://localhost:8080/echo-name" method="get">
    <input type="text" name="fname">
    <input type="text" name="lname">
    <button type="submit">Send Data</button>
  </form>


# The form HTTP request: GET

* Request

~~~~
  GET /echo-name?fname=rick&lname=james HTTP/1.1
  Host: localhost
~~~~

* Log: 

~~~~
  Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
  GET [("fname","rick"),("lname","james")]
  Status: 200 OK. /echo-name
~~~~

* Response:

~~~~
  HTTP/1.1 200 OK
  Server: Warp/1.3.8.2
  Transfer-Encoding: chunked
  Content-Type: text/html

  0011
  Hello rick james!
  0
~~~~


# Hails app handling GET form

~~~~ {.haskell}
{-# LANGUAGE OverloadedStrings #-}
module MyWebApp where

import qualified Data.ByteString.Lazy as L

import Hails.HttpServer
import Hails.Web
import Hails.Web.Frank

server :: Application
server = mkRouter $ do
  routeTop (redirectTo "/static/name.html")
  get "/echo-name" $ do
    mfname <- queryParam "fname"
    mlname <- queryParam "lname"
    case (mfname, mlname) of
      (Just fname, Just lname) -> respond $ 
        okHtml $ L.fromChunks ["Hello ", fname, " ", lname, "!"]
      _       -> respond badRequest
~~~~

* Run the app:

~~~~
$ ghc-pkg trust lio text hails
$ hails -a MyWebApp
~~~~

# The form HTTP request: POST

~~~~
  ./static/name.html:
~~~~

~~~~ {.html}
  ...
    <form action="/echo-name" method="post">
    ...
    </form>
  ...
~~~~

~~~~
  MyWebApp.hs:
~~~~

~~~~ {.haskell}
  ...
  post "/echo-name" $ do
  ...
~~~~

# Simple POST form failure

* Request

~~~~
  POST /echo-name HTTP/1.1
  Host: localhost
  fname=rick&lname=james
~~~~

* Log: 

~~~~
  POST /echo-name
  Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
  POST [("fname","rick"),("lname","james")]
  Status: 400 Bad Request. /echo-name
~~~~

* Response:

~~~~
  HTTP/1.1 400 Bad Request
  Server: Warp/1.3.8.2
  Transfer-Encoding: chunked
  Content-Type: text/html

  00BE
  <!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML 2.0//EN">
  <HTML><HEAD>
  <TITLE>400 Bad Request</TITLE>
  </HEAD><BODY>
  <H1>Bad Request</H1>
  <P>Your request could not be understood.</P>
  </BODY></HTML>

  0
~~~~

# How do we get at the request body?

* Checkout Hails.Web.Controller `request`:

~~~~ {.haskell}
request :: Controller (DCLabeled Request)
~~~~ 

* Requests are labeled to tell you from what user the data is coming
  from

* Request type contains all the data we need:

~~~ {.haskell}
-- | A request sent by the end-user.
data Request = Request {  
    requestMethod  :: Method
 ,  requestHeaders :: RequestHeaders
 ,  requestBody    :: L.ByteString
 ...
 }
~~~~

* Direct extraction of body:

~~~~ {.haskell}
body :: Controller ByteString
~~~~ 

~~~~
  "fname=rick&lname=james"
~~~~

* Manual parsing is annoying...

# Documents

* Document  is a semi-structured data type

* List of field name-value pairs 

~~~~ {.haskell}
type Document = [HsonField]
~~~~

* FieldName: string (`Data.Text`)

~~~~ {.haskell}
data HsonField = HsonField !FieldName HsonValue
~~~~

* Value: float, string, document, blob, etc.

~~~~ {.haskell}
data HsonValue = HsonValue BsonValue
               | HsonLabeled PolicyLabeled
~~~~

* You've seen labeled values, but for now we'll ignore `HsonLabeled`

# Hails BSON values

* BSON is similar to JSON

~~~ {.javascript}
{ "type": 1337, "name": { "first": "rick", "last": "james" } }
~~~

* But has more types. Hails BSON limited to:

~~~~ {.haskell}
data BsonValue = BsonFloat Double
               | BsonString Text
               | BsonDoc BsonDocument
               | BsonArray [BsonValue]
               | BsonBlob Binary
               | BsonObjId ObjectId
               | BsonBool Bool
               | BsonUTC UTCTime
               | BsonNull
               | BsonInt32 Int32
               | BsonInt64 Int64
~~~~

* HsonDocument can contain BsonDocuments
* BsonDocument can contain BsonDocuments
* BsonDocument _cannot_ contain HsonDocuments

# Example

~~~~ {.haskell}
{-# LANGUAGE OverloadedStrings #-}
import Hails.Data.Hson

name = [ "first" -: "rick", "last" -: "james" ] :: BsonDocument
rick = [ "type" -: 1337, "name" -: name] :: HsonDocument
~~~~

* How do we access individual fields?

~~~~ {.haskell}
lookup :: HsonVal v => FieldName -> Document -> m v
~~~~

~~~~ {.haskell}
*Main> lookup "type" rick
1337
*Main> lookup "typez" rick
*** Exception: user error (look: Not found "typez")
*Main> lookup "type" rick :: IO String
*** Exception: user error (fromHsonValue: no conversion)
~~~~ 

* What is `HsonVal`?

~~~~ {.haskell}
class (Typeable a, Show a) => HsonVal a where
  toHsonValue   :: a -> HsonValue
  fromHsonValue :: Monad m => HsonValue -> m a
~~~~

* If you don't care about automatic conversion, use `look`

# Other useful document functions

* Filter fields: only include field names in given list

~~~~ {.haskell}
include :: [FieldName] -> Document -> Document
~~~~

* Filter fields: include all fields but those given list

~~~~ {.haskell}
exclude :: [FieldName] -> Document -> Document
~~~~

* Merge documents:

~~~~ {.haskell}
merge :: Document -> Document -> Document
~~~~

  * Preference is given to first document

* See Hails.Data.Hson for more details


# Thoughs on BSON

* It kind of sucks...

    * Requires type annotations

    * Can use `=:` instead of `-:` to be explicit, but ugly

* Not the most natural way to write code in Haskell

* Key-value data model without nesting would be cleaner

    * Not really document-structured anybody

* In most examples BSON is good enough

* For now: convert to Haskell data-type when dealing with complex data models

~~~~ {.haskell}
import Hails.Database.Structured

instance DCRecord MyType where
  fromDocument :: Document -> Maybe MyType
  fromDocument doc = ....

  toDocument :: MyType -> Document
  toDocument t = ....
~~~~


# One of the most useful BSON functions

~~~~ {.haskell}
labeledRequestToHson :: MonadLIO DCLabel m 
                     => DCLabeled Request -> m (DCLabeled Document)
~~~~

Converts

~~~~
  fname=rick&lname=james
~~~~

to

~~~~ {.haskell}
  [ "fname" -: "rick", "lname" -: "james" ]
~~~~

* BUT, does not know about types

So

~~~~
  type=123&w00t=451
~~~~

is converted to

~~~~ {.haskell}
  [ "type" -: "123", "w00t" -: "451" ]
~~~~

* Use `DCRecord` to convert to Haskell data type

# Simple POST form as an exercise

~~~~
  ./static/name.html:
~~~~

~~~~ {.html}
  ...
    <form action="/echo-name2" method="post">
    ...
    </form>
  ...
~~~~

* Implement `/echo-name2` POST route that implements the
  same functionaility as `/echo-name`

* Helper function:

~~~~ {.haskell}
  hsonRequest :: Controller HsonDocument
  hsonRequest = ???
~~~~

* Hint: you'll need to use an LIO function and `request` from Hails.Web.Controller

# Solution

~~~~ {.haskell}
hsonRequest = request >>= labeledRequestToHson >>= (liftLIO . unlabel)
~~~~

~~~~
  MyWebApp.hs:
~~~~

~~~~ {.haskell}
import Prelude hiding (lookup)
...
import Hails.Data.Hson
import LIO
  ...
  post "/echo-name2" $ do
    doc <- hsonRequest
    let mfname = lookup "fname" doc
        mlname = lookup "lname" doc
    case (mfname, mlname) of
      (Just fname, Just lname) -> respond $ 
        okHtml $ L8.pack $ "Hello " ++ fname ++ " " ++ lname ++ "!"
      _       -> respond badRequest
    
    where hsonRequest = request >>= labeledRequestToHson >>= (liftLIO . unlabel)
~~~~

# More on form elements

* [goto htmlForm;](http://www.teaching-materials.org/htmlcss-1day/lesson3/slides.html#slide26)

* Multiple inputs with same name

    * If you use name with suffix `[]` hails request->document function
      will group those fields into an array

    * Example

~~~~ {.html}
    ...
    <input type="email" name="email[]">
    <input type="email" name="email[]">
    ...
~~~~

~~~~
  [ ...,  "email" -: [ "a@w00t.com", "b@fs.net"], ... ]
~~~~


* What about file uploads?  Create inner document with details:

~~~~
    [ "fileName" -: ...
    , "fileContentType" -: ...
    , "fileContent -: ...  ]
~~~~

* Exercise: wrie an app that prints out the request and demonstrates
  all these cases

# Let's take a look at the SimpleParams example

# Authentication

* Why is the request labeled?

* Hails server labeles request with privilege of logged-in user

* By default:

~~~ {.haskell}
 ...
 lreq <- request
 -- labelOf lreq == True %% True
  ...
~~~

* How do we do authentication?

     * Up to app

     * Example: gitstar uses custom authentication app

     * Example: learnbyhacking uses Mozilla's Persona

     * App can also use OpenID or HTTP base-authentication

* In general:

     * Hails app request login by setting "X-Hails-Login" response header

     * Hails authentication framework handles this and always sets the
       "X-Hails-User" request header to the username of user

# Authentication (contd.)

* Get the current user

~~~~ {.haskell}
getHailsUser :: Controller (Maybe UserName)
~~~~

* Require authentication for certain routes:

~~~~ {.haskell}
...
import qualified Data.Text as T
import Hails.Web.User
...

  ...
  get "/echo" $ withUserOrDoAuth $ \user ->
     respond $ okHtml $ L8.pack $ "Hello " ++ (T.unpack user)
  get "/w00t" $ do 
    muser <- getHailsUser
    ....
  ...
~~~~

* `withUserOrDoAuth` simply sets "X-Hails-Login" header if `getHailsUser` return Nothing

# Interfacing with the DB

* Hails DB is built on top of MongoDB (for now)

    * Need to install and start mongodb

* How is the DB organized?

    * MongoDB has a group of databases
    * Each database has a set of collections
    * Each collection has a set of documents

* In Hails you _must_ specify policy on how database, collection, and documents are accessed

    * Even if policy does nothing!

# MPVC

* Hails code is divinded into two categories

    * View-Controller (VC): what you guys have been building.

        * A view is what the user interacts with (usually HTML)

        * A controller a request handlers (request -> response)

    * Model-Policy (MP): describes the persistent data model

        * Model: what users create, modify, view

        * Policy: how/which users can modify, create, modify

* For today, we'll have a very simple MP:

    * Specify a collection for storing key-value pairs

    * Allow anybody to read/write to the collection

# Key-value store

* We need a bunch of imports...

~~~~ {.haskell}
{-# LANGUAGE DeriveDataTypeable,
             ScopedTypeVariables,
             OverloadedStrings #-}

module SimplePolicyModule (
    StorePolicyModule 
  , withStorePolicyModule
  ) where

import Data.Typeable

import LIO
import LIO.DCLabel
import Hails.Database
import Hails.PolicyModule
import Hails.PolicyModule.DSL
~~~~

# Key-value store (cont.)

* Every MP has a "handler" type used when accessing the DB

~~~~ {.haskell}
data StorePolicyModule = StorePolicyModuleTCB DCPriv
  deriving Typeable
~~~~

# Key-value store (cont.)

* The "handler" type is an instance of `PolicyModule`, i.e., is an MP

* Who can access the database?

~~~~ {.haskell}
instance PolicyModule StorePolicyModule where
  initPolicyModule priv = do
    setPolicy priv $ do
      database $ do
        readers ==> unrestricted
        writers ==> unrestricted
        admins  ==> this
      ...
~~~~

* Old naming scheme: whenever you see PolicyModule, this means MP


# Key-value store (cont.)

* We're going to store our key-value pairs in a collection called "store":

* Anybody can read/write to it

~~~~ {.haskell}
      ...
      collection "store" $ do
        access $ do
          readers ==> unrestricted
          writers ==> unrestricted
        clearance $ do
          secrecy   ==> this
          integrity ==> unrestricted
        document $ \_ -> do
          readers ==> unrestricted
          writers ==> unrestricted
        field "key" key
    return $ StorePolicyModuleTCB priv
      where this = privDesc priv
~~~~

# How do VCs access the MP database?


* Use `withPolicyModule` to execute DB actions for this MP

~~~~ {.haskell}
  withPolicyModule :: PolicyModule pm => (pm -> DBAction a) -> DC a
~~~~

* Example:

~~~~ {.haskell}
...
  withPolicyModule $ \myMP -> do
    fetch ...
    insert ...
    delete ...
...
~~~~

* NOTE: if we do not hide the `StorePolicyModuleTCB` which wraps the
  MP privilege, then any code can get the privilege

* Make sure to always hide the constructor!!

# Custom entrance point

* In general, MP usually exports custom DB interface or entry points

* For our example, let's export `withPolicyModule`:

~~~~ {.haskell}
withStorePolicyModule :: DBAction a -> DC a
withStorePolicyModule act = withPolicyModule (\(_ :: StorePolicyModule) -> act)
~~~~

* VC code can now use our "store" DB:

~~~~ {.haskell}
...
  withStorePolicyModule $ do
    fetch ...
    insert ...
    delete ...
...
~~~~

* NOTE: there is no way to get the MP privilege when wrapping code
  with `withStorePolicyModule`

# Let's look at the DB interface

* [Document-based API](http://hackage.haskell.org/packages/archive/hails/0.11.0.0/doc/html/Hails-Database-Query.html)



# Key-value store (cont.)


* Save the key-value pair

~~~~ {.haskell}
  Frank.post "/store" $ do
    doc <- include ["key","val"] `liftM` hsonRequest
    if length doc /= 2
      then respond badRequest
      else do liftLIO $ withStorePolicyModule $ insert "store" doc
              respond $ redirectTo $ "/store/" ++ ("key" `at` doc)
~~~~

* Fetch the value given the key

~~~~ {.haskell}
  Frank.get "/store/:key" $ do
     ...
~~~~

* Recall

~~~~ {.haskell}
 hsonRequest :: Controller Document
 hsonRequest = request >>= labeledRequestToHson >>= (liftLIO . unlabel)
~~~~

# Html Iframes

* [goto htmlIframe;](http://www.teaching-materials.org/htmlcss-1day/lesson3/slides.html#slide10)

* Update the key-value store such that the top-route `/` displays 2
  iframes

      * First is for storing key-value pairs

      * Second is for fetching values given keys

# Refreshing and redirecting pages with HTML

* Potentially useful info:

* Refresh page every 1 second:

~~~~ {.html}
<meta http-equiv="refresh" content="1">
~~~~

* Redirect page to http://example.com after 1 second

~~~~ {.html}
<meta http-equiv="refresh" content="1; url=http://example.com/">
~~~~

* You can use this for examples, but _avoid_ meta refresh in real apps 
    * We'll see how to update content dynamically with JavaScript later

# More html

* Lists: [goto lists;](http://www.teaching-materials.org/htmlcss-1day/lesson2/slides.html#slide9)

* Internal links: [goto internalLinks;](http://www.teaching-materials.org/htmlcss-1day/lesson2/slides.html#slide18)



# Exercise

* Build a chat app

* Use different routes for 

      1) showing all the posts and 

      2) posting a new comment 

* May want to use `seamless` attribute for `iframe`


# Resources and tools

* IRC
    
      * We hang out on FreeNode/#hails

* Chrome web-dev tool; Firefox inspector and Firebug

* [Mozilla Developer Nework (MDN)](https://developer.mozilla.org/en-US/)

* W3Cschools, but it's less informative though useful while starting out
    
* [TeachingMaterials](http://www.teaching-materials.org/)

* [Eloquent JavaScript](http://eloquentjavascript.net/)

* Git and [Github](http://github.com)

# CSS

  [goto css;](http://www.teaching-materials.org/htmlcss-1day/lesson4/slides.html#slide1)

