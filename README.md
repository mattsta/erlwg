erlwg: rate limiting web getter
===============================

Code Guide
----------
Launch a new erlwg using `rlwg_sup:start_getter(GetterName, GetInterval)`.
The `GetInterval` is the minimum number of seconds between successive tries
to get anything in the pool `GetterName`.

Once you started your getter, get things using
`erlwg_server:get(GetterName, ResourceName, URL)`.

Example:

        application:start(erlwg).
        erlwg_sup:start_getter(google_getter, 30).
        erlwg_server:get(google_getter, main_homepage, "http://google.com").
        erlwg_server:get(google_getter, news_homepage, "http://news.google.com").

        erlwg_sup:start_getter(moo_getter, 15, fun(E) -> {moo, E} end).
        erlwg_server:get(moo, home, "http://us.moo.com")

If it has been longer than `GetInterval` since the last pull on `ResourceName`,
erlwg will fetch a new version.  `rlwg_server:get/3` only blocks when no
previous content is available.  Updates are asyncronous to
`erlwg_server:get/3` requests.
You will be returned cached get data until new results are fetched.

`ResourceName`s are not bound to individual URLs.
You can use one name to get multiple URLs.  Names *do*
managing caching though.  If you get `main_homepage` then change the URL, you
will get results for the old `main_homepage` until you run a `get` after
`GetInterval` has elapsed.

The optional third parameter to `erlwg_sup:start_getter/3` is a transformation
function taking one element (the http body).  The return value of the
transformation function is cached once and only refreshed when a new request
is fetched.  Example use: instead of storing the HTML binary directly,
you can parse the HTML into a tree structure that gets auto-updated
on every new fetch.  It's one less thing for your application code to have
to worry about keeping up to date and synchronized with the latest fetched data.
