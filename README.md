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
