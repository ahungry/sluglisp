How To
------
Load South through Quicklisp or ASDF:

```
(ql:quickload :south)
```

First you need to tell South about the service you want to interact with. For this you will need at least the following three URLs
`oauth/request-token`, `oauth/authorize`, `oauth/access-token`
as well as the `api-key` and `api-secret` of your oAuth application that you want to use to connect. To set up South you can either set the according special variables directly or use the `prepare` function:

```
(south:prepare
  :oauth/request-token "https://api.twitter.com/oauth/request_token"
  :oauth/authenticate "https://api.twitter.com/oauth/authenticate"
  :oauth/authorize "https://api.twitter.com/oauth/authorize"
  :oauth/access-token "https://api.twitter.com/oauth/access_token"
  :api-key key
  :api-secret secret)
```

Twitter requires an additional `authenticate` URL, which is otherwise set to the same as the `authorize` address. To start the authentication process simply call `initiate-authentication`. If your addresses and api codes are correct, it should return an URL that the user has to visit in their browser.

```
(south:initiate-authentication)
```

By default this will use the SERVER method, which loads and starts a hunchentoot instance on `*SERVER-PORT*`. After the remote authentication has been accepted it redirects to this local server. If you want to handle the server yourself, you should instead pass a callback URL as the `:method` to `initiate-authentication`. Depending on the service it might provide additional non-standard authentication methods, like twitter's PIN. The PIN method is already integrated into South, but anything beyond that you will have to add yourself. If you choose to use your own server or a different method, you will need to call `complete-authentication` with the verifier and optionally the access-token.

```
(south:complete-authentication verifier :token access-token)
```

The SERVER method will automatically call `complete-authentication` once it receives the request and shuts itself down. If `complete-authentication` returns successfully you should now be all set to perform oAuth requests. To request with oAuth signatures, you can use the `signed-request` function.

```
(south:signed-request "https://api.twitter.com/1.1/account/verify_credentials.json")
```

Depending on how your service requires it, posting form data may require special treatment. South provides a `signed-data-request` function that is geared towards how twitter requires it, but it may also work for other services.

If you need to handle multiple oAuth accounts at the same time you may want to use the `with-oauth-environment` macro to establish dynamic bindings around the internal special variables. The macro accepts parameters for all the environment variables so you may directly set them without needing to call `prepare`.

Applications
------------
If your application uses South, please let me know so I can list it here!

* [Humbler](https://shinmera.github.io/humbler/), a Tumblr API interface library.
