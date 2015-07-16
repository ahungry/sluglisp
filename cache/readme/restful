# restful

JSON REST APIs made easy.

In other words, spin up a fully standards-compliant<sup>1</sup> JSON API over HTTP
in Common Lisp by providing a simple class.

Not yet available on quicklisp.

Documentation available [here][0].

An example of an implementation is available [here][4].

<sup>1</sup> Closely follows [RFC 7230][1], [RFC 7231][2] and [RFC 5789][3].

## Dependencies

This library uses (and exposes its usage) the following dependencies:

- [`hunchentoot`][5]: defines a new acceptor to be used with hunchentoot. restful
also assumes that `hunchentoot` is used to handle requests/responses.
- [`jonathan`][6]: parse/render JSON thanks to this library. A selling point
of this library is that an object can define a method to handle its rendering.

The following dependencies are used internally:

- `alexandria` for the hash table facilities.
- `cl-ppcre` for the regexes.
- `closer-mop` to define and handle new slot options.

## Roadmap

- Add HATEOAS support through the `Link` response header
- Define a mongodb/postgresql storage
- Add POST handling on collections (create resources, custom actions)


  [0]: http://rawgit.com/Ralt/restful/master/docs/api.html
  [1]: https://tools.ietf.org/html/rfc7230
  [2]: https://tools.ietf.org/html/rfc7231
  [3]: https://tools.ietf.org/html/rfc5789
  [4]: https://github.com/Ralt/restful-blog
  [5]: http://weitz.de/hunchentoot/
  [6]: https://github.com/Rudolph-Miller/jonathan/
