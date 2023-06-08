# JSONCache

Simple cache service to cache a single root JSON object and make it's subtrees available
via child resources.

The root object is accessible via the root resources (`/`), while accessing sub-paths
is equivalent to indexing into the root object.

Supported HTTP methods are `GET`, `PUT`, `PATCH`, and `DELETE`.

# Example

The root object is initially set to `null`:

```
$ curl http://localhost:8080/
null
```

Doing a `PUT` to a sub-resource updates that resource and returns its new value:

```
$ curl http://localhost:8080/foo/bar -XPUT -d '{"value": "baz"}'
{"value": "baz"}
```

Parent resources will reflect child modifications:

```
$ curl http://localhost:8080/
{"foo": {"bar": {"value": "baz"}}}
```

`DELETE` clears the specified resource path (successful `DELETE` always returns `null`):

```
$ curl http://localhost:8080/foo/bar/value -X DELETE
null
```
```
$ curl http://localhost:8080/
{"foo": {"bar": {}}}
```

`PATCH` merges objects at the specified resource path:

```
$ curl http://localhost:8080/foo/baz -XPATCH -d '"hello"'
"hello"
```
```
$ curl http://localhost:8080/
{"foo": {"bar": {}}, "baz": "hello"}
```

`PUT` clobbers the specified resource path:

```
$ curl http://localhost:8080/foo -XPUT -d '{"baz": "hello"}'
{"baz": "hello"}
```
```
$ curl http://localhost:8080/
{"foo": {"baz": "hello"}}
```

# Purpose

Built as a toy example to play with STM (Software Transaction Memory).

NOT FOR PRODUCTION USE.
