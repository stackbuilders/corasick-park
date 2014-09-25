# Corasick Park

Corasick Park is a server for quickly applying lots of transformations
to strings. By using a simple JSON interface, you can specify the
string replacements that should be made. After you describe the
transformations that should be made, you can hit another JSON endpoint
to quickly apply the transformations that you described.

Corasick Park is able to apply hundreds of thousands of transactions
in less than a second by using
[the Aho-Corasick algorithm](http://en.wikipedia.org/wiki/Aho%E2%80%93Corasick_string_matching_algorithm)
for finding out which transformations apply to the string you
provide. Currently the server uses the
[Aho-Corasick implementation in Haskell](http://hackage.haskell.org/package/AhoCorasick)
for the heavy lifting of finding the correct patterns to apply.

Since it is very quick even to precompile hundreds of thousands of
patterns, the server doesn't persist anything to disk, and uses
Haskell's MVars for safe concurrency.

Corasick Park allows you to precompile patterns in groups by a
'bucket' name so that you can easily choose which set of
transformations to apply for each string.

## Interface

`POST` the transformations that you wish to apply to `/operations`:

```json

{"name": "downcasefoos", "operations":
    [{"target":
        { "text": "foo",
          "isCaseSensitive": false,
          "leftBoundaryType": "none",
          "rightBoundaryType": "none",
          "isGlobal": true
        },
      "transform":{"type": "replace", "replacement": "bar"}
      }]
}
```

Then, apply the transformations you specified by submitting a `POST`
request to the `/transform` endpoint:

```json
{ "name": "testbucket", "input": "foo bar baz" }
```

Result:

```json
{
  "result" : "bar bar baz"
}
```

Since Corasick Park doesn't store anything to disk, clients could
first try posting to a particular bucket to apply a transformation,
and if the server returns a 404 (bucket not found) the client should
post all of the transformations again, and then re-try the
transformation endpoint.

## Supported Target Patterns

Corasick Park doesn't have full support for regular
expressions. However you can customize the patterns that are matched
for replacements by specifying case sensitivity (`isCaseSensitive`
option), global or single replacement (`isGlobal` option), and the
types of boundaries on each side of the pattern you specified. The
currently supported boundaries, which can be supplied to the
`leftBoundaryType` and `rightBoundaryType` option are:

* `none` - there is no requirement for the type of boundary around the
  string
* `word` - the match must be surrounded by word boundaries (similar to
  `\b` in a Ruby regular expression
* `line` - there must be a newline at the specified side of the match
* `input` - the input boundary must be on the specified side of the
    match. To specify that the match needs to be exact, just put an
    input boundary on both sides.

## Supported String Transformations

Corasick Park can apply a variety of transformations to input strings
after efficiently finding the applicable transformations using the
Aho-Corasick state machine. The following transformations are
currently supported:

* `replace` - Replaces the target with another target
* `upcase` - Upper-cases matches of the target in input text
* `downcase` - Lower-cases matches of the target in input text
* `titleize` - Captalizes the first letter in each word of the input
  string
* `truncate trailing` - Removes the text following each match of the string

## Author

Justin Leitgeb, Stack Builders Inc.
