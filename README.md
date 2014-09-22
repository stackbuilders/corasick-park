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

## Current Functionality

* HTTP (JSON) service for describing and applying basic string
  substitutions
* Case sensitive pattern application. The user can describe either
  case-sensitive or non-case-sensitive transformations, and Corasick
  Park quickly finds and applies these transformations by generating
  two state machines - one with the downcased patterns to apply and
  one without modifying case.
* Ability to replace one string or all occurrences in the input
  (`isGlobal` option).
* Selectively apply matches based on the boundaries of the target
  string (specified separately for each side of the match):
  * `none` - there is no requirement for the type of boundary around
    the string
  * `word` - the match must be surrounded by word boundaries (similar
    to `\b` in a Ruby regular expression
  * `line` - there must be a newline at the specified side of the
    match
  * `input` - the input boundary must be on the specified side of the
    match. To specify that the match needs to be exact, just put an
    input boundary on both sides.

## Interface

Post the transformations that you wish to apply to `/operations`:

```json
{ "name": "testbucket", "operations":
  [ { "matchType" : { "isCaseSensitive": false,
                      "isGlobal": true,
                      "leftBoundaryType": "none",
                      "rightBoundaryType": "none" },
      "target": "foo",
      "replacement": "bar" }
  ]
}
```

Then, query the bucket name you created with as many strings as you'd
like to `/transform`:

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

## Author

Justin Leitgeb, Stack Builders Inc.
