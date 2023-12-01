# owoify_erl
Turning your worst nightmare into a Rebar3 package on Hex.

[![Erlang CI](https://github.com/deadshot465/owoify_erl/actions/workflows/erlang.yml/badge.svg)](https://github.com/deadshot465/owoify_erl/actions/workflows/erlang.yml)

- [Hex package](https://hex.pm/packages/owoify_erl)

This is an Erlang port of [mohan-cao's owoify-js](https://github.com/mohan-cao/owoify-js), which will help you turn any string into nonsensical babyspeak similar to LeafySweet's infamous Chrome extension.

Just like my other Owoify ports, three levels of owoness are available:

1. **owo (default)**: The most vanilla one.
2. **uwu**: The moderate one.
3. **uvu**: Litewawwy unweadabwal.

Please refer to the original [owoify-js repository](https://github.com/mohan-cao/owoify-js) for more information.

Using a Hex package written in Elixir with Rebar3 is not really straightforward. This Erlang port should make the process easier should you ever need owoifying something in Erlang.

## Reason for development
Because Erlang is an old yet still cool language and it deserves a owoify package, as using an Elixir package with Rebar3 in Erlang is not really straightforward. I also found Erlang's Prolog-like syntax can be terser sometimes.

## Installation
owoify_erl is [available on Hex](https://hex.pm/packages/owoify_erl). The package can be installed
by adding `owoify_erl` to your list of dependencies in `rebar.config`:

```erlang
{deps, [owoify_erl]}.
```

Documentation will be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at [https://hexdocs.pm/owoify_erl](https://hexdocs.pm/owoify_erl).

## Usage
owoify_erl is implemented as a function inside the module `owoify_erl`. Erlang is a functional programming language, so you only need to call `owoify_erl:owoify()` to invoke it.

Note that while the source string is accepted as an ASCII string, the string returned by the function will be a `unicode:charlist()` due to Erlang's handling of Unicode texts.

```erlang
io:format("~ts~n", [owoify_erl:owoify("This is the string to owo! Kinda cute isn't it?")]),
io:format("~ts~n", [owoify_erl:owoify("This is the string to owo! Kinda cute isn't it?", uvu)]).

%% Possible output
%% This is teh stwing two owo! Kinda cute isn't it?
%% fwis is teh stwing two owowowouwu Kinda cute isn't it?
```

## Disclaimer
As always, this package is written for practicing and bots' needs. Performance is **NOT** guaranteed.

That being said, PRs are always welcomed.

## See also
- [owoify-js](https://github.com/mohan-cao/owoify-js) - The original owoify-js repository.
- [Owoify.Net](https://www.nuget.org/packages/Owoify.Net/1.0.1) - The C# port of Owoify written by me.
- [Owoify++](https://github.com/deadshot465/OwoifyCpp) - The C++ header-only port of Owoify written by me.
- [owoify_rs](https://crates.io/crates/owoify_rs) - The Rust port of Owoify written by me.
- [owoify-py](https://pypi.org/project/owoify-py/) - The Python port of Owoify written by me.
- [owoify_dart](https://pub.dev/packages/owoify_dart) - The Dart port of Owoify written by me.
- [owoify_rb](https://rubygems.org/gems/owoify_rb) - The Ruby port of Owoify written by me.
- [owoify-go](https://pkg.go.dev/github.com/deadshot465/owoify-go) - The Go port of Owoify written by me.
- [owoifySwift](https://github.com/deadshot465/OwoifySwift) - The Swift port of Owoify written by me.
- [owoifyKt](https://search.maven.org/search?q=g:%22io.github.deadshot465%22%20AND%20a:%22owoifyKt%22) - The Kotlin port of Owoify written by me.
- [owoify_ex](https://hex.pm/packages/owoify_ex) - The Elixir port of Owoify written by me.
- [owoify_cr](https://crystalshards.org/shards/github/deadshot465/owoify_cr) - The Crystal port of Owoify written by me.
- [owoifynim](https://nimble.directory/pkg/owoifynim) - The Nim port of Owoify written by me.
- [owoify-clj](https://clojars.org/net.clojars.deadshot465/owoify-clj) - The Clojure port of Owoify written by me.
- [purescript-owoify](https://pursuit.purescript.org/packages/purescript-owoify) - The PureScript port of Owoify written by me.
- [owoify-hs](https://hackage.haskell.org/package/owoify-hs) - The Haskell port of Owoify written by me.
