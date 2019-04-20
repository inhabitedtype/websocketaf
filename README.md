# websocketaf

[![Build Status](https://travis-ci.org/inhabitedtype/websocketaf.svg?branch=master)](https://travis-ci.org/inhabitedtype/websocketaf)

websocket/af is a websockets implementation that uses http/af for the initial
connection and upgrade. It currently support a client state machine and is a
work-in-progress.

## Installation

Install the library and its dependencies via [OPAM][opam]:

[opam]: http://opam.ocaml.org/

```bash
opam install websocketaf
```

## Development

To install development dependencies, pin the package from the root of the
repository:

```bash
opam pin add -n websocketaf .
opam install --deps-only websocketaf
```

After this, you may install a development version of the library using the
install command as usual.

Tests can be run via dune:

```bash
dune runtest
```

