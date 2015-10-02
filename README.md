HAProxy proxy protocol v1.5 support for io-streams.

See the protocol specification:
http://haproxy.1wt.eu/download/1.5/doc/proxy-protocol.txt

To build this package using Cabal directly from git, you must run `autoreconf` before the usual Cabal build steps (configure/build/install). `autoreconf` is included in the [GNU Autoconf](http://www.gnu.org/software/autoconf/) tools. There is no need to run the `configure` script: the `setup configure` step will do this for you.
