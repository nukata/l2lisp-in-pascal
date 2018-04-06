# Experimental Lisp interpreters in ISO Pascal

These are experimental Lisp interpreters I wrote in ISO 7185
Pascal 11 years ago (2007).
I had presented them under the MIT License
at <http://www.oki-osk.jp/esc/llsp/>
until last spring (2017), which has been shut down now.

## How to use & what are features

See [v1.1/README.md](v1.1/README.md) and [v2.0/README.md](v2.0/README.md).
Both are written in Japanese.

They have _tail call optimization_ (v1.1 & v2.0) and
_automatic avoidance of free symbol capture in macro expansion_ (v2.0).


## License

These interpreters are licensed under the MIT License.
See [v1.1/L2Lisp.p](v1.1/L2Lisp.p#L1410-L1433)
and [v2.0/L2Lisp.p](v2.0/L2Lisp.p#L1426-L1449).
