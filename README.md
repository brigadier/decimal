# decimal
An Erlang decimal arithmetic library.

fork of https://github.com/egobrain/decimal/ with decimal representation changed from {Base, Exp} to #{decimal, {base = Base, exp = Exp}}.

- (good) can dispatch on tag
- (bad) about 10% slower on some operations, depending on precision.    