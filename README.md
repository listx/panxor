#panxor --- numerically XOR any arbitrary values

`panxor` applies the XOR operation; it is primarily meant to be used on top of a hashing utility like sha1sum(1).

For example, you can do something like `sha1sum FILES | panxor --stdin-hex --min-length 40` to XOR all of the hex hashes output by sha1sum into a single 40-character hex string.

#Installation

Use cabal to install dependencies and to build.

