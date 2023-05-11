# TOTP

Calculates TOTP code from base32 key.

## Install

```sh
cabal install
```

## Usage

```sh
totp CDUTKNO2LJKKG... # base32 secret key
# 355409
```

Outputs the 6 digit time based code.
