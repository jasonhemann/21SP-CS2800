---
title: ACL2s Reference
layout: single
toc: true
classes: wide
---

## Types

| Name                  | Recognizer             | Description                                                                                              |
| `all`                 | `allp`                 | the ACL2s universe                                                                                       |
| `atom`                | `atom`                 | atomic data: numbers, strings, symbols, characters, ...                                                  |
| `symbol`              | `symbolp`              | symbols                                                                                                  |
| `boolean`             | `booleanp`             | t or nil                                                                                                 |
| `bool`                | `boolp`                | alias for boolean                                                                                        |
| `rational`            | `rationalp`            | rational numbers (we make the simplifying assumption (which is not true) that all numbers are rationals) |
| `integer`             | `integerp`             | integers                                                                                                 |
| `int`                 | `intp`                 | alias for integer                                                                                        |
| `nat`                 | `natp`                 | natural numbers (integers >= 0)                                                                          |
| `pos`                 | `posp`                 | integers > 0                                                                                             |
| `neg`                 | `negp`                 | integers < 0                                                                                             |
| `non-pos-integer`     | `non-pos-integerp`     | integers <= 0                                                                                            |
| `non-0-integer`       | `non-0-integerp`       | integers \ {0}                                                                                           |
| `non-neg-rational`    | `non-neg-rationalp`    | rationals >= 0                                                                                           |
| `pos-rational`        | `pos-rationalp`        | rationals > 0                                                                                            |
| `neg-rational`        | `neg-rationalp`        | rationals < 0                                                                                            |
| `non-pos-rational`    | `non-pos-rationalp`    | rationals <= 0                                                                                           |
| `non-0-rational`      | `non-0-rationalp`      | rationals \ {0}                                                                                          |
| `cons`                | `consp`                | conses, constructed with cons                                                                            |
| `true-list`           | `true-listp`           | true lists (nil terminated cons)                                                                         |
| `tl`                  | `tlp`                  | alias for true-list                                                                                      |
| `list`                | `listp`                | conses or nil                                                                                            |
| `non-empty-true-list` | `non-empty-true-listp` | non-empty true-list                                                                                      |
| `ne-tl`               | `ne-tlp`               | alias for non-empty-true-list                                                                            |

## Function signatures

| Name            | Contract                                  | Description                                                                     |
| `if`            | `All x All x All -> All`                  | if test then else                                                               |
| `equal`         | `All x All -> Bool`                       | checks if args are equal                                                        |
| `implies`       | `All x All -> Bool`                       | implication                                                                     |
| `iff`           | `All x All -> Bool`                       | if and only if (Boolean equality)                                               |
| `xor`           | `All x All -> Bool`                       | xor (Boolean disequality)                                                       |
| `=`             | `Rational x Rational -> Bool`             | checks if args are equal                                                        |
| `/=`            | `Rational x Rational -> Bool`             | checks if args differ                                                           |
| `binary-+`      | `Rational x Rational -> Rational`         | addition                                                                        |
| `binary-*`      | `Rational x Rational -> Rational`         | multiplication                                                                  |
| `<`             | `Rational x Rational -> Bool`             | less than                                                                       |
| `unary--`       | `Rational -> Rational`                    | unary -                                                                         |
| `unary-/`       | `Rational \ {0} -> Rational`              | unary /                                                                         |
| `min`           | `Rational x Rational -> Rational`         | minimum                                                                         |
| `max`           | `Rational x Rational -> Rational`         | maximum                                                                         |
| `expt`          | `(Rational X Integer) \ {(0, i) : i < 0}` | exponentiation                                                                  |
| `numerator`     | `Rational -> Integer`                     | numerator                                                                       |
| `denominator`   | `Rational -> Pos`                         | denominator                                                                     |
| `mod`           | `Rational x Rational \ {0} -> Rational`   | modular arithmetic                                                              |
| `ceiling`       | `Rational x Rational \ {0} -> Integer`    | ceiling of quotient                                                             |
| `floor`         | `Rational x Rational \ {0} -> Integer`    | floor of quotient                                                               |
| `zp`            | `Nat -> Bool`                             | is the number = 0?                                                              |
| `zip`           | `Int -> Bool`                             | is the number = 0?                                                              |
| `zerop`         | `Rational -> Bool`                        | is the number = 0?                                                              |
| `evenp`         | `Integer -> Bool`                         | is the number even?                                                             |
| `oddp`          | `Integer -> Bool`                         | is the number odd?                                                              |
| `endp`          | `List -> Bool`                            | is the list empty?                                                              |
| `lendp`         | `TL -> Bool`                              | is the TL empty?                                                                |
| `cons`          | `All x All -> All`                        | create a cons                                                                   |
| `lcons`         | `All x TL -> NE-TL`                       | create a NE-TL                                                                  |
| `car`           | `List -> All`                             | car of a list, (car nil)=nil                                                    |
| `cdr`           | `List -> All`                             | cdr of a list, (cdr nil)=nil                                                    |
| `left`          | `Cons -> All`                             | car of a cons                                                                   |
| `right`         | `Cons -> All`                             | cdr of a cons                                                                   |
| `head`          | `NE-TL -> All`                            | car of a tl                                                                     |
| `tail`          | `NE-TL -> All`                            | cdr of a tl                                                                     |
| `nth`           | `Nat x TL -> All`                         | nth element; nil if nat too large                                               |
| `nthcdr`        | `Nat x TL -> All`                         | nth cdr                                                                         |
| `binary-append` | `TL x All -> All`                         | concatenation                                                                   |
| `bin-app`       | `TL x TL -> TL`                           | concatenation                                                                   |
| `rev`           | `ALL -> TL`                               | reverse                                                                         |
| `lrev`          | `TL -> TL`                                | reverse a list                                                                  |
| `len`           | `ALL -> Nat`                              | length of anything                                                              |
| `llen`          | `TL -> Nat`                               | length of a true list                                                           |
| `in`            | `ALL x TL -> Bool`                        | (in a X) checks if a is in X                                                    |
| `del`           | `ALL x TL -> TL`                          | (del a X) deletes first occurrence of a from X (not builtin, see lecture notes) |
| `cons-size`     | `ALL -> Nat`                              | returns the "size" of its input (see lecture notes)                             |


## Macros

| Name      | Description                                                      |
| `==`      | abbreviation for `equal`                                         |
| `!=`      | abbreviation for `(not (equal ...))`                             |
| `and`     | arbitrary # of args, expands into `if`s                          |
| `or`      | arbitrary # of args, expands into `if`s                          |
| `cond`    | expand into `if`s                                                |
| `+`       | arbitrary # of args, expands to binary-`+`                       |
| `1+`      | expands to `(+ 1 ...)`                                           |
| `*`       | arbitrary # of args, expands to binary-`*`                       |
| `-`       | 1 (unary--) or 2 (subtraction) args                              |
| `1-`      | expands to `(- ... 1)`                                           |
| `/`       | 1 (unary-/) or 2 (division) args                                 |
| `<=`      | less than or equal, expands into `<`                             |
| `>`       | greater than, expands into `<`                                   |
| `>=`      | greater than or equal, expands into `<`                          |
| `caar`    | `(car (car ...))`                                                |
| `cadr`    | `(car (cdr ...)) `                                               |
| `c****r`  | Can have up to 4 a's d's and expands into nested `car`s/`cdr`s   |
| `first`   | synonym for `car`                                                |
| `second`  | synonym for `cadr`                                               |
| `third`   | synonym for `caddr`, up to tenth supported                       |
| `rest`    | synonym for `cdr`                                                |
| `lcar`    | synonym for `head`                                               |
| `lcdr`    | synonym for `tail`                                               |
| `lcaar`   | `(lcar (lcar ...))`                                              |
| `lcadr`   | `(lcar (lcdr ...))`                                              |
| `lc****r` | Can have up to 4 a's d's and expands into nested `lcar`s/`lcdr`s |
| `lfirst`  | synonym for `lcar`                                               |
| `lsecond` | synonym for `lcadr`                                              |
| `lthird`  | synonym for `lcaddr`, up to `ltenth` supported                   |
| `list`    | create a truelist of 0 or more arguments, expands into `cons`    |
| `append`  | append 0 or more lists, expands into `binary-append`             |
| `app`     | append 0 or more lists, expands into `bin-app`                   |


## Papers 

[https://www.cs.utexas.edu/users/moore/publications/acl2-papers.html](https://www.cs.utexas.edu/users/moore/publications/acl2-papers.html)
