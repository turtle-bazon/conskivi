# conskivi

A Redis-like key-value database in Common Lisp with multiple storage backends.

## Structure

```
conskivi-core/        — Generic interface (conskivi-put, conskivi-get, conskivi-sadd, ...)
conskivi-fileonly/    — File-based backend with B+tree for compound types
conskivi-inmemory/    — Pure in-memory backend (reference implementation)
```

## Supported Types

- Strings, Integers, Floats, Booleans, Keywords
- Lists (linked list of array blocks)
- Sets (B+tree per key)
- Hashes (B+tree per key)
- Sorted Sets (B+tree + skiplist per key)

## Installation

Available via Quicklisp. See [quicklisp-bazon-public](https://github.com/turtle-bazon/quicklisp-bazon-public) for setup instructions.

## Usage

```lisp
;; Load
(ql:quickload :conskivi-core)
(ql:quickload :conskivi-fileonly)

;; Start
(defvar *db* (make-instance 'conskivi-fileonly:conskivi-fileonly-database
                            :name "mydb"
                            :db-path #p"/path/to/data/"))
(conskivi-fileonly:conskivi-start *db*)

;; Operations
(conskivi-core:conskivi-put *db* :name "Alice")
(conskivi-core:conskivi-get *db* :name)        ; => "Alice"

(conskivi-core:conskivi-sadd *db* :tags "lisp")
(conskivi-core:conskivi-smembers *db* :tags)   ; => ("lisp")

(conskivi-core:conskivi-hset *db* :user :age 30)
(conskivi-core:conskivi-hget *db* :user :age)  ; => 30

(conskivi-core:conskivi-zadd *db* :scores 100.0 "alice")
(conskivi-core:conskivi-zscore *db* :scores "alice") ; => 100.0

;; Save and stop
(conskivi-fileonly:conskivi-save *db*)
(conskivi-fileonly:conskivi-stop *db*)
```

## Testing

```lisp
(asdf:load-system :conskivi-fileonly-tests)
(fiveam:explain! (fiveam:run 'conskivi-fileonly-tests::all-tests))
```

169/169 tests passing.

## Benchmarks

5K entries, AMD Ryzen 7 3700X, Redis v7:

| Operation | conskivi-fileonly | Redis (pipelined) | Redis (single-command) | % vs pipelined | % vs single-command |
|-----------|------------------:|-------------------:|-----------------------:|---------------:|--------------------:|
| SET | 8,803 | 885,995 | 34,452 | 1.0% | 25.5% |
| GET | 19,531 | 1,486,920 | 33,133 | 1.3% | 58.9% |
| SADD | 1,580 | 937,567 | 39,967 | 0.2% | 4.0% |
| HSET | 4,613 | 628,491 | 32,971 | 0.7% | 14.0% |
| HGET | 19,531 | 1,495,508 | 33,856 | 1.3% | 57.7% |
| ZADD | 4,789 | 698,096 | 34,201 | 0.7% | 14.0% |
| ZSCORE | 65,789 | 1,405,503 | 36,340 | 4.7% | 181.0% |

| Operation | conskivi-inmemory | Redis (pipelined) | Redis (single-command) | % vs pipelined | % vs single-command |
|-----------|------------------:|-------------------:|-----------------------:|---------------:|--------------------:|
| SET | 1,250,000 | 885,995 | 34,452 | 141% | 3,628% |
| GET | 2,272,676 | 1,486,920 | 33,133 | 153% | 6,859% |
| SADD | 3,125,000 | 937,567 | 39,967 | 333% | 7,819% |
| HSET | 2,500,000 | 628,491 | 32,971 | 398% | 7,582% |
| HGET | 3,125,000 | 1,495,508 | 33,856 | 209% | 9,230% |
| ZADD | 1,470,567 | 698,096 | 34,201 | 211% | 4,299% |
| ZSCORE | 3,125,000 | 1,405,503 | 36,340 | 222% | 8,599% |

Note: "Redis (pipelined)" batches 5K commands in one TCP write.
"Redis (single-command)" sends one command per TCP round-trip.

## License

GPL-3.0
