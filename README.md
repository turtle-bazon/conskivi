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

5K entries, AMD Ryzen 7 3700X, Redis v7 pipelined:

| Operation | conskivi-fileonly | Redis (pipelined) | Redis (single-command) | % vs pipelined | % vs single-command |
|-----------|------------------:|-------------------:|-----------------------:|---------------:|--------------------:|
| SET | 10,163 | 776,636 | ~30,000 | 1.3% | 33.9% |
| GET | 24,038 | 874,359 | ~50,000 | 2.7% | 48.1% |
| SADD | 1,715 | 852,708 | ~30,000 | 0.2% | 5.7% |
| HSET | 5,123 | 1,280,078 | ~30,000 | 0.4% | 17.1% |
| HGET | 21,930 | 923,896 | ~50,000 | 2.4% | 43.9% |
| ZADD | 5,531 | 1,137,284 | ~30,000 | 0.5% | 18.4% |
| ZSCORE | 83,333 | 713,608 | ~50,000 | 11.7% | 166.7% |

| Operation | conskivi-inmemory | Redis (pipelined) | Redis (single-command) | % vs pipelined | % vs single-command |
|-----------|------------------:|-------------------:|-----------------------:|---------------:|--------------------:|
| SET | 1,315,790 | 776,636 | ~30,000 | 169% | 4,386% |
| GET | 2,500,000 | 874,359 | ~50,000 | 286% | 5,000% |
| SADD | 3,124,902 | 852,708 | ~30,000 | 366% | 10,416% |
| HSET | 2,500,000 | 1,280,078 | ~30,000 | 195% | 8,333% |
| HGET | 3,125,000 | 923,896 | ~50,000 | 338% | 6,250% |
| ZADD | 1,562,500 | 1,137,284 | ~30,000 | 137% | 5,208% |
| ZSCORE | 3,571,301 | 713,608 | ~50,000 | 500% | 7,143% |

Note: "Redis (no pipeline)" estimates are for single-command round-trips on localhost (~10-50K ops/s depending on data size).
"Redis (pipelined)" batches 5K commands in one TCP write.

## License

GPL-3.0
