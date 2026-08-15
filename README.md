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
| SET | 1,208,891 | 885,995 | 34,452 | 136% | 3,508% |
| GET | 1,947,333 | 1,486,920 | 33,133 | 131% | 5,877% |
| SADD | 2,557,008 | 937,567 | 39,967 | 273% | 6,398% |
| HSET | 1,894,216 | 628,491 | 32,971 | 301% | 5,745% |
| HGET | 2,651,661 | 1,495,508 | 33,856 | 177% | 7,832% |
| ZADD | 1,749,710 | 698,096 | 34,201 | 251% | 5,116% |
| ZSCORE | 2,616,690 | 1,405,503 | 36,340 | 186% | 7,199% |

Note: "Redis (pipelined)" batches commands in one TCP write.
"Redis (single-command)" sends one command per TCP round-trip.

### Concurrent (in-memory only)

100 threads, 20M pre-populated keys, 20M ops per operation type, AMD Ryzen 7 3700X, Redis v7 (100 pipelined clients):

| Operation | conskivi-inmemory | Redis (100 clients) | % of Redis |
|-----------|------------------:|--------------------:|-----------:|
| SET | 1,117,062 | 749,732 | 149% |
| GET | 1,065,410 | 1,128,435 | 94% |
| SADD | 1,131,214 | 844,208 | 134% |
| HSET | 931,613 | 663,122 | 141% |
| HGET | 1,298,693 | 831,504 | 156% |
| ZADD | 803,983 | 638,302 | 126% |
| ZSCORE | 1,261,981 | 851,507 | 148% |

Note: Redis uses 100 concurrent pipelined clients over TCP.
conskivi-inmemory uses 100 native SBCL threads with striped locks.

## License

GPL-3.0
