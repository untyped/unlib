#lang mzscheme

(require (file "cache-test.ss")
         (file "cache-internal-test.ss")
         (file "contract-test.ss")
         (file "debug-test.ss")
         (file "exn-test.ss")
         (file "file-test.ss")
         (file "generator-test.ss")
         (file "gen-test.ss")
         (file "hash-table-test.ss")
         (file "hash-test.ss")
         (file "lifebox-test.ss")
         (file "list-test.ss")
         (file "log-test.ss")
         (file "number-test.ss")
         (file "pipeline-test.ss")
         (file "preprocess-test.ss")
         (file "project-test.ss")
         (file "profile-test.ss")
         (file "string-test.ss")
         (file "symbol-test.ss")
         (file "test-base.ss")
         (file "time-test.ss")
         (file "trace-test.ss")
         (file "yield-test.ss")
         (file "check/all-check-tests.ss"))

(provide all-unlib-tests)

(define all-unlib-tests
  (test-suite "unlib"
    all-check-tests
    cache-tests
    cache-internal-tests
    contract-tests
    debug-tests
    exn-tests
    file-tests
    generator-tests
    gen-tests
    hash-table-tests
    hash-tests
    lifebox-tests
    list-tests
    log-tests
    number-tests
    pipeline-tests
    preprocess-tests
    profile-tests
    project-tests
    string-tests
    symbol-tests
    time-tests
    trace-tests
    yield-tests))
