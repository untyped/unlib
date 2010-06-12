#lang scheme/base

(require "bytes-test.ss"
         "cache-test.ss"
         "cache-internal-test.ss"
         "convert-test.ss"
         "contract-test.ss"
         "crc-test.ss"
         "date-test.ss"
         "debug-test.ss"
         "enum-test.ss"
         "enumeration-test.ss"
         "exn-test.ss"
         "file-test.ss"
         "generator-test.ss"
         "gen-test.ss"
         "hash-table-test.ss"
         "hash-test.ss"
         "keyword-test.ss"
         "lifebox-test.ss"
         "list-test.ss"
         "log-test.ss"
         "match-test.ss"
         "number-test.ss"
         "pipeline-test.ss"
         "preprocess-test.ss"
         "project-test.ss"
         "profile-test.ss"
         "string-test.ss"
         "symbol-test.ss"
         "syntax-test.ss"
         "test-base.ss"
         "time-test.ss"
         "trace-test.ss"
         "url-test.ss"
         "yield-test.ss")

; Tests ------------------------------------------

(define/provide-test-suite all-unlib-tests
  bytes-tests
  cache-tests
  cache-internal-tests
  contract-tests
  convert-tests
  crc-tests
  date-tests
  debug-tests
  enum-tests
  enumeration-tests
  exn-tests
  file-tests
  generator-tests
  gen-tests
  hash-table-tests
  hash-tests
  keyword-tests
  lifebox-tests
  list-tests
  log-tests
  match-tests
  number-tests
  pipeline-tests
  preprocess-tests
  profile-tests
  project-tests
  string-tests
  symbol-tests
  syntax-tests
  time-tests
  trace-tests
  url-tests
  yield-tests)