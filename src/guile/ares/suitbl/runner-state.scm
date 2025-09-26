;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2024, 2025 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl runner-state)
  #:use-module ((ares atomic)
                #:select
                (atomic-box-update!
                 make-atomic-box
                 atomic-box-ref))
  #:use-module ((ares suitbl definitions) #:select (test-runner* test?))
  #:use-module ((ares suitbl reporters) #:select (test-reporter-base))
  #:use-module ((srfi srfi-1)
                #:select (alist-delete alist-cons))
  #:use-module ((ice-9 match) #:select (match))

  #:use-module ((srfi srfi-197) #:select (chain chain-and))

  #:export (save-event!
            get-log

            add-loaded-test!
            make-suite-node
            make-test-node
            add-suite-tree!
            get-suite-forest
            reset-loaded-tests!
            get-loaded-tests
            get-scheduled-tests
            get-stats

            save-run-history!
            get-run-history
            get-run-summary

            get-runner-config-value
            set-runner-config-value!
            merge-runner-config))


;;;
;;; Helpers
;;;

(define (update-alist-value alist key value)
  (chain alist
    (alist-delete key _)
    (alist-cons key value _)))

(define (update-atomic-alist-value! alist-atom key f)
  (atomic-box-update!
   alist-atom
   (lambda (alist)
     (let* ((value (or (assoc-ref alist key) #f))
            (new-value (f value)))
       (update-alist-value alist key new-value)))))


;;;
;;; Logging
;;;

(define (save-event! state event)
  (update-atomic-alist-value!
   state 'events
   (lambda (l)
     (cons event (or l '())))))

(define (get-log state)
  (reverse
   (chain state
     (atomic-box-ref _)
     (assoc-ref _ 'events)
     (or _ '()))))


;;;
;;; Suite tree
;;;

(define (suite-node? x)
  (and (list? x)
       (assoc-ref x 'suite)
       (assoc-ref x 'suite-node/children)))

(define (make-suite-node suite children)
  `((suite . ,suite)
    (suite-node/children . ,children)))

(define (test-node? x)
  (and (list? x)
       (assoc-ref x 'test)))

(define (make-test-node test)
  `((test . ,test)))


;;;
;;; Loaded and scheduled tests and suites
;;;

(define (add-loaded-test! state test)
  (update-atomic-alist-value!
   state 'runner/loaded-tests
   (lambda (l) (cons test (or l '())))))

(define (add-suite-tree! state suite-tree)
  (update-atomic-alist-value!
   state 'runner/suite-forest
   (lambda (forest) (cons suite-tree (or forest '())))))

(define (get-suite-forest state)
  (reverse
   (chain (atomic-box-ref state)
     (assoc-ref _ 'runner/suite-forest)
     (or _ '()))))

(define (reset-loaded-tests! state)
  (atomic-box-update!
   state
   (lambda (alist)
     (chain alist
       (update-alist-value _ 'runner/loaded-tests '())
       (update-alist-value _ 'runner/suite-forest '())))))

(define (get-loaded-tests state)
  (chain (atomic-box-ref state)
    (assoc-ref _ 'runner/loaded-tests)
    (or _ '())))

(define (select-interesting-tests lot)
  (filter (lambda (t)
            (chain-and t
              (assoc-ref _ 'test/metadata)
              (assoc-ref _ 'slow?)))
          lot))

(define (get-scheduled-tests state runner-config)
  (let ((lot-transformation identity))
    (chain (get-loaded-tests state)
      (lot-transformation _))))

(define (get-stats state)
  (let* ((state-val (atomic-box-ref state))
         (loaded-tests-count (chain state
                               (get-loaded-tests _)
                               (length _))))
    `((loaded-tests-count . ,loaded-tests-count)
      (selected-tests-count . ,loaded-tests-count))))


;;;
;;; Run history and summaries
;;;

(define (save-run-history! state run-history)
  (update-atomic-alist-value!
   state 'runner/run-history (lambda (_) run-history)))

(define (get-run-history state)
  (chain state
    (atomic-box-ref _)
    (assoc-ref _ 'runner/run-history)))

(define initial-run-summary
  `((tests . 0)
    (failures . 0)
    (errors . 0)
    (skipped . 0)
    (assertions . 0)))

(define (merge-run-summaries s1 s2)
  (map
   (lambda (v)
     (match v
       ((key . value)
        (cons key (+ (assoc-ref s2 key) value)))))
   s1))

(define (get-run-summary state)
  (define run-history (get-run-history state))

  (if run-history
      (let loop ((summary initial-run-summary)
                 (remaining-items run-history))
        (if (null? remaining-items)
            summary
            (let ((item (car remaining-items)))
              (loop
               (merge-run-summaries summary (assoc-ref item 'test-run/result))
               (cdr remaining-items)))))
      #f))


;;;
;;; Test runner config
;;;

(define (merge-runner-config cfg1 cfg2)
  (append cfg1 cfg2))

(define (get-runner-config state)
  (chain (atomic-box-ref state)
    (assoc-ref _ 'runner/config)
    (or _ '())))

(define (get-runner-config-value state key)
  (chain-and (atomic-box-ref state)
    (assoc-ref _ 'runner/config)
    (assoc-ref _ key)))

(define (set-runner-config-value! state key value)
  (update-atomic-alist-value!
   state 'runner/config
   (lambda (alist) (update-alist-value (or alist '()) key value))))
