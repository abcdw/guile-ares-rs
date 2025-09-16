;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2024, 2025 Andrew Tropin <andrew@trop.in>

(define-module (ares suitbl runner-state)
  #:use-module ((ares atomic)
                #:select
                (atomic-box-update!
                 make-atomic-box
                 atomic-box-ref))
  #:use-module ((ares suitbl core) #:select (test-runner* test?))
  #:use-module ((ares suitbl reporters) #:select (test-reporter-base))
  #:use-module ((srfi srfi-1)
                #:select (alist-delete alist-cons))

  #:use-module ((srfi srfi-197) #:select (chain chain-and))

  #:export (get-stats
            get-loaded-tests
            get-runner-config-value
            set-runner-config-value!
            reset-loaded-tests!
            add-loaded-test!
            get-scheduled-tests
            add-suite!
            save-run-history!
            save-event!
            merge-runner-config))


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
;;; Test runner state management
;;;

(define (save-run-history! state run-history)
  (update-atomic-alist-value!
   state 'runner/run-history (lambda (_) run-history)))

(define (save-event! state event)
  (update-atomic-alist-value!
   state 'events
   (lambda (l)
     (cons event (or l '())))))

(define (add-suite! state suite)
  (update-atomic-alist-value!
   state 'suite (lambda (_) suite)))

(define (merge-runner-config cfg1 cfg2)
  (append cfg1 cfg2))

(define (get-loaded-tests state)
  (chain (atomic-box-ref state)
    (assoc-ref _ 'loaded-tests)
    (or _ '())))

(define (add-loaded-test! state test)
  (update-atomic-alist-value!
   state 'loaded-tests
   (lambda (l) (cons test (or l '())))))

(define (reset-loaded-tests! state)
  (update-atomic-alist-value!
   state 'loaded-tests
   (lambda (l) '())))

(define (select-interesting-tests lot)
  (filter (lambda (t)
            (chain-and t
              (assoc-ref _ 'test/metadata)
              (assoc-ref _ 'slow?)))
          lot))

(define (get-scheduled-tests state runner-config)
  (let ((lot-transformation identity))
    (chain (atomic-box-ref state)
      (assoc-ref _ 'loaded-tests)
      (or _ '())
      (lot-transformation _))))

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

(define (get-stats state)
  (let* ((state-val (atomic-box-ref state))
         (loaded-tests-count (chain state
                               (get-loaded-tests _)
                               (length _))))
    `((loaded-tests-count . ,loaded-tests-count)
      (selected-tests-count . ,loaded-tests-count))))
