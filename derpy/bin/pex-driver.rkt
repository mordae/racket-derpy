#lang typed/racket/base
;
; PEx Driver Executable
;

(require typed/racket/class
         racket/cmdline
         racket/match
         typed/json)

(require mordae/syntax
         mordae/match
         mordae/evt
         pex
         zmq)

(require derpy/util/zmq
         derpy/util/error)


(define hashjs
  (inst hasheq Symbol JSExpr))


(define-logger client)


(: rpc-endpoint (Parameterof String))
(define rpc-endpoint
  (make-parameter "tcp://127.0.0.1:46103"))

(: rpc-endpoint (Parameterof String))
(define pub-endpoint
  (make-parameter "tcp://127.0.0.1:46203"))

(: endpoint-identity (Parameterof String))
(define endpoint-identity
  (make-parameter "pex"))

(define bank
  (command-line
    #:program "pex-driver"
    #:once-each
    (("-i" "--identity") identity "ZeroMQ identity for both endpoints"
                         (endpoint-identity (assert identity string?)))

    (("-r" "--rpc-bind") endpoint "RPC endpoint to bind to."
                         (rpc-endpoint (assert endpoint string?)))

    (("-p" "--pub-bind") endpoint "Publisher endpoint to bind to"
                         (pub-endpoint (assert endpoint string?)))

    #:args (device-path bank-id)
    (let* ((device-path (cast device-path Path-String))
           (bank-id (cast bank-id String))
           (bank (string->number bank-id)))
      (unless (exact-nonnegative-integer? bank)
        (fail "invalid bank ~s, natural number expected" bank-id))
      (new bank% (path device-path) (id bank)))))


(define router
  (socket 'router
          #:bind (list (rpc-endpoint))
          #:identity (endpoint-identity)))

(define pusher
  (socket 'pub
          #:bind (list (pub-endpoint))
          #:identity (endpoint-identity)))


(: push (-> JSExpr Void))
(define (push value)
  (socket-send-json pusher value))


(: relay-status (-> JSExpr))
(define (relay-status)
  (for/list ((relay-id : Positive-Integer (send bank list-relays))) : (Listof JSExpr)
    (let ((relay (send bank get-relay relay-id)))
      (hashjs 'id relay-id
              'on? (send relay get-on?)))))

(: fader-status (-> JSExpr))
(define (fader-status)
  (for/list ((fader-id : Positive-Integer (send bank list-faders))) : (Listof JSExpr)
    (let ((fader (send bank get-fader fader-id)))
      (hashjs 'id fader-id
              'level (send fader get-level)))))


(: push-full-status (-> Void))
(define (push-full-status)
  (push (hashjs 'full (hashjs 'relays (relay-status)
                              'faders (fader-status)))))


(: push-relay-status (-> (Instance Relay%) Void))
(define (push-relay-status relay)
  (let ((status (hashjs 'id (get-field id relay)
                        'on? (send relay get-on?))))
    (push (hashjs 'delta (hashjs 'relay status)))))

(: push-fader-status (-> (Instance Fader%) Void))
(define (push-fader-status fader)
  (let ((status (hashjs 'id (get-field id fader)
                        'level (send fader get-level))))
    (push (hashjs 'delta (hashjs 'fader status)))))


(: ticker-main (-> Nothing))
(define (ticker-main)
  (define timer
    (wrap-evt (recurring-alarm-evt 5000)
              (λ (now)
                (log-client-info "[periodic] status")
                (push-full-status))))

  (loop (sync timer)))


(: router-main (-> Nothing))
(define (router-main)
  (loop
    (define-values (sender request)
      (socket-receive-json-from router))

    (match request
      ((hash-lookup ('request "status"))
       (log-client-info "[~s] status" sender)
       (push-full-status))

      ((hash-lookup ('request "set-on!")
                    ('relay (? exact-positive-integer? relay-id))
                    ('on? (? boolean? on?)))
       (log-client-info "[~s] set-on! ~a ~a" sender relay-id on?)
       (let ((relay (send bank get-relay relay-id)))
         (send relay set-on! on?)
         (push-relay-status relay)))

      ((hash-lookup ('request "set-level!")
                    ('fader (? exact-positive-integer? fader-id))
                    ('level (? exact-nonnegative-integer? level)))
       (log-client-info "[~s] set-level! ~a ~a" sender fader-id level)
       (let ((fader (send bank get-fader fader-id)))
         (send fader set-level! level)
         (push-fader-status fader)))

      ((hash-lookup ('request "fade-to-level!")
                    ('fader (? exact-positive-integer? fader-id))
                    ('level (? exact-nonnegative-integer? level)))
       (log-client-info "[~s] fade-to-level! ~a ~a" sender fader-id level)
       (let ((fader (send bank get-fader fader-id)))
         (send fader fade-to-level! level)
         (push-fader-status fader)))

      (else
       (log-client-error "[~s] invalid request: ~s" sender request)))))


;; Wait until something dies.
(void (sync (thread router-main)
            (thread ticker-main)))


; vim:set ts=2 sw=2 et:
