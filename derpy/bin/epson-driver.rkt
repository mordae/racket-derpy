#lang typed/racket/base
;
; Tesira Executable
;

(require typed/racket/class
         racket/cmdline
         racket/match
         typed/json)

(require mordae/syntax
         mordae/match
         mordae/evt
         esc/vp21
         zmq)

(require derpy/util/zmq
         derpy/util/error)


(define hashjs
  (inst hasheq Symbol JSExpr))


(define-logger client)


(: rpc-endpoint (Parameterof String))
(define rpc-endpoint
  (make-parameter "tcp://127.0.0.1:46111"))

(: rpc-endpoint (Parameterof String))
(define pub-endpoint
  (make-parameter "tcp://127.0.0.1:46211"))

(: endpoint-identity (Parameterof String))
(define endpoint-identity
  (make-parameter "epson"))

(define device
  (command-line
    #:program "epson-driver"
    #:once-each
    (("-i" "--identity") identity "ZeroMQ identity for both endpoints"
                         (endpoint-identity (assert identity string?)))

    (("-r" "--rpc-bind") endpoint "RPC endpoint to bind to."
                         (rpc-endpoint (assert endpoint string?)))

    (("-p" "--pub-bind") endpoint "Publisher endpoint to bind to"
                         (pub-endpoint (assert endpoint string?)))

    #:args (device-address)
    (with-handlers ((exn:fail? (λ (exn)
                                 (fail "Connection to ~a failed" device-address))))
      (new projector% (host (cast device-address String))))))


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


(: push-status (-> Void))
(define (push-status)
  (let ((status  (send device get-power-status))
        (mute?   (send device get-mute?))
        (freeze? (send device get-freeze?)))
    (let ((str-status (symbol->string status)))
      (push (hashjs 'full (hashjs 'status str-status
                                  'mute? mute?
                                  'freeze? freeze?))))))


(: ticker-main (-> Nothing))
(define (ticker-main)
  (define timer
    (wrap-evt (recurring-alarm-evt 5000)
              (λ (now)
                (push-status))))

  (loop (sync timer)))


(: router-main (-> Nothing))
(define (router-main)
  (loop
    (define-values (sender request)
      (socket-receive-json-from router))

    (match request
      ((hash-lookup ('request "status"))
       (push-status))

      ((hash-lookup ('request "online!"))
       (send device online!))

      ((hash-lookup ('request "offline!"))
       (send device offline!))

      ((hash-lookup ('request "set-freeze!")
                    ('freeze? (? boolean? freeze?)))
       (send device set-freeze! freeze?)

       (let ((freeze? (send device get-freeze?)))
         (push (hashjs 'delta (hashjs 'freeze? freeze?)))))

      ((hash-lookup ('request "set-mute!")
                    ('mute? (? boolean? mute?)))
       (send device set-mute! mute?)

       (let ((mute? (send device get-mute?)))
         (push (hashjs 'delta (hashjs 'mute? mute?)))))

      (else
       (log-client-error "[~a] invalid request: ~s" sender request)))))


;; Wait until something dies.
(void (sync (thread router-main)
            (thread ticker-main)))


; vim:set ts=2 sw=2 et:
