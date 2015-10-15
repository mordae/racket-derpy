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
    (let ((device-address (cast device-address String)))
      (with-handlers ((exn:fail?
                        (λ (exn)
                          (fail "Connection to ~a failed" device-address))))
        (new projector% (host device-address))))))


(define router
  (socket 'router
          #:bind (list (rpc-endpoint))
          #:identity (endpoint-identity)))

(define pusher
  (socket 'pub
          #:bind (list (pub-endpoint))
          #:identity (endpoint-identity)))


(define-type Aspect-String
  (U "normal" "4:3" "16:9" "auto" "full" "zoom" "native"))

(define-type Format-String
  (U "4:3" "16:9" "16:10"))

(define-predicate aspect-string? Aspect-String)
(define-predicate format-string? Format-String)


(: push (-> JSExpr Void))
(define (push value)
  (socket-send-json pusher value))


(: push-status (-> Void))
(define (push-status)
  (let ((status  (send device get-power-status))
        (mute?   (send device get-mute?))
        (freeze? (send device get-freeze?))
        (aspect  (send device get-aspect))
        (format  (send device get-format)))
    (let ((str-status (symbol->string status))
          (str-aspect (symbol->string aspect))
          (str-format (symbol->string format)))
      (push (hashjs 'full (hash-set
                            (hashjs 'status str-status
                                    'aspect str-aspect
                                    'mute? mute?
                                    'freeze? freeze?)
                            'format str-format))))))


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
       (log-client-info "[~s] status" sender)
       (push-status))

      ((hash-lookup ('request "online!"))
       (log-client-info "[~s] online!" sender)
       (send device online!))

      ((hash-lookup ('request "offline!"))
       (log-client-info "[~s] offline!" sender)
       (send device offline!))

      ((hash-lookup ('request "set-aspect!")
                    ('aspect (? aspect-string? str-aspect)))
       (log-client-info "[~s] set-aspect! ~a" sender str-aspect)
       (let ((aspect (cast (string->symbol str-aspect) Projector-Aspect)))
         (send device set-aspect! aspect))

       (let ((aspect (send device get-aspect)))
         (push (hashjs 'delta (hashjs 'aspect (symbol->string aspect))))))

      ((hash-lookup ('request "set-format!")
                    ('format (? format-string? str-format)))
       (log-client-info "[~s] set-format! ~a" sender str-format)
       (let ((format (cast (string->symbol str-format) Projector-Format)))
         (send device set-format! format))

       (let ((format (send device get-format)))
         (push (hashjs 'delta (hashjs 'format (symbol->string format))))))

      ((hash-lookup ('request "set-freeze!")
                    ('freeze? (? boolean? freeze?)))
       (log-client-info "[~s] set-freeze! ~a" sender freeze?)
       (send device set-freeze! freeze?)

       (let ((freeze? (send device get-freeze?)))
         (push (hashjs 'delta (hashjs 'freeze? freeze?)))))

      ((hash-lookup ('request "set-mute!")
                    ('mute? (? boolean? mute?)))
       (log-client-info "[~s] set-mute! ~a" sender mute?)
       (send device set-mute! mute?)

       (let ((mute? (send device get-mute?)))
         (push (hashjs 'delta (hashjs 'mute? mute?)))))

      (else
       (log-client-error "[~s] invalid request: ~s" sender request)))))


;; Wait until something dies.
(void (sync (thread router-main)
            (thread ticker-main)))


; vim:set ts=2 sw=2 et:
