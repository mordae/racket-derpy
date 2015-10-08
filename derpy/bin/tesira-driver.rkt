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
         tesira/util
         tesira/mixer
         tesira
         zmq)

(require derpy/util/zmq
         derpy/util/error)


(define hashjs
  (inst hasheq Symbol JSExpr))


(define-logger client)


(: rpc-endpoint (Parameterof String))
(define rpc-endpoint
  (make-parameter "tcp://127.0.0.1:46102"))

(: rpc-endpoint (Parameterof String))
(define pub-endpoint
  (make-parameter "tcp://127.0.0.1:46202"))

(: endpoint-identity (Parameterof String))
(define endpoint-identity
  (make-parameter "tesira"))

(define address "localhost")
(define alias 'mixer)


(define mixer
  (command-line
    #:program "tesira-driver"
    #:once-each
    (("-i" "--identity") identity "ZeroMQ identity for both endpoints"
                         (endpoint-identity (assert identity string?)))

    (("-r" "--rpc-bind") endpoint "RPC endpoint to bind to."
                         (rpc-endpoint (assert endpoint string?)))

    (("-p" "--pub-bind") endpoint "Publisher endpoint to bind to"
                         (pub-endpoint (assert endpoint string?)))

    #:args (device-address mixer-alias)
    (let ((device-address (cast device-address String))
          (mixer-alias (cast mixer-alias String)))
      (with-handlers ((exn:fail:network?
                        (λ (exn)
                          (fail "Connect to ~a failed" device-address)))
                      (exn:fail?
                        (λ (exn)
                          (fail "Cannot attach to ~a" mixer-alias))))
        (new tesira-mixer%
             (alias (string->symbol mixer-alias))
             (device (tesira-connect device-address)))))))


(: input-number? (-> Any Boolean : #:+ Natural))
(define (input-number? v)
  (and (exact-nonnegative-integer? v)
       (< v (send mixer get-num-inputs))))


(: output-number? (-> Any Boolean : #:+ Natural))
(define (output-number? v)
  (and (exact-nonnegative-integer? v)
       (< v (send mixer get-num-outputs))))


(: level-value? (-> Any Boolean : #:+ Tesira-Number))
(define (level-value? v)
  (and (tesira-number? v)
       (>= v -100.0)
       (<= v   12.0)))


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


(: push-line-status (-> (Instance Tesira-Mixer-Line%) Void))
(define (push-line-status line)
  (let ((kind (if (is-a? line tesira-mixer-input%) 'input 'output))
        (status (send line status))
        (number (sub1 (get-field index line))))
    (push (hashjs 'delta (hashjs kind (hashjs kind number 'status status))))))

(: push-full-status (-> Void))
(define (push-full-status)
  (let ((mixer-status (send mixer status)))
    (push (hashjs 'full (hashjs 'status "online"
                                'mixer mixer-status)))))


(: ticker-main (-> Nothing))
(define (ticker-main)
  (define timer
    (wrap-evt (recurring-alarm-evt 3000)
              (λ (now)
                (push (hashjs 'delta (hashjs 'status "online"))))))

  (loop (sync timer)))


(: status-main (-> Nothing))
(define (status-main)
  (define timer
    (wrap-evt (recurring-alarm-evt 30000)
              (λ (now)
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

      ((hash-lookup ('request "set-input-level!")
                    ('input (? input-number? input-number))
                    ('level (? level-value? level-value)))
       (log-client-info "[~s] set-input-level! ~a ~a"
                        sender input-number level-value)
       (let ((input (send mixer get-input input-number)))
         (send input set-level! level-value)
         (push-line-status input)))

      ((hash-lookup ('request "set-output-level!")
                    ('output (? output-number? output-number))
                    ('level (? level-value? level-value)))
       (log-client-info "[~s] set-output-level! ~a ~a"
                        sender output-number level-value)
       (let ((output (send mixer get-output output-number)))
         (send output set-level! level-value)
         (push-line-status output)))

      ((hash-lookup ('request "set-input-mute!")
                    ('input (? input-number? input-number))
                    ('mute? (? boolean? mute?)))
       (log-client-info "[~s] set-input-mute! ~a ~a"
                        sender input-number mute?)
       (let ((input (send mixer get-input input-number)))
         (send input set-mute! mute?)
         (push-line-status input)))

      ((hash-lookup ('request "set-output-mute!")
                    ('output (? output-number? output-number))
                    ('mute? (? boolean? mute?)))
       (log-client-info "[~s] set-output-mute! ~a ~a"
                        sender output-number mute?)
       (let ((output (send mixer get-output output-number)))
         (send output set-mute! mute?)
         (push-line-status output)))

      ((hash-lookup ('request "set-input-label!")
                    ('input (? input-number? input-number))
                    ('label (? string? label)))
       (log-client-info "[~s] set-input-label! ~a ~s"
                        sender input-number label)
       (let ((input (send mixer get-input input-number)))
         (send input set-label! label)
         (push-line-status input)))

      ((hash-lookup ('request "set-output-label!")
                    ('output (? output-number? output-number))
                    ('label (? string? label)))
       (log-client-info "[~s] set-output-label! ~a ~s"
                        sender output-number label)
       (let ((output (send mixer get-output output-number)))
         (send output set-label! label)
         (push-line-status output)))

      (else
       (log-client-error "[~s] invalid request: ~s" sender request)))))


;; Wait until something dies.
(void (sync (thread router-main)
            (thread ticker-main)
            (thread status-main)))


; vim:set ts=2 sw=2 et:
