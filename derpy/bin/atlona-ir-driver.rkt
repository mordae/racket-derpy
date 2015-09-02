#lang typed/racket/base
;
; Atlona Executable
;

(require racket/cmdline
         racket/string
         racket/match
         racket/tcp
         typed/json)

(require mordae/syntax
         mordae/match
         mordae/evt
         zmq)

(require derpy/util/zmq
         derpy/util/error)


(define hashjs
  (inst hasheq Symbol JSExpr))


(define-logger device)
(define-logger client)


(: rpc-endpoint (Parameterof String))
(define rpc-endpoint
  (make-parameter "tcp://127.0.0.1:46101"))

(: rpc-endpoint (Parameterof String))
(define pub-endpoint
  (make-parameter "tcp://127.0.0.1:46201"))

(: endpoint-identity (Parameterof String))
(define endpoint-identity
  (make-parameter "atlona"))


(define-type Port-Number
  (U 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))

(define-type Output Port-Number)
(define-type Input  Port-Number)


(define-predicate input? Input)
(define-predicate output? Output)


(define extron-host
  (command-line
    #:program "atlona-ir-driver"
    #:once-each
    (("-i" "--identity") identity "ZeroMQ identity for both endpoints"
                         (endpoint-identity (assert identity string?)))

    (("-r" "--rpc-bind") endpoint "RPC endpoint to bind to."
                         (rpc-endpoint (assert endpoint string?)))

    (("-p" "--pub-bind") endpoint "Publisher endpoint to bind to"
                         (pub-endpoint (assert endpoint string?)))

    #:args (host)
    (assert host string?)))


(: string->input (-> String Input))
(define (string->input str)
  (let ((value (string->number str)))
    (and (assert value exact-positive-integer?)
         (assert (sub1 value) input?))))

(: string->output (-> String Output))
(define (string->output str)
  (let ((value (string->number str)))
    (and (assert value exact-positive-integer?)
         (assert (sub1 value) output?))))


(: parse-status (-> String (Listof Input)))
(define (parse-status line)
  (define status : (Vectorof Input)
    (make-vector 16 0))

  (define parts
    (regexp-match* #rx"x[0-9]+AVx[0-9]+" line))

  (for ((part parts))
    (match-let (((list in-str out-str)
                 (regexp-match* #rx"[0-9]+" part)))
      (let ((input (string->input in-str))
            (output (string->output out-str)))
        (vector-set! status output input))))

  (vector->list status))


;; Connect to the Extron device for IR control.
(define-values (in out)
  (with-handlers ((exn:fail? (λ (exn)
                               (fail "Connection to ~a failed" extron-host))))
    (tcp-connect extron-host 23)))

;; Lock to prevent concurrent access to the socket.
(define lock
  (make-semaphore 1))

;; Discard welcome lines.
(void (read-line in 'any)
      (read-line in 'any)
      (read-line in 'any))


(: device-send (-> String Void))
(define (device-send command)
  (with-semaphore lock
    (parameterize ((current-output-port out))
      (log-device-debug "-> ~a" command)
      (printf "W~a|" command)
      (flush-output)
      (void (read-line in 'any)))))

(: device-connect (-> Input Output Void))
(define (device-connect in-port out-port)
  (vector-set! device-state out-port in-port)

  (device-send (format "1,1,~a,0IR" (+ 17 out-port)))
  (sleep 0.5)

  (device-send (format "1,1,~a,0IR" (add1 in-port)))
  (sleep 4.0))

(: device-state (Vectorof Input))
(define device-state
  (for/vector ((i 16)) : Input
    (cast i Input)))


;; Reset the matrix before starting.
(log-device-info "resetting matrix")
(for ((i 16))
  (device-connect (cast i Input) (cast i Output)))


;; To receive commands.
(define router
  (socket 'router
          #:bind (list (rpc-endpoint))
          #:identity (endpoint-identity)))

;; To notify clients of events.
(define pusher
  (socket 'pub
          #:bind (list (pub-endpoint))
          #:identity (endpoint-identity)))


(: report-status (-> Void))
(define (report-status)
  (let ((matrix (vector->list device-state)))
    (socket-send-json pusher (hashjs 'full (hashjs 'status "online"
                                                   'matrix matrix)))))


;; Report our cached status.
(: ticker-main (-> Nothing))
(define (ticker-main)
  (define timer
    (wrap-evt
      (recurring-alarm-evt 3000)
      (λ (now)
        (device-send "1PC")
        (report-status))))

  (loop (sync timer)))


;; Listen to client's requests and relay them to the device.
(: router-main (-> Nothing))
(define (router-main)
  (log-client-info "accepting client requests")
  (loop
    (define-values (sender request)
      (socket-receive-json-from router))

    (match request
      ((hash-lookup ('request "status"))
       (report-status))

      ((hash-lookup ('request "connect!")
                    ('input (? input? input))
                    ('output (? output? output)))
       (device-connect input output)
       (report-status))

      ((hash-lookup ('request "default!"))
       (for ((i 16))
         (device-connect (cast i Input) (cast i Output)))
       (report-status))

      (else
       (log-client-error "[~s] invalid request: ~s" sender request)))))


;; Wait until something dies.
(void (sync (thread router-main)
            (thread ticker-main)))


; vim:set ts=2 sw=2 et:
