#lang typed/racket/base
;
; Atlona Executable
;

(require racket/cmdline
         racket/string
         racket/match
         typed/json)

(require mordae/syntax
         mordae/match
         mordae/evt
         libserialport
         zmq)

(require derpy/util/zmq)


(define-logger device)
(define-logger client)


(: rpc-endpoint (Parameterof String))
(define rpc-endpoint
  (make-parameter "tcp://127.0.0.1:46101"))

(: rpc-endpoint (Parameterof String))
(define pub-endpoint
  (make-parameter "tcp://127.0.0.1:46201"))

(: device-path (Parameterof Path-String))
(define device-path
  (make-parameter ""))

(: endpoint-identity (Parameterof String))
(define endpoint-identity
  (make-parameter "atlona"))


(define-type Port-Number
  (U 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))

(define-type Output
  (U Port-Number))

(define-type Input
  (U 'null Port-Number))


(define-predicate input? Input)
(define-predicate output? Output)


(device-path
  (command-line
    #:program "atlona-driver"
    #:once-each
    (("-i" "--identity") identity "ZeroMQ identity for both endpoints"
                         (endpoint-identity (assert identity string?)))

    (("-r" "--rpc-bind") endpoint "RPC endpoint to bind to."
                         (rpc-endpoint (assert endpoint string?)))

    (("-p" "--pub-bind") endpoint "Publisher endpoint to bind to"
                         (pub-endpoint (assert endpoint string?)))

    #:args (device-path)
    (begin (assert device-path path-string?))))


(: string->input (-> String Input))
(define (string->input str)
  (let ((value (string->number str)))
    (cond
      ((and value (exact-positive-integer? value))
       (assert (sub1 value) input?))

      (else 'null))))


(: string->output (-> String Output))
(define (string->output str)
  (let ((value (string->number str)))
    (cond
      ((and value (exact-positive-integer? value))
       (assert (sub1 value) output?))

      (else
       (error 'string->output "~s: invalid output" str)))))


(: parse-status (-> String (Listof Input)))
(define (parse-status line)
  (define status : (Vectorof Input)
    (make-vector 16 'null))

  (define parts
    (regexp-match* #rx"x[0-9]+AVx[0-9]+" line))

  (for ((part parts))
    (match-let (((list in-str out-str)
                 (regexp-match* #rx"[0-9]+" part)))
      (let ((input (string->input in-str))
            (output (string->output out-str)))
        (vector-set! status output input))))

  (vector->list status))


;; Open serial port for communication with the device.
(define-values (in out)
  (open-serial-port (device-path) #:baudrate 115200))


(: device-send (-> String Void))
(define (device-send command)
  (parameterize ((current-output-port out))
    (log-device-debug "-> ~a" command)
    (printf "~a\r\n" command)))


(: device-receive (-> String))
(define (device-receive)
  (let ((line (string-trim (assert (read-line in) string?))))
    (log-device-debug "<- ~a" line)
    (values line)))


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


;; Periodically inquires about statuses.
(: ticker-main (-> Nothing))
(define (ticker-main)
  (define timer
    (wrap-evt
      (recurring-alarm-evt 3000)
      (λ (now)
        (device-send "PWSTA")
        (device-send "Status"))))

  (loop (sync timer)))


;; Listen to client's requests and relay them to the driver.
(: router-main (-> Nothing))
(define (router-main)
  (loop
    (define-values (sender request)
      (socket-receive-json-from router))

    (match request
      ((hash-lookup ('request "status"))
       (device-send "PWSTA")
       (device-send "Status"))

      ((hash-lookup ('request "connect")
                    ('input (? input? input))
                    ('output (? output? output)))
       (if (eq? input 'null)
           (device-send (format "x~a$" (add1 output)))
           (device-send (format "x~aAVx~a" (add1 input) (add1 output))))

       (device-send "Status"))

      ((hash-lookup ('request "disable")
                    ('output (? output? output)))
       (device-send (format "x~a$" (add1 output)))
       (device-send "Status"))

      ((hash-lookup ('request "default"))
       (device-send "All#")
       (device-send "Status"))

      ((hash-lookup ('request "reset"))
       (device-send "Mreset"))

      ((hash-lookup ('request "online"))
       (device-send "PWON"))

      ((hash-lookup ('request "offline"))
       (device-send "PWOFF"))

      (else
       (log-client-error "[~a] invalid request: ~s" sender request)))))


;; Listen to messages from the device and publish them.
(: pusher-main (-> Nothing))
(define (pusher-main)
  (loop
    (match (device-receive)
      ("PWON"
       (socket-send-json pusher (hasheq 'status "online")))

      ("PWOFF"
       (socket-send-json pusher (hasheq 'status "offline")))

      ((and (pregexp #px"(x[0-9]+AVx[0-9]+,?){16}") line)
       (let ((status (parse-status line)))
         (socket-send-json pusher (hasheq 'matrix status))))

      ;; Ignore other messages, we can't do anything about them anyway.
      ;; If user wants to debug things, they have already seen them.
      (else (void)))))


;; Wait until something dies.
(void (sync (thread pusher-main)
            (thread router-main)
            (thread ticker-main)))


; vim:set ts=2 sw=2 et:
