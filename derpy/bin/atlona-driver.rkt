#lang typed/racket/base
;
; Atlona Executable
;

(require racket/cmdline
         racket/match
         typed/json)

(require mordae/syntax
         mordae/match
         mordae/evt
         libserialport
         zmq)

(require derpy/util/zmq)


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


(device-path
  (command-line
    #:program "atlona-driver"
    #:once-each
    (("-i" "--identity") identity "ZeroMQ identity for both endpoints"
                         (endpoint-identity (cast identity String)))

    (("-r" "--rpc-bind") endpoint "RPC endpoint to bind to."
                         (rpc-endpoint (cast endpoint String)))

    (("-p" "--pub-bind") endpoint "Publisher endpoint to bind to"
                         (pub-endpoint (cast endpoint String)))

    #:args (device-path)
    (begin (cast device-path Path-String))))


(: port-number? (-> Any Boolean : #:+ Byte))
(define (port-number? v)
  (and (exact-nonnegative-integer? v)
       (>= v 0)
       (<= v 15)))


(: string->port-number (-> String Byte))
(define (string->port-number str)
  (define value
    (string->number str))

  (unless value
    (error 'string->port-number "~s: not a number" str))

  (assert value byte?))


(: parse-status (-> String (Listof Byte)))
(define (parse-status line)
  (define status : (Vectorof Byte)
    (make-vector 16 0))

  (define parts
    (regexp-match* #rx"x[0-9]+AVx[0-9]+" line))

  (for ((part parts))
    (match-let (((regexp #rx"x([0-9]+)AVx([0-9]+)" (list _ in-str out-str)) part))
      (when* ((in  (string->port-number (cast in-str String)))
              (out (string->port-number (cast out-str String))))
        (vector-set! status
                     (cast (sub1 out) Integer)
                     (cast (sub1 in) Byte)))))

  (vector->list status))


;; Open serial port for communication with the device.
(define-values (in out)
  (open-serial-port (device-path) #:baudrate 115200))


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
        (write-string "PWSTA\r\n" out)
        (write-string "Status\r\n" out))))

  (loop (sync timer)))


;; Listen to client's requests and relay them to the driver.
(: router-main (-> Nothing))
(define (router-main)
  (loop
    (define-values (sender request)
      (socket-receive-json-from router))

    (match request
      ((hash-lookup ('request "status"))
       (write-string "PWSTA\r\n" out)
       (write-string "Status\r\n" out))

      ((hash-lookup ('request "connect")
                    ('input (? port-number? input))
                    ('output (? port-number? output)))
       (parameterize ((current-output-port out))
         (printf "x~aAVx~a\r\n" (add1 input) (add1 output))
         (printf "Status\r\n")))

      ((hash-lookup ('request "online"))
       (write-string "PWON\r\n" out))

      ((hash-lookup ('request "offline"))
       (write-string "PWOFF\r\n" out))

      (else
       (printf "[~a] invalid request: ~s\n" sender request)))))


;; Listen to messages from the device and publish them.
(: pusher-main (-> Nothing))
(define (pusher-main)
  (loop
    (match (read-line in)
      ("PWON\r"
       (socket-send-json pusher (hasheq 'status "online")))

      ("PWOFF\r"
       (socket-send-json pusher (hasheq 'status "offline")))

      ((and (pregexp #px"(x[0-9]+AVx[0-9]+,?){16}") line)
       (let ((status (parse-status line)))
         (socket-send-json pusher (hasheq 'matrix status))))

      ((pregexp #px"x[0-9]+AVx[0-9]+")
       ;; Output has been modified by a client, ignore partial status.
       (void))

      (line
       (printf "-> unknown event: ~s\n" line)))))


;; Wait until something dies.
(void (sync (thread pusher-main)
            (thread router-main)
            (thread ticker-main)))


; vim:set ts=2 sw=2 et:
