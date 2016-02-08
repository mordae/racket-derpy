#lang typed/racket/base
;
; Atlona Executable
;

(require racket/cmdline
         racket/string
         racket/match
         typed/racket/class
         typed/json)

(require mordae/syntax
         mordae/match
         mordae/evt
         cuecore
         zmq)

(require derpy/util/zmq)


(define hashjs
  (inst hasheq Symbol JSExpr))


(define-logger client)


(: rpc-endpoint (Parameterof String))
(define rpc-endpoint
  (make-parameter "tcp://127.0.0.1:46104"))

(: rpc-endpoint (Parameterof String))
(define pub-endpoint
  (make-parameter "tcp://127.0.0.1:46204"))

(: device-host (Parameterof String))
(define device-host
  (make-parameter ""))

(: endpoint-identity (Parameterof String))
(define endpoint-identity
  (make-parameter "lighting"))


(define-type Light
  (U "left" "right"))

(define-type Lamp
  (U "on" "off"))

(define-type Color
  (U "white" "dark-green" "red" "light-azure" "magenta" "uv-filter"
     "yellow" "green" "pink" "blue" "deep-red" "random"))

(define-type Shutter
  (U "closed" "open"))


(define-predicate light? Light)
(define-predicate lamp? Lamp)
(define-predicate color? Color)
(define-predicate shutter? Shutter)


(device-host
  (command-line
    #:program "lighting-driver"
    #:once-each
    (("-i" "--identity") identity "ZeroMQ identity for both endpoints"
                         (endpoint-identity (assert identity string?)))

    (("-r" "--rpc-bind") endpoint "RPC endpoint to bind to."
                         (rpc-endpoint (assert endpoint string?)))

    (("-p" "--pub-bind") endpoint "Publisher endpoint to bind to"
                         (pub-endpoint (assert endpoint string?)))

    #:args (device-host)
    (begin (assert device-host string?))))


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


;;
;; This proxy object hides the complexity of the web interface and
;; provides just two, simple methods; `get-status` and `set-channel!`.
;;
;; Our two lights use following channels:
;;
;;  *  left: 1-18 (group 0)
;;  *  right: 513-530 (group 8)
;;
(define cc : (Instance CueCore%)
  (new cuecore% (host (device-host))))


(: channels->status (-> (Listof Natural) JSExpr))
(define (channels->status channels)
  (match channels
    ((list* pan _ tilt _ _ power color _ _ _ _ _ _ focus _ shutter volume _)
     (hashjs 'level (hashjs 'pan pan
                            'tilt tilt
                            'focus focus
                            'volume volume)
             'lamp (match power
                     (130 "on")
                     (230 "off")
                     (else "unknown"))
             'color (match color
                      (0 "white")
                      (11 "dark-green")
                      (23 "red")
                      (34 "light-azure")
                      (46 "magenta")
                      (58 "uv-filter")
                      (70 "yellow")
                      (81 "green")
                      (93 "pink")
                      (105 "blue")
                      (117 "deep-red")
                      (250 "random")
                      (else "unknown"))
             'shutter (match shutter
                        (0 "closed")
                        (32 "open")
                        (else "unknown"))))))


(: get-status (-> JSExpr))
(define (get-status)
  (hashjs 'left (channels->status (send cc get-status (cast 0 Group)))
          'right (channels->status (send cc get-status (cast 8 Group)))))

(: push-status (-> Void))
(define (push-status)
  (socket-send-json pusher (hashjs 'full (get-status))))


(: set-channel! (-> Light Natural Natural Void))
(define (set-channel! light channel value)
  (define absolute-channel
    (match light
      ("left" (+ 0 channel))
      ("right" (+ 512 channel))))

  (send cc set-channel! absolute-channel value))


;; Periodically inquire about status.
(: ticker-main (-> Nothing))
(define (ticker-main)
  (define timer
    (wrap-evt
      (recurring-alarm-evt 5000)
      (λ (now)
        (push-status))))

  (loop (sync timer)))


;; Listen to client's requests and relay them to the driver.
(: router-main (-> Nothing))
(define (router-main)
  (loop
    (define-values (sender request)
      (socket-receive-json-from router))

    (log-client-info "[~s] ~s" sender request)

    (match request
      ((hash-lookup ('request "status"))
       (void))

      ((hash-lookup ('request "set-lamp!")
                    ('light (? light? light))
                    ('lamp (? lamp? lamp)))
       (set-channel! light 6 (match lamp
                               ("on" 130)
                               ("off" 230))))

      ((hash-lookup ('request "set-shutter!")
                    ('light (? light? light))
                    ('shutter (? shutter? shutter)))
       (set-channel! light 16 (match shutter
                                ("open" 32)
                                ("closed" 0))))

      ((hash-lookup ('request "set-color!")
                    ('light (? light? light))
                    ('color (? color? color)))
       (set-channel! light 7 (match color
                               ("white" 0)
                               ("dark-green" 11)
                               ("red" 23)
                               ("light-azure" 34)
                               ("magenta" 46)
                               ("uv-filter" 58)
                               ("yellow" 70)
                               ("green" 81)
                               ("pink" 93)
                               ("blue" 105)
                               ("deep-red" 117)
                               ("random" 250))))

      ((hash-lookup ('request "set-pan!")
                    ('light (? light? light))
                    ('pan (? exact-nonnegative-integer? pan)))
       (set-channel! light 1 pan))

      ((hash-lookup ('request "set-tilt!")
                    ('light (? light? light))
                    ('tilt (? exact-nonnegative-integer? tilt)))
       (set-channel! light 3 tilt))

      ((hash-lookup ('request "set-focus!")
                    ('light (? light? light))
                    ('focus (? exact-nonnegative-integer? focus)))
       (set-channel! light 14 focus))

      ((hash-lookup ('request "set-volume!")
                    ('light (? light? light))
                    ('volume (? exact-nonnegative-integer? volume)))
       (set-channel! light 17 volume))

      (else
       (log-client-error "[~s] invalid request: ~s" sender request)))

    ;; Notify clients right away.
    (push-status)))


;; Wait until something dies.
(void (sync (thread router-main)
            (thread ticker-main)))


; vim:set ts=2 sw=2 et:
