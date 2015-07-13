#lang typed/racket/base
;
; ZeroMQ Utilities
;

(require racket/match
         typed/json)

(require zmq)

(provide socket-receive-json-from
         socket-receive-json
         socket-send-json-to
         socket-send-json)


(: socket-receive-json-from (-> Socket (values Bytes JSExpr)))
(define (socket-receive-json-from socket)
  (match (socket-receive socket)
    ((list sender bstr)
     (values sender (bytes->jsexpr bstr)))))

(: socket-receive-json (-> Socket JSExpr))
(define (socket-receive-json socket)
  (match (socket-receive socket)
    ((list bstr)
     (bytes->jsexpr bstr))))


(: socket-send-json (-> Socket JSExpr Void))
(define (socket-send-json socket jsexpr)
  (let ((payload (jsexpr->bytes jsexpr)))
    (socket-send socket payload)))


(: socket-send-json-to (-> Socket Bytes JSExpr Void))
(define (socket-send-json-to socket bstr jsexpr)
  (let ((payload (jsexpr->bytes jsexpr)))
    (socket-send socket bstr payload)))


; vim:set ts=2 sw=2 et:
