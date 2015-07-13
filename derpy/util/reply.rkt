#lang typed/racket/base
;
; Reply
;
; Allows safe communication of asynchronous operation result between threads.
; Reply is bound to a target thread. If the target dies during a `sync`, an
; exception is raised in the waiting thread.
;

(require typed/racket/async-channel
         racket/function
         racket/match
         typed/json)

(require mordae/syntax)

(provide thread-send-with-reply
         thread-receive-with-reply
         spawn-server-thread
         thread-call)


(define-type Flat
  (Rec flat
    (U Boolean Number Char Null Symbol String Keyword (Pairof flat flat))))

(define-predicate exn-value?
  (U Flat exn))


(define-syntax-rule (spawn-server-thread request body ...)
  (spawn-thread
    (loop
      (thread-receive-with-reply
        (λ (request)
          body ...)))))


(: thread-send-with-reply (-> Thread JSExpr (Evtof JSExpr)))
(define (thread-send-with-reply a-thread v)
  (let ((reply-channel ((inst make-async-channel (-> JSExpr)))))
    ((inst thread-send JSExpr) a-thread (cons reply-channel v))
    (choice-evt (wrap-evt reply-channel
                          (λ ((produce-result : (-> JSExpr)))
                            (produce-result)))
                (wrap-evt a-thread
                          (λ _ (error 'thread-send-with-reply
                                      "target thread terminated"))))))


(: thread-call (-> Thread JSExpr JSExpr))
(define (thread-call thread request)
  (sync (thread-send-with-reply thread request)))


(: thread-receive-with-reply (-> (-> JSExpr JSExpr) Void))
(define (thread-receive-with-reply proc)
  (match (thread-receive)
    ((cons (? async-channel? reply-channel)
           (? jsexpr? value))
     (async-channel-put (cast reply-channel (Async-Channelof (-> JSExpr)))
                        (invoke proc (cast value JSExpr))))

    (else (thread-receive-with-reply proc))))


(: invoke (-> (-> JSExpr JSExpr) JSExpr (-> JSExpr)))
(define (invoke proc v)
  (with-handlers ((exn-value? raise-thunk))
    (let ((result (proc v)))
      (thunk result))))

(: raise-thunk (-> (U Flat exn) (-> Nothing)))
(define (raise-thunk exn)
  (thunk (raise exn)))


; vim:set ts=2 sw=2 et:
