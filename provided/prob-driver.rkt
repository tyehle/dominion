#lang racket
(require "driver.rkt"
         "proc.rkt"
         "move.rkt"
         "cards.rkt")
(require (prefix-in srfi: srfi/48))

(define programs (vector->list (current-command-line-arguments)))
(define names (generate-names programs))


(define (results-of players)
  (define hands (for/list ([p players])
                  (append (player-deck p)
                          (player-hand p)
                          (player-discards p))))
  (define scores (for/list ([hand hands])
                   (for/fold ([v 0]) ([c hand])
                     (+ v (points-of c)))))
  (define winner-score (apply max scores))
  (define tie? ((count (λ (score) (= score winner-score)) scores) . > . 1))
  (map (λ (p score) (if (= score winner-score)
                        (if tie?
                            `(,(player-name p) 0 1 0)
                            `(,(player-name p) 1 0 0))
                        `(,(player-name p) 0 0 1)))
       players scores))

(define (transpose xss)
  (apply map list xss))

(define (sum xs)
  (foldl (λ (t x) (+ t x)) 0 xs))

(define (run-games n)
  (define all-results
    (apply append (build-list n (λ (_) (results-of
                                        (drive names
                                               (map start-program programs)
                                               message-to
                                               message-from
                                               (lambda (g t c) (void))
                                               (lambda (m c) (void))
                                               (lambda (d) (void))))))))
  
  (define (results-for-player name)
    (filter (λ (result) (eq? name (first result))) all-results))
  
  (map (λ (name) (let* ([results (results-for-player name)]
                        [together (transpose results)])
                   (list (first (first together))
                         (/ (sum (second together)) n)
                         (/ (sum (third together)) n)
                         (/ (sum (fourth together)) n))))
       names))


;; report-results : list-of-player -> 
(define (report-results players)
  (for ([p players])
    (printf "~a\n\twins:  ~a\n\tties:  ~a\n\tloses: ~a\n" 
            (first p)
            (srfi:format "~1,2F" (second p))
            (srfi:format "~1,2F" (third p))
            (srfi:format "~1,2F" (fourth p)))))

(report-results (run-games 50))

