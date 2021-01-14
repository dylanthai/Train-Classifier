;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname guess) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "animals.rkt")
;;
;; *****************************
;; Dylan Thai (20898721)
;; CS 135 Fall 2020
;; Assignment 07, Problem 2
;; *****************************
;;


;; An Example is a (cons Sym (listof Sym))
;; Requires: each attribute in the rest is unique

;; A Histogram is a (listof (list Sym Nat))
;; Requires: A symbol can appear in only one pair.

;; An Augmented Histogram (AH) is a (listof (list Sym Nat Nat))
;; Requires: A symbol can appear in only one triple.

;; An Entropy Association List (EAL) is a (listof (list Sym Num))
;; Requires: A symbol can appear in only one pair.

;; A Decision Tree (DT) is one of:
;; * Bool
;; * (list Sym DT DT)

;; Constants:
(define seen
  (list
   (list 'squirrel 'small 'angry)
   (list 'goose 'large 'swims 'flies 'angry)
   (list 'goose 'large 'swims 'flies 'angry)
   (list 'crow 'medium 'flies 'angry)))

(define test-example
  (list
   (list 'squirrel 'a 'b 'c 'd)
   (list 'goose 'b 'c)
   (list 'goose 'a 'b 'd 'e)
   (list 'crow 'f 'a 'b 'c)))



;;
;; Part a
;;

;; (collect-attributes examples) Produces a list of attributes
;;    contained in the examples with no duplicates
;; Examples:
(check-expect (collect-attributes empty) empty)

(check-expect (collect-attributes seen)
              (list 'small 'angry 'large 'swims 'flies 'medium))



;; collect-attributes: (listof Example) -> (listof Sym)
(define (collect-attributes examples)
  (local
    [;; (add-new-attributes lst-of-attributes) 
     ;; add-new-attributes: (listof Example) -> (listof Sym)
     (define (add-new-attributes examples lst-of-attributes)
       (cond [(empty? examples) lst-of-attributes]
             [else (add-new-attributes (rest examples)
                                       (append lst-of-attributes
                                               (new-attributes (rest (first examples))
                                                               lst-of-attributes)))]))

     ;; (new-attributes animal-attributes lst-of-attributes) add new attributes of a single animal
     ;; new-attributes: (listof Sym) -> (listof Sym)
     (define (new-attributes animal-attributes lst-of-attributes)
       (cond [(empty? animal-attributes) empty]
             [(member? (first animal-attributes) lst-of-attributes)
              (new-attributes (rest animal-attributes) lst-of-attributes)]
             [else (cons (first animal-attributes)
                         (new-attributes (rest animal-attributes)
                                         lst-of-attributes))]))]

    (cond [(empty? examples) empty]
          [else (add-new-attributes examples (list))])))

;; Tests:
(check-expect (collect-attributes test-example)
              (list 'a 'b 'c 'd 'e 'f))



;;
;; Part b
;; 

;; (split-examples examples symbol) Produces a list of two list of 
;;    examples one with examples containing symbol and the other 
;;    with examples not containing symbol
;; Examples:
(check-expect (split-examples seen 'goose)
              (list
               (list
                (list 'goose 'large 'swims 'flies 'angry)
                (list 'goose 'large 'swims 'flies 'angry))
               (list
                (list 'crow 'medium 'flies 'angry)
                (list 'squirrel 'small 'angry))))
(check-expect (split-examples seen 'small)
              (list
               (list
                (list 'squirrel 'small 'angry))
               (list
                (list 'crow 'medium 'flies 'angry)
                (list 'goose 'large 'swims 'flies 'angry)
                (list 'goose 'large 'swims 'flies 'angry))))

;; split-examples: (listof Example) Sym -> (listof (listof Example) (listof Example))
(define (split-examples examples symbol)
  (local
    [;; (do-split examples two-lists) 
     ;; do-split: (listof Example) (list (listof Examples) (listof Examples)) -> (list (listof Examples) (listof Examples))
     (define (do-split examples two-lists)
       (cond [(empty? examples) two-lists]
             [(member? symbol (first examples))
              (do-split (rest examples)
                        (list (cons (first examples) (first two-lists))
                              (second two-lists)))]
             [else (do-split (rest examples)
                             (list (first two-lists)
                                   (cons (first examples) (second two-lists))))]))]
    (cond [(empty? examples) (list empty empty)]
          [else (do-split examples (list (list)
                                         (list)))])))

;; Tests:
(check-expect (split-examples empty 'small) (list empty empty))
(check-expect (split-examples seen 'typo)
              (list (list)
                    (list (list 'crow 'medium 'flies 'angry)
                          (list 'goose 'large 'swims 'flies 'angry)
                          (list 'goose 'large 'swims 'flies 'angry)
                          (list 'squirrel 'small 'angry))))

;;
;; Part c
;;

;; (histogram examples) produces a list of attribute/count pairs
;; Examples:
(check-expect (histogram seen)
              (list
               (list 'small 1) (list 'angry 4) (list 'large 2)
               (list 'swims 2) (list 'flies 3) (list 'medium 1)))

;; histogram: (listof Example) -> Histogram
(define (histogram examples)
  (local
    [;; (create-histogram examples lst-of-attributes) creates the
     ;;     histogram for the function
     ;; create-histogram: (listof Example) (listof Sym) -> (listof (list Sym Num))
     (define (create-histogram examples lst-of-attributes histogram)
       (cond [(empty? lst-of-attributes) histogram]
             [else (create-histogram examples
                                     (rest lst-of-attributes)
                                     (append histogram (list (list (first lst-of-attributes)
                                                                   (count-appearances examples (first lst-of-attributes))))))]))

     ;; (count-appearances examples attribute) counts the number of
     ;;     times attribute appears in examples
     ;; count-appearances: (listof Example) Sym -> Nat
     (define (count-appearances examples attribute)
       (length (first (split-examples examples attribute))))]
    
    (cond [(empty? examples) empty]
          [else (create-histogram examples (collect-attributes examples) (list))])))

;; Tests:
(check-expect (histogram empty) empty)

;;
;; Part d
;;

;; (augment-histogram histogram atributes total) Augments the histogram
;; Examples:
(check-expect
 (augment-histogram
  (list (list 'a 100) (list 'c 50))
  (list 'a 'b 'c)
  200)
 (list (list 'a 100 100) (list 'b 0 200) (list 'c 50 150)))

(check-expect
 (augment-histogram empty (list 'x 'y) 10)
 (list (list 'x 0 10) (list 'y 0 10)))

;; augment-histogram: Histogram (listof Sym) Nat -> AH
(define (augment-histogram histogram attributes total)
  (local
    [;; (make-augmented-histogram histogram attributes augmented-histogram) makes the augmented histogram
     ;; make-augmented-histogram: Histogram (listof Sym) AH -> AH
     (define (make-augmented-histogram histogram attributes existing-attributes augmented-histogram)
       (cond [(empty? attributes) augmented-histogram]
             [(member? (first attributes) existing-attributes)
              (make-augmented-histogram histogram (rest attributes) existing-attributes
                                        (append augmented-histogram
                                                (list (append (retrieve-sym-nat histogram (first attributes))
                                                              (list (- total (second (retrieve-sym-nat histogram (first attributes)))))))))]
             [else (make-augmented-histogram histogram (rest attributes) existing-attributes
                                             (append augmented-histogram (list (list (first attributes) 0 total))))]))

     ;; (collect-attributes-from-histogram histogram)
     ;; collect-attributes-from-histogram: Histogram -> (listof Sym)
     (define (collect-attributes-from-histogram histogram)
       (cond [(empty? histogram) empty]
             [else (cons (first (first histogram))
                         (collect-attributes-from-histogram (rest histogram)))]))

     ;; (retrieve-sym-nat histogram sym) Produces the sym-nat pair with sym
     (define (retrieve-sym-nat histogram sym)
       (cond [(symbol=? (first (first histogram)) sym) (first histogram)]
             [else (retrieve-sym-nat (rest histogram) sym)]))]
    
    (cond [(empty? attributes) empty]

          [else (make-augmented-histogram histogram
                                          attributes
                                          (collect-attributes-from-histogram histogram)
                                          (list))])))

;; Tests:




;;
;; Part e
;;

;; (define entropy postive-count negative-counts) produces the entropy
;;    from elements from an augmented histogram
;; Examples:
(check-within (entropy (list 'large 126 59) (list 'large 146 669))
              0.5663948489858 0.001)
(check-within (entropy (list 'small 17 168) (list 'small 454 361))
              0.5825593868115 0.001)
(check-within (entropy (list 'a 0 100) (list 'b 100 0))
              0.0 0.001)

;; entropy: positive counts negative-counts

(define (entropy postive-counts negative-counts)
  (local
    [;; (p n m) calculates the probability from a pair of
     ;;    counts n and m
     ;; Requires: (and (n >= 0)  (m >= 0))
     
     (define (p n m)
       (cond [(> (+ n m) 0) (/ n (+ n m))]
             [else 0.5]))

     ;; (e p) calculates e(p)
     (define (e p)
       (cond [(= p 0) 0]
             [else (* -1 p (/ (log p) (log 2)))]))]

    (+ (* (p (+ (second postive-counts) (second negative-counts))
             (+ (third postive-counts) (third negative-counts)))
          
          (+ (e (p (second postive-counts) (second negative-counts)))
             (e (p (second negative-counts) (second postive-counts)))))

       (* (p (+ (third postive-counts) (third negative-counts))
             (+ (second postive-counts) (second negative-counts)))
          
          (+ (e (p (third postive-counts) (third negative-counts)))
             (e (p (third negative-counts) (third postive-counts))))))))


;;
;; Part f
;;

;; (entropy-attributes postive negative) computes the entropy of
;;    each attribute producing a list of attribute/entropy pairs.

;; entropy-attributes: AH AH -> EAL
(define (entropy-attributes positive negative)
  (cond [(empty? positive) empty]
        [else (cons (list (first (first positive))
                          (entropy (first positive) (first negative)))
                    (entropy-attributes (rest positive) (rest negative)))]))

;; Test:
(check-within
 (entropy-attributes
  (list
   (list 'large 126 59) (list 'angry 161 24)
   (list 'small 17 168) (list 'flies 170 15)
   (list 'swims 162 23) (list 'medium 42 143))
  (list
   (list 'large 146 669) (list 'angry 469 346)
   (list 'small 454 361) (list 'flies 615 200)
   (list 'swims 365 450) (list 'medium 215 600)))

 (list
  (list 'large #i0.5663948489858) (list 'angry #i0.6447688190492)
  (list 'small #i0.5825593868115) (list 'flies #i0.6702490498564)
  (list 'swims #i0.6017998773730) (list 'medium #i0.6901071708677))

 0.001)

;;
;; Part g
;;

;; (best-attribute entropies) Produce the attribute with the minimum
;;    entropy
;; Example:
(check-expect (best-attribute
               (list
                (list 'large #i0.5663948489858) (list 'angry #i0.6447688190492)
                (list 'small #i0.5825593868115) (list 'flies #i0.6702490498564)
                (list 'swims #i0.6017998773730) (list 'medium #i0.6901071708677))) 'large)

(check-expect (best-attribute
               (list
                (list 'large #i0.5663948489858) (list 'angry #i0.6447688190492))) 'large)

;; best-attribute: EAL -> Sym
;; Requires entropies to be a non-empty list
(define (best-attribute entropies)
  (cond [(empty? (rest entropies)) (first (first entropies))]
        [(> (second (first entropies)) (second (second entropies)))
         (best-attribute (rest entropies))]
        [else (best-attribute (cons (first entropies)
                                    (rest (rest entropies))))]))
;; Tests:
(check-expect (best-attribute
               (list
                (list 'a 5) (list 'b 4) (list 'c 6) (list 'd 8)
                (list 'e 7) (list 'f 1) (list 'g 9))) 'f)
(check-expect (best-attribute
               (list
                (list 'a 5) (list 'b 4) (list 'c 6) (list 'd 8)
                (list 'e 7) (list 'f 9) (list 'g 1))) 'g)

;;
;; Part h
;;

;; (build-dt examples label) produces a decision tree

;; build-dt: (listof Example) Sym -> DT
(define (build-dt examples label)
  (local [
          ;; (postive-examples examples label) produces a list of
          ;;    positive examples
          ;; (listof Examples) Sym -> (listof Examples)
          (define (positive-examples examples label)
            (first (split-examples examples label)))

          ;; (negative-examples examples label) produces a list of
          ;;    negative examples
          ;; (listof Examples) Sym -> (listof Examples)
          (define (negative-examples examples label)
            (second (split-examples examples label)))
          
          ;; (examples-with-root-attribute examples root-attribute) produces a list of
          ;;    positive examples
          ;; (listof Examples) Sym -> (listof Examples)
          (define (examples-with-root-attribute examples root-attribute)
            (first (split-examples examples root-attribute)))

          ;; (examples-without-root-attribute examples root-attribute) produces a list of
          ;;    negative examples
          ;; (listof Examples) Sym -> (listof Examples)
          (define (examples-without-root-attribute examples root-attribute)
            (second (split-examples examples root-attribute)))
          
          ;; (find-root-attribute examples label) produces the root
          ;;    attribute
          ;; (listof Examples) Sym -> Sym
          (define (find-root-attribute examples label)
            (best-attribute (augment-histogram (histogram examples) (collect-attributes examples) (length examples))))

          ;; (remove-root-attribute examples label) produces the root
          ;;    attribute
          ;; (listof Examples) Sym -> (listof Examples)
          (define (remove-root-attribute examples root-attribute)
            (remove-root-attribute-helper examples root-attribute empty))

                                         
          (define (remove-root-attribute-helper examples root-attribute lst-without-root)
            (cond [(empty? examples) lst-without-root]
                  [(member? root-attribute (first examples))
                   (remove-root-attribute-helper (rest examples) root-attribute (append lst-without-root (list (remove-root (first examples) root-attribute))))]
                  [else (remove-root-attribute-helper (rest examples) root-attribute (append lst-without-root (list (first examples))))]))

          ;; (remove-root examples label removes the root from a
          ;;    list of Examples
          ;; (listof Examples) Sym -> (listof Examples)
          (define (remove-root lst root)
            (cond [(symbol=? root (first lst)) (rest lst)]
                  [else (cons (first lst) (remove-root (rest lst) root))]))]

    (cond [(empty? (positive-examples examples label)) false]
          [(empty? (negative-examples examples label)) true]

          [(empty? (collect-attributes examples))
           (cond [(> (length (positive-examples examples label))
                     (length (negative-examples examples label))) true]
                 [else false])]
          [else (local
                  [ (define root-attribute (find-root-attribute examples label))
                    
                    (define left-list (remove-root-attribute (examples-with-root-attribute examples root-attribute) root-attribute))

                    (define right-list (examples-without-root-attribute examples root-attribute))

                    (define left-tree (build-dt left-list label))

                    (define right-tree (build-dt right-list label))]

                  (cond [(equal? left-tree right-tree) left-tree]
                        [else (list root-attribute left-tree right-tree)]))])
    ))

;;
;; Part i
;;

;; (train-classifier examples label) produces a predicate which
;;    produces true if the list of attributes given are the attributes
;;    of the given label/animal and false otherwise

;; train-classifier: (listof Example) Sym -> ((listof Sym) -> Bool)
(define (train-classifier examples label)
  (local
    [(define decision-tree (build-dt examples label))

     ;; (predicate lst-of-attributes) produces a predicate from a
     ;;    list of attributes and a decision-tree
     (define (predicate lst-of-attributes)
       (create-predicate lst-of-attributes decision-tree))

     (define (create-predicate lst-of-attributes decision-tree)
       (cond [(boolean?  decision-tree) decision-tree]
             [(member? (first decision-tree) lst-of-attributes)
              (create-predicate lst-of-attributes (second decision-tree))]
             [else (create-predicate lst-of-attributes (third decision-tree))]))]
    
    predicate))

;; Tests:
(define goose? (train-classifier (random-animals 1000) 'goose))
(check-expect (goose? (list 'large 'angry 'flies 'swims)) true)
(check-expect (goose? (list 'small 'angry)) false)
(define squirrel? (train-classifier (random-animals 1000) 'squirrel))
(check-expect (squirrel? (list 'large 'angry 'flies 'swims)) false)
(check-expect (squirrel? (list 'small 'angry)) true)
(define crow? (train-classifier (random-animals 1000) 'crow))
(check-expect (crow? (list 'angry 'flies 'medium)) true)