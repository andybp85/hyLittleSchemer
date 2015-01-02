; hyLittleSchemer, The Little Schemer in Hy
; Andrew Stanish, 2015

;--------------------------------------------------------
; Before We Begin...
;--------------------------------------------------------

; elthe
; else for cond. the pythonic else works with if statements, 
; but not with cond. the hy documentation says to just use 
; true, but I defined this because it looks nicer.

(def elthe true)

;--------------------------------------------------------
; 1. Toys
;--------------------------------------------------------

; atom?
; returns true if x is an atom
(defn atom? [x]
  (not (coll? x)))


; null?
; returns true if x is an empty list
(defn null? [x]
  (empty? x))


; eq?
; Schemer wants this to return true if both arguments are 
; non-numeric atoms, but the footnotes seem to say that 
; this should suffice
(defn eq? [x y]
  (= x y))

;--------------------------------------------------------
; 2. Do It, Do It Again, and Again, and Again ...
;--------------------------------------------------------

; lat?
; returns true if l is a list of atoms, or if l is an atom
;
; => (lat? '(bacon (and eggs)))
; False

(def lat? 
  (lambda [l]
    (cond
      [(null? l) true]
      [(atom? (car l)) (lat? (cdr l))]
      [elthe false])))


; member?
;returns true if atom a is a member of list l
;
; => (member? 'tea '(coffee tea and milk))
; True

(def member?
  (lambda [a lat]
    (cond
      [(null? lat) false]
      [elthe (or (eq? (car lat) a)
        (member? a (cdr lat)))])))

;--------------------------------------------------------
; 3. Cons the Magnificent 
;--------------------------------------------------------

; rember
; takes an atom and a lat as its arguments, and makes a new 
; lat with the first occurrence of the atom in the old lat removed
;
;=> (rember 'and '(bacon lettuce and tomato))
;('bacon' 'lettuce' 'tomato')

; long versiom
;(def rember
;  (lambda [a lat]
;    (cond
;      [(null? lat) '()]
;      [elthe (cond
;              [(eq? (car lat) a) (cdr lat)]
;              [elthe (cons (car lat)
;                      (rember a
;                        (cdr lat)))])])))
; short version 
(def rember 
  (lambda [a lat]
    (cond
      [(null? lat) '()]
      [(eq? (car lat) a) (cdr lat)]
      [elthe (cons (car lat) 
               (rember a (cdr lat)))])))


; firsts
; takes one argument, a list, which is either a null list or contains 
; only non-empty lists, and builds another list composed of the 
; first S-expression of each internal list
;
; => (firsts '((a b) (c d) (e f)))
; ('a' 'c' 'e')

(def firsts 
  (lambda [l]
    (cond
      [(null? l) '()]
      [elthe (cons (car (car l)) 
               (firsts (cdr l)))])))


; insertR
; akes three arguments: the atoms new and old, and a lat.  function 
; insertR builds a lat with new inserted to the right of the first 
; occurrence of old
;
; => (insertR 'topping 'fudge '(ice cream with fudge for dessert))
; ('ice' 'cream' 'with' 'fudge' 'topping' 'for' 'dessert')

(def insertR
  (lambda [new old lat]
    (cond 
      [(null? lat) '()]
      [elthe 
        (cond 
          [(eq? (car lat) old) 
           (cons old 
             (cons new (cdr lat)))]
          [elthe (cons (car lat) 
                   (insertR new old 
                     (cdr lat)))])])))


; insertL
; insertL inserts the atom new to the left of the first occurrence 
; of the atom old in lat
;
; => (insertL 'topping 'fudge '(ice cream with fudge for dessert))
; ('ice' 'cream' 'with' 'topping' 'fudge' 'for' 'dessert')

(def insertL
  (lambda [new old lat]
    (cond 
      [(null? lat) '()]
      [elthe 
        (cond 
          [(eq? (car lat) old) 
             (cons new lat)]
          [elthe (cons (car lat) 
                   (insertL new old 
                     (cdr lat)))])])))


; subst
; replaces the first occurrence of old in the lat with new
;
; => (subst 'topping 'fudge '(ice cream with fudge for dessert))
; ('ice' 'cream' 'with' 'topping' 'for' 'dessert')

(def subst
  (lambda [new old lat]
    (cond 
      [(null? lat) '()]
      [elthe 
        (cond 
          [(eq? (car lat) old) 
            (cons new (cdr lat))]
          [elthe (cons (car lat) 
                   (subst new old 
                     (cdr lat)))])])))


; subst2
; replaces either the first occurrence of 01 or the first 
; occurrence of 02 by new
;
; => (subst2 'vanilla 'chocolate 'banana '(banana ice cream with chocolate topping))
; ('vanilla' 'ice' 'cream' 'with' 'chocolate' 'topping')

(def subst2
  (lambda [new o1 o2 lat]
    (cond
      [(null? lat) '()]
      [(or (eq? o1 (car lat)) 
           (eq? o2 (car lat)))
         (cons new (cdr lat))]
      [elthe (cons (car lat)
               (subst2 new o1 o2 
                 (cdr lat)))])))


; multirember
; gives as its final value the lat with all occurrences of a removed
;
; => (multirember 'cup '(coffee cup tea cup and hick cup))
; ('coffee' 'tea' 'and' 'hick')

(def multirember
  (lambda [a lat]
    (cond
      [(null? lat) '()]
      [elthe
        (cond
          [(eq? a (car lat))
             (multirember a (cdr lat))]
          [elthe (cons (car lat)
                   (multirember a (cdr lat)))])])))


; multiinsertR 
; like insertR but after every instance in list
;
; => (multiinsertR 'cup 'tea '(coffee tea and tea))
; ('coffee' 'tea' 'cup' 'and' 'tea' 'cup')

(def multiinsertR
  (lambda [new old lat]
    (cond 
      [(null? lat) '()]
      [elthe
        (cond 
          [(eq? old (car lat))
             (cons old (cons new 
                        (multiinsertR new old 
                          (cdr lat))))]
          [elthe (cons (car lat)
                   (multiinsertR new old 
                     (cdr lat)))])])))


; multiinsertL
; like insertL but every instance
;
; => (multiinsertL 'fried 'fish '(chips and fish or fish and fried))
; ('chips' 'and' 'fried' 'fish' 'or' 'fried' 'fish' 'and' 'fried')

(def multiinsertL
  (lambda [new old lat]
    (cond 
      [(null? lat) '()]
      [elthe 
        (cond 
          [(eq? old (car lat)) 
             (cons new (cons old 
                         (multiinsertL new old 
                           (cdr lat))))]
          [elthe (cons (car lat)
                   (multiinsertL new old
                     (cdr lat)))])])))


; multisubst
; like subst but every instance
;
; => (multisubst 'fried 'fish '(chips and fish or fish and fried))
; ('chips' 'and' 'fried' 'or' 'fried' 'and' 'fried')

(def multisubst
  (lambda [new old lat]
    (cond 
      [(null? lat) '()]
      [elthe 
        (cond 
          [(eq? old (car lat)) 
             (cons new
               (multisubst new old 
                 (cdr lat)))]
          [elthe (cons (car lat)
                   (multisubst new old
                     (cdr lat)))])])))


;--------------------------------------------------------
; 4.  Numbers Game
;--------------------------------------------------------




