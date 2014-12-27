; elthe
; else for cond, just looks nicer
(def elthe true)

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

; lat?
; returns true if l is a list of atoms, or if l is an atom
(def lat? 
  (lambda [l]
    (cond
      [(null? l) true]
      [(atom? (car l)) (lat? (cdr l))]
      [elthe false])))

; member?
;returns true if atom a is a member of list l
(def member?
  (lambda [a lat]
    (cond
      [(null? lat) false]
      [elthe (or (eq? (car lat) a)
        (member? a (cdr lat)))])))

; rember
; takes an atom and a lat as its arguments, and makes a new 
; lat with the first occurrence of the atom in the old lat removed
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


