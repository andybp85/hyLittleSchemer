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

; add1 / sub1
; these just rename the native inc and dec

(defn add1 [x]
  (inc x))

(defn sub1 [x]
  (dec x))


; o+
; adding just using add1 and sub1 (see o-)
;
; => (o+ 10 5)
; 15

(def o+
  (lambda [n m]
    (cond
      [(zero? m) n]
      [elthe (add1 (o+ n (sub1 m)))])))


; o-
; takes two numbers as arguments, and reduces the second until
; it hits zero. It subtracts one from the result as many times
; as it did to cause the second one to reach zero.
;
; => (o- 10 5)
; 5

(def o-
  (lambda [n m]
    (cond
      [(zero? m) n]
      [elthe (sub1 (o- n (sub1 m)))])))


; addtup
; builds a number by totaling all the numbers in a tup
;
; => (addtup '(5 6 7))
; 18

(def addtup
  (lambda [tup]
    (cond
      [(null? tup) 0]
      [elthe (o+ (car tup)
               (addtup (cdr tup)))])))


; x
; multiplication - adds n up m times
;
; => (x 3 5)
; 15

(def x
  (lambda [n m]
    (cond
      [(zero? m) 0]
      [elthe (o+ n (x n (sub1 m)))])))


; tup+
; adds the first number of tup1 to the first number of
; tup2, then it adds the second number of tup1 to the
; second number of tup2, and so on, building a tup of
; the answers, for tups of the same length
;
; => (tup+ '(3 6 9 11 4) '(8 5 2 0 7))
; (11 11 11 11 11)
;
; breaks if tups are different lengths
; (def tup+
;  (lambda [tup1 tup2]
;    (cond
;      [(and (null? tup1) (null? tup2))
;       '()]
;      [elthe
;        (cons (o+ (car tup1) (car tup2))
;               (tup+
;                 (cdr tup1) (cdr tup2)))])))
;
; => (tup+ '(3 7 8 1) '(4 6))
; (7 13 8 1)

(def tup+
  (lambda [tup1 tup2]
    (cond
      [(null? tup1) tup2]
      [(null? tup2) tup1]
      [elthe
        (cons (o+ (car tup1) (car tup2))
               (tup+
                 (cdr tup1) (cdr tup2)))])))


; o>
; greater than

(def o>
  (lambda [n m]
    (cond
      [(zero? n) false]
      [(zero? m) true]
      [elthe (o> (sub1 n) (sub1 m))])))


; o<
; less than

(def o<
  (lambda [n m]
    (cond
      [(zero? m) false]
      [(zero? n) true]
      [elthe (o< (sub1 n) (sub1 m))])))


; o=
; equals

(def o=
  (lambda [n m]
    (cond
      [(< n m) false]
      [(> n m) false]
      [elthe true])))


; ↑
; raise first argment to power of second argument
;
; => (↑ 2 9)
; 512

(def ↑
  (lambda [n m]
    (cond
      [(zero? m) 1]
      [elthe (x n (↑ n (sub1 m)))])))


; ÷
; division with whole numbers
;
; => (÷ 15 4)
; 3

(def ÷
  (lambda [n m]
    (cond
      [(o< n m) 0]
      [elthe (add1 (÷ (- n m) m))])))


; length
; count atoms in a lat
;
; => (length '(hotdogs with mustard sauerkraut and pickles))
; 6

(def length
  (lambda [lat]
     (cond
       [(null? lat) 0]
       [elthe (add1 (length (cdr lat)))])))


; pick
; return the nth element from a lat
;
; => (pick 4 '(lasagna spaghetti ravioli macaroni meatball))
; 'macaroni'

(def pick
  (lambda [n lat]
    (cond
      [(zero? (sub1 n)) (car lat)]
      [elthe (pick (sub1 n) (cdr lat))])))


; rempick
; remove the nth element from a list
;
; => (rempick 3 '(hotdogs with hot mustard))
; ('hotdogs' 'with' 'mustard')

(def rempick
  (lambda [n lat]
    (cond
      [(zero? (sub1 n)) (cdr lat)]
      [elthe (cons (car lat) 
               (rempick (sub1 n) (cdr lat)))])))


; no-nums
; gives as a final value a lat obtained by removing all the 
; numbers from the lat
;
; => (no-nums '(5 pears 6 prunes 9 dates))
; ('pears' 'prunes' 'dates')

(def no-nums
  (lambda [lat]
    (cond
      [(null? lat) '()]
      [elthe 
        (cond 
          [(numeric? (car lat)) 
             (no-nums (cdr lat))]
          [elthe (cons (car lat) 
                   (no-nums 
                     (cdr lat)))])])))


; all-nums
; xtracts a tup from a lat using all the numbers in the lat
;
; => (all-nums '(5 pears 6 prunes 9 dates))
; (5 6 9)')

(def all-nums
  (lambda [lat]
    (cond
      [(null? lat) '()]
      [elthe 
        (cond 
          [(numeric? (car lat)) 
           (cons (car lat) 
             (all-nums (cdr lat)))]
          [elthe (all-nums (cdr lat))])])))









