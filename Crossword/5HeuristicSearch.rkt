 #lang racket


;-------------------Inputs--------------------

(define input(list "aaaaa" "bbbbb" "ccccc" "ddddd" "eeeee"))


(define vowels (list "a" "e" "i" "o" "u"))



;====================Main Functions========================

(define (PuzzleSolver StartNode FirstWords SecondWords ThirdWords FourthWords FifthWords Stack)
  (if (HasChildren? StartNode)
      (cond
        [(equal? (length StartNode) 0)(print "X")(PuzzleSolver
                                       (car (MakeChildren StartNode FirstWords Stack))
                                       FirstWords
                                       SecondWords
                                       ThirdWords
                                       FourthWords
                                       FifthWords
                                       (cdr (MakeChildren StartNode FirstWords Stack))
                                       ) ]
        [(equal? (length StartNode) 1)(PuzzleSolver
                                       (car (heuristic (MakeChildren StartNode SecondWords Stack) (list) (list) (list) (list)))
                                       FirstWords
                                       SecondWords
                                       ThirdWords
                                       FourthWords
                                       FifthWords
                                       (pop (heuristic (MakeChildren StartNode SecondWords Stack) (list) (list) (list) (list)))
                                       ) ]
        [(equal? (length StartNode) 2)(PuzzleSolver
                                       (top (MakeChildren StartNode ThirdWords Stack))
                                       FirstWords
                                       SecondWords
                                       ThirdWords
                                       FourthWords
                                       FifthWords
                                       (pop (MakeChildren StartNode ThirdWords Stack))
                                       ) ]
        [(equal? (length StartNode) 3)(PuzzleSolver
                                       (top (MakeChildren StartNode ThirdWords Stack))
                                       FirstWords
                                       SecondWords
                                       ThirdWords
                                       FourthWords
                                       FifthWords
                                       (pop (MakeChildren StartNode ThirdWords Stack))
                                       ) ]
        [(equal? (length StartNode) 4)(PuzzleSolver
                                       (top (MakeChildren StartNode ThirdWords Stack))
                                       FirstWords
                                       SecondWords
                                       ThirdWords
                                       FourthWords
                                       FifthWords
                                       (pop (MakeChildren StartNode ThirdWords Stack))
                                       ) ]

        
        )
      (cond
        [(IsGoal? input StartNode) StartNode]
        [else
         (if (empty? Stack)
             (print "no solution")
             (PuzzleSolver(top Stack) FirstWords SecondWords ThirdWords FourthWords FifthWords (pop Stack)))
         ]
        )
      )
  )

(define (MakeChildren ParentNode Words Stack)
  (cond
    [(equal? (length Words) 1)(push (flatten(list ParentNode (car Words))) Stack)]
    [else(MakeChildren ParentNode (cdr Words)(push (flatten(list ParentNode (car Words))) Stack))]
    )
  
  )


(define (Main)
  (define FirstWords (wordFilter input (remove-duplicates (1stLetters input))))
  (define SecondWords (wordFilter input (remove-duplicates (2ndLetters input))))
  (define ThirdWords (wordFilter input (remove-duplicates (3rdLetters input))))
  (define FourthWords (wordFilter input (remove-duplicates (4thLetters input))))
  (define FifthWords (wordFilter input (remove-duplicates (5thLetters input))))
  (PuzzleSolver '() FirstWords SecondWords ThirdWords FourthWords FifthWords '())
  )


;=====================Supporting Functions=========================

;----------------------Heuristic Functions-------------------------

(define (countVowel words)

  (+ ( + (if (member (string (string-ref (car words) 0)) vowels) 1 0)
         (+ (if (member (string (string-ref (car words) 1)) vowels) 1 0)
            (if (member (string (string-ref (car words) 2)) vowels) 1 0)))
     ( + (if (member (string (string-ref (cadr words) 0)) vowels) 1 0)
         (+ (if (member (string (string-ref (cadr words) 1)) vowels) 1 0)
            (if (member (string (string-ref (cadr words) 2)) vowels) 1 0))))
  )  

(define (heuristic stack list1 list2 list3 list4)
  (if (empty? stack)
      (print "no solution")
      (cond
        [(equal? (length(car stack)) 1)
         (append list4 (append list3 (append list2 (append list1 stack))))]
        [(= 4 (countVowel (car stack)))
         (heuristic (cdr stack) list1 list2 list3 (push (car stack) list4 ))]
        [(= 3 (countVowel (car stack)))
         (heuristic (cdr stack) list1 list2 (push (car stack) list3) list4 )]
        [(= 2 (countVowel (car stack)))
         (heuristic (cdr stack) list1 (push (car stack) list2 ) list3 list4)]
        [(= 1 (countVowel (car stack)))
         (heuristic (cdr stack) (push (car stack) list1 ) list2 list3 list4)]
        )
      )
  ) 

;-----------------------Pre-Search Filters-------------------------


;Creates of list of the starting letters of every word in the given list.
;May include duplicates
(define (1stLetters words)
  (if (empty? words) '()
      (append (1stLetters (cdr words)) (list(string-ref (car words) 0)))
      )
  )

(define (2ndLetters words)
  (if (empty? words) '()
      (append (2ndLetters (cdr words)) (list(string-ref (car words) 1)))
      )
  )

(define (3rdLetters words)
  (if (empty? words) '()
      (append (3rdLetters (cdr words)) (list(string-ref (car words) 2)))
      )
  )

(define (4thLetters words)
  (if (empty? words) '()
      (append (3rdLetters (cdr words)) (list(string-ref (car words) 3)))
      )
  )

(define (5thLetters words)
  (if (empty? words) '()
      (append (3rdLetters (cdr words)) (list(string-ref (car words) 4)))
      )
  )



;accepts a list of word and a list of letters and removes words that are
;comprized of letters other than those provided in the list and returns that
;list
(define (wordFilter words letters)
  (if (empty? words) '()
      (if (and (member (string-ref (car words) 0) letters) (and (member(string-ref (car words) 1) letters)  (member (string-ref (car words) 2) letters)))
          (append (list(car words)) (wordFilter (cdr words) letters))
          (wordFilter (cdr words) letters)
          )
      )
  )


;accepts a list of letters and makes a list of letters that were duplicated
;in the old list. The new list still may contain duplicates. This should NOT
;be passed to wordfilter as it is to specific.
(define (LetterMatchFilter letters)
  (if (empty? letters) '()
      (if (member (car letters) (cdr letters))
          (append (list (car letters)) (LetterMatchFilter (cdr letters)))
          (LetterMatchFilter (cdr letters))
          )
      )
  )

;---------------------------Checker Functions-------------------------

(define (HasChildren? Node)
  (not(equal?(length Node) 3))
  )

(define (IsGoal? words Node)
  (if (and
       (member(string (string-ref (list-ref Node 0) 0) (string-ref (list-ref Node 1) 0)(string-ref (list-ref Node 2) 0)) words)
       (and
        (member (string (string-ref (list-ref Node 0) 1) (string-ref (list-ref Node 1) 1)(string-ref (list-ref Node 2) 1)) words)
        (member (string (string-ref (list-ref Node 0) 2) (string-ref (list-ref Node 1) 2)(string-ref (list-ref Node 2) 2)) words)
            )
           )
      #t
      #f
      )
)



;-------------------------Stack Tools-----------------------

;Stack tools
(define (empty? stack)
  (null? stack)
  )

(define (push el stack)
  (cons el stack)
  )

(define (pop stack)
  (if (empty? stack)
      stack
      (cdr stack)
      )
  )

(define (top stack)
  (if (empty? stack)
      '()
      (car stack)
      )
  )


;++++++++++++++++++++++Execution+++++++++++++++++++++++


(Main)