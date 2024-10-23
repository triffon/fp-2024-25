;дефиниция на foldr
(define (foldr op nv lst)
  (if (null? lst)
      nv
      (op (car lst) (foldr op nv (cdr lst)))))

;конструктивна дефиниция на member без foldr
(define (member* =? x lst)
  (cond ((null? lst) #f)
        ((=? x (car lst)) lst)
        (else (member* =? x (cdr lst)))))


;конструктивна дефиниция на member, използвайки foldr
; идея >>

;bmember* връща #t, ако x е елемент на lst,
;и #f в противен случай.
;първо проверяваме дали изобщо елементът x е в lst:
;- ако не е, директно връщаме #f
;- ако е, започваме да изпълняваме foldr:
;ламбда функцията има за цел при всяко изпълнение на op
;(от foldr op nv lst) да проверява дали x е елемент на
;останалата част от списъка ys:
;- ако x е в ys, продължаваме напред
;- ако x вече не е в ys, то значи вече сме преминали
;през него и трябва да го добавим към резултата.
;по същия начин добавяме и останалите елементи
;след първото срещане на x
(define (fmember* =? x lst)
  (define (bmember* lst)
    (cond ((null? lst) #f)
          ((=? x (car lst)) #t)
          (else (bmember* (cdr lst)))))
  
  (and (bmember* lst)
       (foldr (lambda (y ys)
                (if (bmember* ys)
                    ys
                    (cons y ys)))
              '()
              lst)))

;----------

;алтернативен подход, използвайки индекса на
;елемента x в списъка (нека е n)
;след което "премахваме" първите n елемента
; >>

;ако елементът x не е в lst,
;връща дължината на lst,
;което е полезна дефиниция за по-долната имплементация
(define (index =? x lst)
  (cond ((null? lst) 1) ;
        ((=? x (car lst)) 0)
        (else (+ 1 (index =? x (cdr lst))))))

(define (drop n lst)
  (if (or (null? lst) (<= n 0))
      lst
      (drop (- n 1) (cdr lst))))

(define (drop-member* =? x lst)
  (define nl (drop (index =? x lst) lst))
  (if (null? nl)
      #f
      nl))