Реализирайте следните функции на scheme (R5RS):

1. `cartesian-product`, която за дадени 2 списъка намира декартовото им
   произведение.

   Пример:
   ```scheme
   (cartesian-product '(1 2 3) '(4 5)) ; ~> '((1 . 4) (1 . 5) (2 . 4) (2 . 5) (3 . 4) (3 . 5))
   ```

2. `excluded`, която за дадено естествено число **n > 1** връща списък от всички
   двойки `(a . b)`, такива че произведението **a.b** е равно на сумата от
   всички останали числа в интервала **[1, n]**

   Пример:
   ```scheme
    (excluded 10) ; ~> '((6 . 7) (7 . 6))
    ; 6 * 7 = 42 = 1 + 2 + 3 + 4 + 5 + 8 + 9 + 10
   ```

* Всяка вярна задача носи по 4т + 1т ако е решена без явна рекурсия.

* Имате право да използвате всички функции от R5RS заедно със следните:
    ```scheme
    (define (foldr op nv lst)
        (if (null? lst)
            nv
            (op (car lst) (foldr op nv (cdr lst)))))

    (define (foldl op nv lst)
        (if (null? lst)
            nv
            (foldl op (op nv (car lst)) (cdr lst))))

    (define (filter pred? lst)
      (foldr (lambda (x nv)
               (if (pred? x)
                   (cons x nv)
                   nv))
             '()
             lst))

    (define (accumulate-i from to term op nv)
        (if (> from to)
            nv
            (accumulate-i (+ from 1) to term op (op nv (term from)))))
    ```
