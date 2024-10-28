# Упражнение 6

```scheme
(define (foldr op init l)
  (if (null? l)
      init
      (op (car l) (foldr op  init (cdr l)))))

(define (foldl op init l)
  (if (null? l)
      init
      (foldl op (op init (car l)) (cdr l))))
```

> **Note**
> `foldr` -> многото скоби са **вдясно**
> `foldl` -> многото скоби са **вляво**

```scheme
(foldr * 1 '(1 2 3)) ;; => (1 * (2 * (3 * 1)))
(foldl * 1 '(1 2 3)) ;; => (((1 * 1) * 2) * 3)
```


# Задачи:

1. Дефинирайте функция `(sum l)` чрез `foldr`/`foldl`

```scheme
(sum '(1 2 3)) ;; => 6
```

2. Дефинирайте функция `(len l)` чрез `foldr`/`foldl`

```scheme
(len '())      ;; => 0
(len '(123))   ;; => 1
(len '(1 2 3)) ;; => 3
```

3. Дефинирайте функците `(any? p l)` и `(all? p l)` чрез `foldr`/`foldl`

```scheme
(any? odd? '(1 2 3 4 5)) ;; => #t
(any? odd? '(2 4 6))     ;; => #f

(all? even? '(1 2 3 4 5)) ;; => #f
(all? even? '(2 4 6))     ;; => #t
```

4. Дефинирайте функциите `foldr1` и `foldl1`, които са подобни на `foldr`/`foldl`, но не приемат първичен елемент (гърмят при празен списък)

```scheme
(foldr1 + '(1 2 3)) ;; => (1 + (2 + 3))
(foldl1 + '(1 2 3)) ;; => ((1 + 2) + 3)
```

5. Дефинирайте функциите `minimum` и `maximum` чрез `foldr1`/`foldl1`
```scheme
(minimum '(5 7 1 3)) ;; => 1
(maximum '(5 7 1 3)) ;; => 7
```

6. Дефинирайте функция `map` чрез `foldr`/`foldl`

```scheme
(map (lambda (x) (* x 2)) '(1 2 3)) ;; => '(2 4 6)
```

7. Дефинирайте функция `filter` чрез `foldr`/`foldl`

```scheme
(filter odd? '(1 2 3 4 5)) ;; => '(1 3 5)
```

8. Дефинирайте функция `(reverse l)` чрез `foldr`/`foldl`

```scheme
(reverse '(1 2 3)) ;; => '(3 2 1)
```

9. Дефинирайте функция `(take n l)`

```scheme
(take 5 '(1 2 3 4 5 6 7 8 9 10)) ;; => '(1 2 3 4 5)
(take 0 '(1 2 3)) ;; => '()
(take 5 '(1 2 3)) ;; => '(1 2 3)
```

- Дефинирайте функция `(drop n l)`

```scheme
(drop 5 '(1 2 3 4 5 6 7 8 9 10)) ;; => '(6 7 8 9 10)
(drop 0 '(1 2 3)) ;; => '(1 2 3)
(drop 5 '(1 2 3)) ;; => '()
```

10. Дефинирайте функция `(take-while p? l)`

```scheme
(take-while (lambda (x) (> x 5)) '(8 7 6 5 4 3)) ;; => '(8 7 6)
(take-while odd? '(2 1 1 1 1)) ;; => '()
```

- Дефинирайте функция `(drop-while p? l)`

```scheme
(drop-while (lambda (x) (> x 5)) '(8 7 6 5 4 3)) ;; => '(5 4 3)
(drop-while odd? '(2 1 1 1 1)) ;; => '(2 1 1 1 1)
```

11. Дефинирайте функция `(zip l1 l2)`

```scheme
(zip '(1 2 3) '("a" "b" "c")) ;; => '((1 . "a") (2 . "b") (3 . "c"))
(zip '(1 2) '(3 4 5 6 7)) ;; => '((1 . 3) (2 . 4))
```

- Дефинирайте функция `(zip-with f l1 l2)`

```scheme
(zip-with + '(1 2 3) '(5 4 2)) ;; => '(6 6 5)
```
