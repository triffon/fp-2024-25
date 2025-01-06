# Functor, Applicative, Monad, Do

Дефинирайте рекурсивен алгебричен тип `Chain`, който има два конструктора:
 - `Singleton`, приемащ един параметър `a`
 - `Append`, приемащ два параметъра `Chain a`

Дефинирайте собствен type class `Appendable` и съдържа следните функции:
 - `prepend :: a -> c a -> c a`
 - `append :: c a -> a -> c a`
 - `concatenate :: c a -> c a -> c a`

Създайте инстанция на класа `Appendable` с типа `Chain`.

Дефинирайте алгебричен тип `Validated`, който има два конструктора:
 - `Valid`, приемащ един параметър `a`
 - `Invalid`, приемащ един параметър `Chain e`

Създайте инстанция на класа `Monad` с типа `Validated`.

> Подсказка: Вдъхновете се от поведението на `Either`

Пресъздайте примера от code snippet-а `06-do.hs` чрез `Validated`.