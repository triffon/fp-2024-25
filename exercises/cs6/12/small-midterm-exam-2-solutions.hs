import Prelude hiding (id)

-- Работим с устройства, които са описани със следния тип:
data Device = Device
  { id :: Int, -- Уникален идентификатор на устройството
    owner :: Int, -- Идентификатор на собственика на устройството
    server :: String, -- Име на сървъра, на който се съхраняват данни за устройството
    shared :: Bool -- Булев флаг: True, ако устройството е споделено; False в противен случай
  }

-- Трябва да се напише функция groupSharedDevices,
-- която приема списък от устройства и връща групиране по сървър,
-- като всяка група от сървърите е допълнително групирана по собствениците на устройствата.
--
-- Вход: Списък от устройства.
--
-- Изход: Списък от двойки, където:
-- - Първият елемент е името на сървъра.
-- - Вторият елемент е списък от двойки, съдържащи:
--   - Идентификатора на собственика.
--   - Списък с идентификаторите на устройствата, които принадлежат на този собственик на съответния сървър.

-- Разделяме списък по предикат с partition
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p xs = (truthy, falsy)
  where
    truthy = filter p xs
    falsy = filter (not . p) xs

-- Трябва да е очевидно че ни се налага да правим това групиране 2 пъти,
-- затова си пишем тази функция. Другият вариант, разбира се,
-- е да направим първото групиране и да си copy-paste-нем кода.
--
-- Използваме partition за да отделим всяка група
groupBy :: (Eq b) => (a -> b) -> [a] -> [(b, [a])]
groupBy _ [] = []
groupBy f ys@(x : xs) = (key, group) : groupBy f rest
  where
    key = f x -- Ключа на текущата група (само защото го използваме 2 пъти)
    (group, rest) = partition (\y -> f y == key) ys

-- Може и без тези помощни функции, но спестяват няколко вложени ламбди
mapSnd :: (a -> b) -> (c, [a]) -> (c, [b])
mapSnd f (x, ys) = (x, map f ys)

mapSnds :: (a -> b) -> [(c, [a])] -> [(c, [b])]
mapSnds f = map (mapSnd f)

-- Филтрираме си споделените устройства и ги групираме по server
groupSharedDevicesByServer :: [Device] -> [(String, [Device])]
groupSharedDevicesByServer = groupBy server . filter shared

-- Групирането по owner е аналогично,
-- но накрая трябва да заменим всеки Device с Int (неговото id)
--
-- Т.е. правим превод от [(String, [(Int, [Device])])] към [(String, [(Int, [Int])])],
-- затова са помощните функции mapSnd(s)
--
-- В случея идеята да се изнесат mapSnd(s) се случва наобратно:
-- 1. Пишем някакви сложни трансформации над резултата
-- 2. Изнасяме същите тези трансформации в генерализирани функции
groupSharedDevices :: [Device] -> [(String, [(Int, [Int])])]
groupSharedDevices = mapSnds (mapSnd id) . groupedDevices
  where
    groupByOwner (server, devices) = (server, groupBy owner devices)
    groupedDevices = map groupByOwner . groupSharedDevicesByServer

exampleDevices :: [Device]
exampleDevices =
  [ Device 1 101 "ServerA" True,
    Device 2 102 "ServerA" False,
    Device 3 101 "ServerA" True,
    Device 4 103 "ServerB" True,
    Device 5 104 "ServerB" False,
    Device 6 103 "ServerB" True,
    Device 7 101 "ServerA" False,
    Device 8 102 "ServerC" True,
    Device 9 101 "ServerC" True,
    Device 10 105 "ServerC" False,
    Device 11 104 "ServerD" True,
    Device 12 103 "ServerD" True,
    Device 13 101 "ServerD" False,
    Device 14 102 "ServerA" True,
    Device 15 105 "ServerB" False,
    Device 16 106 "ServerB" True,
    Device 17 103 "ServerC" True,
    Device 18 104 "ServerC" False,
    Device 19 105 "ServerD" True,
    Device 20 101 "ServerA" True
  ]

-- Пример:
-- >>> groupSharedDevices exampleDevices
-- [("ServerA",[(101,[1,3,20]),(102,[14])]),("ServerB",[(103,[4,6]),(106,[16])]),("ServerC",[(102,[8]),(101,[9]),(103,[17])]),("ServerD",[(104,[11]),(103,[12]),(105,[19])])]
