-- Работим с устройства, които са описани със следния тип:
data Device = Device
  { id :: Int,       -- Уникален идентификатор на устройството
    owner :: Int,    -- Идентификатор на собственика на устройството
    server :: String, -- Име на сървъра, на който се съхраняват данни за устройството
    shared :: Bool   -- Булев флаг: True, ако устройството е споделено; False в противен случай
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
--
groupSharedDevices :: [Device] -> [(String, [(Int, [Int])])]
groupSharedDevices = undefined

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
