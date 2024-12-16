main :: IO()
main = do
    print $ season2017

type Name = String
type PointsWon = Int
type SetsLost = Int
type TennisPlayer = (Name, PointsWon, SetsLost)
type Tournament = (Name, [TennisPlayer])

season2017 :: [Tournament]
season2017 = [
    ("Australian Open", [
        ("Roger Federer", 2000, 10),
        ("Rafael Nadal", 1200, 12),
        ("Grigor Dimitrov", 720, 5),
        ("Stan Wawrinka", 720, 7)
        ]),
    ("Roland Garros", [
        ("Rafael Nadal", 2000, 0),
        ("Stan Wawrinka", 1200, 5),
        ("Dominic Thiem", 720, 6),
        ("Andy Murray", 720, 7)
        ]),
    ("Wimbledon", [
        ("Roger Federer", 2000, 0),
        ("Marin Cilic", 1200, 5),
        ("Tomas Berdych", 720, 3),
        ("Sam Querrey", 720, 11)
        ]),
    ("US Open", [
        ("Rafael Nadal", 2000, 4),
        ("Kevin Anderson", 1200, 8),
        ("Juan Martin del Potro", 720, 7),
        ("Pablo Carreno Busta", 720, 9)
        ])
    ]

-- izmislete si zada4i