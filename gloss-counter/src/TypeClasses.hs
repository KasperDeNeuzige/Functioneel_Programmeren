

module TypeClasses where
    
    import Model 

    import Graphics.Gloss
    import Graphics.Gloss.Interface.IO.Game
    import System.Random

    
-- Action . Movement
-- Collision
-- Draw

    class Update a where
        update :: Gamestate -> a -> a 
        updateEvent :: Event -> Gamestate -> a -> a
    
    instance Update Gamestate where
        updateEvent e g gs = gs {objects = updateEvent e gs objects}
    
    instance Update Objects where
        updateEvent e g ob = ob {player = updateEvent e g player, enemies = update g enemies}

    instance Update Player where
        updateEvent e g pl | e == ????
                           | e == 
        update g pl = pl {spaceShip = update g spaceShip, bullets = map (update g) bullets}

    instance Update Enemies where
        update g en = en {spaceShips = map (update g) spaceShips, meteorites = map (update g) meteorites, bullets = map (update g) bullets}

    instance Update SpaceShip where
        update sp g = collision (movement sp) 

    instance Update Meteorite where
        update mt g = movement mt

    instance Update Bullet where
        update bu g = movement bu
    




    class Movement a where 
        movement :: a -> a

    newLocation :: Shape -> Point -> Shape
    newLocation (Rectangle a b c d) p = Rectangle (addPoints a p) (addPoints b p) (addPoints c p) (addPoints d p)
    newLocation (Circle a f) p        = Circle (addPoints a p) f
    newLocation (Triangle a b c) p    = Triangle (addPoints a p) (addPoints b p) (addPoints c p)

    addPoints :: Point -> Point -> Point
    addPoints (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)
    
    instance Movement SpaceShip where
        movement sp = sp {shapeSP = newLocation shapeSP speedSP}
    
    instance Movement Meteorite where
        movement mt = mt {shapeM = newLocation shapeM speedM}
    
    instance Movement Bullet where
        movement bu = bu {shapeB = newLocation shapeB speedB}




    class Collision a where
    collision :: Objects -> a -> a

    pointDistance :: Point -> Point -> Float
    pointDistance (Point x1 y1) (Point x2 y2) = sqrt (a * a + b * b)
        where a = abs(x1 - x2)
              b = abs(y1 - y2)

    pointInsideShape :: Shape -> Point -> Bool
    pointInsideShape (Rectangle (Point p1 q1) (Point p2 q2) (Point p3 q3) (Point p4 q4)) (Point x y) = (any [x > b | b <- [p1 : p2 : p3 : p4 : []]]) && (any [x < c | c <- [p1 : p2 : p3 : p4 : []]]) &&
                                                                                                       (any [y > d | d <- [q1 : q2 : q3 : q4 : []]]) && (any [y < e | e <- [q1 : q2 : q3 : q4 : []]])
    isInside :: Shape -> Shape -> Bool
    isInside (Circle p1 d1) (Circle p2 d2)                     = (pointDistance p1 p2) < (d1 + d2)
    isInside (Circle p1 d1) (Rectangle q1 q2 q3 q4)            = any [pointDistance p1 x < d1 | x <- [q1 : q2 : q3 : q4 : []]]
    isInside (Circle p1 d1) (Triangle q1 q2 q3 )               = any [pointDistance p1 x < d1 | x <- [q1 : q2 : q3 : []]]
    isInside r@(Rectangle p1 p2 p3 p4) (Rectangle q1 q2 q3 q4) = any [pointInsideShape r x | x <- [q1 : q2 : q3 : q4 : []]]
    isInside r@(Rectangle p1 p2 p3 p4) (Triangle q1 q2 q3)     = any [pointInsideShape r x | x <- [q1 : q2 : q3 : []]]
    isInside (Triangle p1 p2 p3) (Triangle q1 q2 q3)           = undefined
    isInside p q                                               = isInside q p
    
    instance Collision EnemySpaceShip where
        collision ob sp | isInside (shapeSP (enemySpaceShip sp) (shapeSP (spaceShip (player ob))) = sp {enemySpaceShip = enemySpaceShip {healthSP = healthSp - 1}}
                        | otherwise                                                               = sp
    instance Collision 


    class RemoveDead a where
    removeIfDead :: a -> Maybe a  

    instance RemoveDead SpaceShip
    removeIfDead sp | healthSP sp <= 0 = Nothing
                    | otherwise        = Just sp

    instance RemoveDead Meteorite
    removeIfDead mt | healthMT mt <= 0 = Nothing
                    | otherwise        = Just mt
                    
    instance RemoveDead Bullet
    removeIfDead bt | healthB bt <= 0 = Nothing
                    | otherwise       = Just bt