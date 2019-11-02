

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
        update g pl = pl {spaceShip = update g spaceShip}

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
        collision :: a -> Objects -> a

    instance Collision SpaceShip where
        collision = --Health eraf en misschien movement terug