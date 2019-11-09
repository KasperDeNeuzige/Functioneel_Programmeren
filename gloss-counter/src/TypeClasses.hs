

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
    updateEvent :: Action -> Gamestate -> a -> a
    
instance Update Gamestate where
    updateEvent e g gs = gs {objects = collision (movement (updateEvent e gs (objects gs)))} -- removeDead (collision (movement objects))
    
instance Update Objects where
    updateEvent e g ob = ob {player = updateEvent e g (player ob), enemies = update g (enemies ob)}

{-instance Update Player where
    updateEvent e g pl | e == Shoot = 
                       | e == 
    update g pl = pl {spaceShip = update g (spaceShip pl), bullets = map (update g) (bullets pl)}-}

instance Update Enemies where
    update g en = en {spaceShips = map (update g) (spaceShips en), meteorites = map (update g) (meteorites en)}

instance Update EnemySpaceShip where
    update g ensp@EnemySpaceShip {enemySpaceShip = sp, bulletsEN = bt} | lastFire sp + fireRate sp > elapsedTime g = ensp {sp = update g sp, bt = (update g (InitOb shapeToPoint (shapeSP sp) bulletSpeed newBullet)) : (map (update g) bt)}
                                                                       | otherwise                                 = ensp {sp = update g sp, bt = map (update g) bt}

instance Update Meteorite where
    update g mt = mt

instance Update Bullet where
    update g bu = bu



class Movement a where 
    movement :: a -> a

newLocation :: Shape -> Point -> Shape
newLocation (Rectangle a b c d) p = Rectangle (addPoints a p) (addPoints b p) (addPoints c p) (addPoints d p)
newLocation (Circle a f) p        = Circle (addPoints a p) f
newLocation (Triangle a b c) p    = Triangle (addPoints a p) (addPoints b p) (addPoints c p)

shapeToPoint :: Shape -> Point
shapeToPoint (Circle p d)                                                     = p 
shapeToPoint (Rectangle (Point x1 y1)(Point x2 y2)(Point x3 y3)(Point x4 y4)) = Point ((x1 + x2 + x3 + x4)/4) ((y1 + y2 + y3 + y4)/4)

addPoints :: Point -> Point -> Point
addPoints (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

instance Movement Objects where
    movement ob = ob {player = movement (player ob), enemies = movement (enemies ob)}

instance Movement Player where
    movement pl = pl {spaceShip = movement (spaceShip pl), bullets = map movement (bullets pl)}
    
instance Movement Enemies where
    movement en = en {meteorites = map movement (meteorites en), spaceShips = map movement (spaceShips en)}

instance Movement EnemySpaceShip where
    movement sp = sp {spaceShip = movement (spaceShip sp), bullets = map movement (bullets sp)}

instance Movement SpaceShip where
    movement sp = sp {shapeSP = newLocation (shapeSP sp) (speedSP sp)}
    
instance Movement Meteorite where
    movement mt = mt {shapeM = newLocation (shapeM mt) (speedM mt)}
    
instance Movement Bullet where
    movement bu = bu {shapeB = newLocation (shapeB bu) (speedB bu)}




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
isInside (Circle p1 d1) (Rectangle q1 q2 q3 q4)            = any [pointDistance p1 x < d1 | x <- [q1 : q2 : q3 : q4 : []]]  -- Isn't precise enough
isInside (Circle p1 d1) (Triangle q1 q2 q3 )               = any [pointDistance p1 x < d1 | x <- [q1 : q2 : q3 : []]]       -- Isn't precise enough
isInside r@(Rectangle p1 p2 p3 p4) (Rectangle q1 q2 q3 q4) = any [pointInsideShape r x | x <- [q1 : q2 : q3 : q4 : []]]
isInside r@(Rectangle p1 p2 p3 p4) (Triangle q1 q2 q3)     = any [pointInsideShape r x | x <- [q1 : q2 : q3 : []]]          -- Isn't precise enough
isInside (Triangle p1 p2 p3) (Triangle q1 q2 q3)           = undefined
isInside p q                                               = isInside q p
    
instance Collision EnemySpaceShip where
    collision ob sp | and map (isInside (shapeSP (enemySpaceShip sp))) (shapeSP (spaceShip (player ob))) : (map shapeB(bullets(player ob))) = sp {enemySpaceShip = enemySpaceShip sp{healthSP = (healthSp (enemySpaceShip sp)) - 1}}
                    | otherwise                                                                                                             = sp
instance Collision Player where
    collision ob pl | and map (isInside (shapeSP (spaceShip pl))) (map shapeM (meteorites (enemies ob)) ++ (map shapeSP(enemySpaceShip( spaceShips (enemies ob))))) = pl {spaceShip = spaceShip pl {healthSP = (healthSP (spaceShip pl)) - 1}}
                    | otherwise                                                                                                                                     = pl
instance Collision Meteorite where
    collision ob mt | isInside (shapeM mt) (shapeSP (spaceShip (player ob))) = mt {healthMT = (healthMT mt) - 1}
                    | otherwise                                              = mt
instance Collision Bullet where 
    collision ob bt | isInside (shapeM bt) (shapeSP (spaceShip (player ob))) = bt {healthB = (healthB bt) - 1}
                    | otherwise                                              = bt

    
    
    
class RemoveDead a where
removeIfDead :: a -> Maybe a 


instance RemoveDead Player where
    removeIfDead pl | removeIfDead (spaceShip pl) == Nothing = Nothing
                    | otherwise                              = Just pl

instance RemoveDead EnemySpaceShip where
    removeIfDead en | removeIfDead (enemySpaceShip en) == Nothing = Nothing
                    | otherwise                                   = Just en

instance RemoveDead SpaceShip where
removeIfDead sp | healthSP sp <= 0 = Nothing
                | otherwise        = Just sp

instance RemoveDead Meteorite where
removeIfDead mt | healthM mt <= 0 = Nothing
                | otherwise       = Just mt
                    
instance RemoveDead Bullet where
removeIfDead bt | healthB bt <= 0 = Nothing
                | otherwise       = Just bt



newBullet :: Bullet
newBullet = Bullet Init 1 (Point 0 0)

bulletSpeed :: Point
bulletSpeed = (0, -100)

newMeteorite :: Meteorite
newMeteorite = Meteorite Init 1 (Point 0 0)

newSpaceShip :: SpaceShip
newSpaceShip = SpaceShip Init 1 (Point 0 0) 0 0

newPlayer :: Player
newPlayer = Player newSpaceShip []

newEnemySpaceShip :: EnemySpaceShip
newEnemySpaceShip = EnemySpaceShip newSpaceShip []


class InitOb a where
initialize :: Point -> Point -> a -> a 

startingPosition :: Point
startingPosition = undefined-- Random starting position on top/right of screen

pointToRectangle :: Point -> Float -> Float -> Rectangle
pointToRectangle center height width = Rectangle (addPoints center (Point halfHeight (negate halfWidth))) (addPoints center (Point halfHeight halfWidth)) (addPoints center (Point (negate halfHeight) halfWidth) (addPoints center (Point halfHeight (negate halfWidth))))
    where halfHeight = height/2
          halfWidth  = width/2

instance InitOb Bullet where 
initialize pos speed bt = bt {shapeB = Shape pos 10, healthB = 1, speedB = speed}

instance InitOb Meteorite where
initialize pos speed mt = mt {shapeM = Shape pos 10, healthM = 1, speedM = speed}

instance InitOb SpaceShip where
initialize pos speed sp = sp {shapeSP = pointToRectangle pos 10 10, healthSP = 1, speedSP = speed, fireRate = 1000, lastFire = 0}

instance InitOb Player where
initialize pos speed pl = pl {spaceShip = (InitOb pos speed spaceShip) {healthSP = 3}}

instance InitOb EnemySpaceShip where
initialize pos speed sp = sp {enemySpaceShip = InitOb pos speed enemySpaceShip}