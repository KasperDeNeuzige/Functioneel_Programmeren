

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
    updateEvent e g gs = gs {objects = updateEvent e gs objects} -- removeDead (collision (movement objects))
    
instance Update Objects where
    updateEvent e g ob = ob {player = updateEvent e g player, enemies = (collision (movement (update g enemies)))}

instance Update Player where
    updateEvent e g pl | e == ????
                       | e == 
    update g pl = pl {spaceShip = update g spaceShip, bullets = map (update g) bullets}

instance Update Enemies where
    update g en = en {spaceShips = map (update g) spaceShips, meteorites = map (update g) meteorites, bullets = map (update g) bullets}

instance Update EnemySpaceShip where
    update sp g | sp {enemySpaceShip {lastFire}} + sp {enemySpaceShip {fireRate}} > g {elapsedTime} = Bullet initShape

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

instance Movement Objects where
    movement ob = ob {player = movement player, enemies = movement enemies}

instance Movement Player where
    movement pl = pl {spaceShip = movement spaceShip, bullets = map movement bullets}
    
instance Movement Enemies where
    movement en = en {meteorites = map movement meteorites, spaceShips = map movement spaceShips, bullets = map movement bullets}

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
isInside (Circle p1 d1) (Rectangle q1 q2 q3 q4)            = any [pointDistance p1 x < d1 | x <- [q1 : q2 : q3 : q4 : []]]  -- Isn't precise enough
isInside (Circle p1 d1) (Triangle q1 q2 q3 )               = any [pointDistance p1 x < d1 | x <- [q1 : q2 : q3 : []]]       -- Isn't precise enough
isInside r@(Rectangle p1 p2 p3 p4) (Rectangle q1 q2 q3 q4) = any [pointInsideShape r x | x <- [q1 : q2 : q3 : q4 : []]]
isInside r@(Rectangle p1 p2 p3 p4) (Triangle q1 q2 q3)     = any [pointInsideShape r x | x <- [q1 : q2 : q3 : []]]          -- Isn't precise enough
isInside (Triangle p1 p2 p3) (Triangle q1 q2 q3)           = undefined
isInside p q                                               = isInside q p
    
instance Collision EnemySpaceShip where
    collision ob sp | isInside (shapeSP (enemySpaceShip sp) (shapeSP (spaceShip (player ob))) = sp {enemySpaceShip = enemySpaceShip {healthSP = healthSp - 1}}
                    | otherwise                                                               = sp
instance Collision Player where
    collision ob pl | and (map (isInside (shapeSP (spaceShip pl))) (concat (meteorites (enemies ob) : spaceShips (enemies ob) : bullets (enemies ob) : []))) = pl {spaceShip = spaceShip {healthSP = healthSP - 1}}
                    | otherwise                                                                                                                              = pl
instance Collision Meteorite where
    collision ob mt | isInside (shapeM mt) (shapeSP (spaceShip (player ob))) = mt {healthMT = healthMT - 1}
                    | otherwise                                              = mt
instance Collision Bullet where 
    collision ob bt | isInside (shapeM bt) (shapeSP (spaceShip (player ob))) = bt {healthB = healthB - 1}
                    | otherwise                                              = bt

    
    
    
class RemoveDead a where
removeIfDead :: a -> Maybe a  

instance RemoveDead SpaceShip
removeIfDead sp | healthSP sp <= 0 = Nothing
                | otherwise        = Just sp

instance RemoveDead Meteorite
removeIfDead mt | healthM mt <= 0 = Nothing
                | otherwise        = Just mt
                    
instance RemoveDead Bullet
removeIfDead bt | healthB bt <= 0 = Nothing
                | otherwise       = Just bt



newBullet :: Bullet
newBullet = Bullet Init 1 (Point 0 0)

newMeteorite :: Meteorite
newMeteorite = Meteorite Init 1 (Point 0 0)

newSpaceShip :: SpaceShip
newSpaceShip = SpaceShip Init 1 (Point 0 0) 0 0


class InitOb a where
initialize :: Point -> Point -> a -> a 

startingPosition :: Point
startingPosition = -- Random starting position on top/right of screen

pointToRectangle :: Point -> Float -> Float -> Rectangle
pointToRectangle center height width = Rectangle (addPoints center (Point halfHeight (negate halfWidth))) (addPoints center (Point halfHeight halfWidth)) (addPoints center (Point (negate halfHeight) halfWidth) (addPoints center (Point halfHeight (negate halfWidth))))
    where halfHeight = height/2
          halfWidth  = width/2

instance InitOb Bullet where 
initialize pos speed bt = bt {shapeB = Shape pos 10, healthB = 1, speedB = speed}

instance InitOb Meteorite where
initialize pos speed mt = mt {shapeM = Shape pos 10, healthM = 1, speedM = speed}

instance InitOb SpaceShip where
initialize pos speed sp = sp {shapeSP = pointToRectangle pos 10 10, healthSp = 1, speedSP = speed, fireRate = 1000, lastFire = 0}

toX :: Point -> Float
toX Point x y = x

toY :: Point -> Float
toY Point x y = y

class Draw a where
    draw :: a -> Picture
    
instance Draw SpaceShip where
draw s = translate (toX (speedSP s)) (toY (speedSP s)) (color blue (shapeSP s))

instance Draw EnemySpaceShip where
draw s = translate (toX (speedSP enemySpaceShip s)) (toY (speedSP enemySpaceShip s)) (color red (shapeSP s))

instance Draw Bullet where
draw b = translate (toX (speedB b)) (toY (speedB b)) (color yellow (shapeB b))

instance Draw Meteorite where
draw m = translate (toX (speedM m)) (toY (speedM m)) (color white (shapeM m))






   








    
    
    
