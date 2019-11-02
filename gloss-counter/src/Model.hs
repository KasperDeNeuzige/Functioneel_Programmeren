-- | This module contains the data types
--   which represent the state of the game
module Model where


data Gamestate = Gamestate {objects :: Objects, settings :: Settings, elapsedTime :: Float}
data Objects = Objects {player :: Player, enemies :: Enemies}
data Settings = Settings {}
data Player = Player {spaceShip :: SpaceShip}
data Enemies = Enemies {meteorites :: [Meteorite], spaceShips :: [SpaceShip], bullets :: [Bullet]}
data Meteorite = Meteorite {shapeM :: Shape, healthM :: Float, speedM :: Point}
data SpaceShip = SpaceShip {shapeSP :: Shape, healthSP :: Float, speedSP :: Point, fireRate :: Float, lastFire :: Float}
data Bullet = Bullet {shapeB :: Shape, healthB :: Float, speedB :: Point}
data Shape = Rectangle Point Point Point Point
           | Circle Point Float
           | Triangle Point Point Point
data Point = Point Float Float