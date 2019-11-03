-- | This module contains the data types
--   which represent the state of the game
module Model where


data Gamestate = Gamestate {objects :: Objects, buttonPressed :: ,settings :: Settings, elapsedTime :: Float}
data Objects = Objects {player :: Player, enemies :: Enemies}
data Settings = Settings {}
data Player = Player {spaceShip :: SpaceShip, bullets :: [Bullet]}
data Enemies = Enemies {meteorites :: [Meteorite], spaceShips :: [EnemySpaceShip], bullets :: [Bullet]}
data Meteorite = Meteorite {shapeM :: Shape, healthM :: Float, speedM :: Point}
data SpaceShip = SpaceShip {shapeSP :: Shape, healthSP :: Float, speedSP :: Point, fireRate :: Float, lastFire :: Float}
data EnemySpaceShip = EnemySpaceShip {enemySpaceShip :: SpaceShip}
data Bullet = Bullet {shapeB :: Shape, healthB :: Float, speedB :: Point}
data Shape = Rectangle Point Point Point Point        --Assumed to be parralel with x and y axis
           | Circle Point Float
           | Triangle Point Point Point
data Point = Point Float Float