-- | This module contains the data types
--   which represent the state of the game
module Model where


data Gamestate = Gamestate {objects :: Objects, buttonPressed :: Action, elapsedTime :: Float}
data Objects = Objects {player :: Player, enemies :: Enemies}

data Player = Player {spaceShip :: SpaceShip, bulletsPL :: [Bullet], action :: Action}
data Enemies = Enemies {meteorites :: [Meteorite], spaceShips :: [EnemySpaceShip]}
data Meteorite = Meteorite {shapeM :: Shape, healthM :: Float, speedM :: Point}
data SpaceShip = SpaceShip {shapeSP :: Shape, healthSP :: Float, speedSP :: Point, fireRate :: Float, lastFire :: Float}
data EnemySpaceShip = EnemySpaceShip {enemySpaceShip :: SpaceShip, bulletsEN :: [Bullet]}
data Bullet = Bullet {shapeB :: Shape, healthB :: Float, speedB :: Point}

data Action = Shoot Action
            | Move Point Action
            | Pause Action
            | Nothing

data Shape = Rectangle Point Point Point Point        --Assumed to be parralel with x and y axis
           | Circle Point Float
           | Triangle Point Point Point
           | Init
data Point = Point Float Float