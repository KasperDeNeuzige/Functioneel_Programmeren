-- | This module contains the data types
--   which represent the state of the game
module Model where


data Gamestate = Gamestate {
           player :: Player, 
           enemies :: [Enemy],
           settings :: Settings,
           elapsedTime :: Float, 
           gameOver :: Bool}
data Settings = Settings {}
data Player = Player {spaceShip :: SpaceShip, bullets :: [Bullet]}
data Enemy = Meteorite Shape Healt Location Speed | EnemySpaceship Spaceship [Bullet]
data Meteorite = Meteorite {shapeM :: Shape, healthM :: Health, locationM :: Location, speedM :: Speed}
data SpaceShip = SpaceShip {shapeSP :: Shape, healthSP :: Health, locationS = Location, speedSP :: Speed, fireRate :: Float, lastFire :: Float}
data EnemySpaceShip = EnemySpaceShip {enemySpaceShip :: SpaceShip, bullets :: [Bullet]}
data Bullet = Bullet {shapeB :: Shape, speedB :: Point} --Heb hier health er uit gehaald, leek me niet nodig bij een bullet?
data Shape = Rectangle Point Point Point Point        --Assumed to be parralel with x and y axis
           | Circle Point Float
           | Triangle Point Point Point
           | Init
data Point = Point Float Float
data Location = Location Point
data Health = Health Float
data Speed = Speed Float --Speed was oorspronkelijk een point? en er was geen location bij de props
