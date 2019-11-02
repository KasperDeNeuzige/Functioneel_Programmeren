module Datastructures where 
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

  class Movement a where 
      movement :: a -> a

  newLocation :: Shape -> Point -> Shape
  newLocation (Rectangle a b c d) p = Rectangle (a + p) (b + p) (c + p) (d + p)
  newLocation (Circle a f) p        = Circle (a + p) f
  newLocation (Triangle a b c) p     = Triangle (a + p) (b + p) (c + p)

  addPoints :: Point -> Point -> Point
  addPoints (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

  instance Movement Gamestate where 
      movement gs = gs {objects = movement objects}
  instance Movement Objects where
      movement ob = ob {player = movement player, enemies = map movement enemies}
  instance Movement Player where
      movement pl = pl {spaceShips = movement spaceShip}
  instance Movement SpaceShip where
      movement sp = sp {shapeSP = newLocation shapeSP speedSP}
  instance Movement Enemies where
      movement en = en {meteorites = map movement meteorites, map movement spaceShips, map movement bullets}
  instance Movement Meteorite where
      movement mt = mt {shapeM = newLocation shapeM speedM}
  instance Movement Bullet where
      movement bu = bu {shapeB = newLocation shapeB speedB}