import Graphics.Gloss
import Graphics.Gloss.Interface.Game

data Direction = U | R | D | L

data World = World 
             { snake :: [Point]
             , size :: Int
             , apples :: [Point]
             , direction :: Direction
             , time :: Float
             }

timestep = 0.5

initial :: World
initial = World [(0,0), (1,0), (2,0)] 5 [(0,4)] U 0

event :: Event -> World -> World
event (EventKey (SpecialKey key) Up _ _) world = changeDirection key world
event _ w = w

changeDirection :: SpecialKey -> World -> World
changeDirection KeyUp world = world { direction = U }
changeDirection KeyRight world = world { direction = R }
changeDirection KeyDown world = world { direction = D }
changeDirection KeyLeft world = world { direction = L }

step :: Float -> World -> World
step dt world = if newTime < timestep then
  world { time = newTime } else
  world { time = newTime - timestep, snake = moveSnake world }
  where newTime = (time world) + dt

moveSnake :: World -> [Point]
moveSnake world = newHead : newTail where
  newHead = addDir (direction world) x
  s@(x:xs) = snake world
  newTail = init s

addDir U (x,y) = (x, y+1)
addDir R (x,y) = (x+1, y)
addDir D (x,y) = (x, y-1)
addDir L (x,y) = (x-1, y)

draw world = Pictures $
  map drawSnakeSegment (snake world) ++
  map drawApple (apples world)

drawTimestamp world = translate (-225) (-225) $ scale 0.2 0.2 $ text $ show $ time world

drawApple (x,y) = color red $ translate (x*10) (y*10) $ circleSolid 4

drawSnakeSegment (x,y) = translate (x*10) (y*10) $ Circle 4
