module Interpreter where

import Parser (Command (..), Program (..), parse)

-- The pen can either be up or down.
data PenState = PenUp | PenDown
  deriving (Eq, Show)

--
-- The state `s` of the turtle at any moment in time consists of:
--   `pen s` : whether the pen is up or down
--   `pos s` : its current location on the canvas as a coordinate
--   `ang s` : the direction it is currently facing in radians
--
data TurtleState = State
  { pen :: PenState,
    pos :: (Int, Int),
    ang :: Float
  }
  deriving (Eq, Show)

-- The turtle initially starts with its pen up, located in the
-- bottom left of the canvas facing due east.
initialTurtleState :: TurtleState
initialTurtleState =
  State
    { pen = PenUp,
      pos = (0, 500),
      ang = 0.0
    }

-- When `d` is an angle expressed in degrees,
-- `degToRad d` evaluates to `d` expressed in radians.
degToRad :: Int -> Float
degToRad d = fromIntegral d * (pi / 180)

-- `computeNewPosition r d (x,y)` evaluates to the position `(xNew,yNew)`
-- that is reached after starting in position `(x,y)` whilst facing at
-- an angle of `r` radians, and moving forward in a straight line for a
-- distance of `d` units.
computeNewPosition :: Int -> Float -> (Int, Int) -> (Int, Int)
computeNewPosition d r (x, y) =
  (x + round (fromIntegral d * cos r), y + round (fromIntegral (-d) * sin r))

--
-- `interpretCmd c s` evaluates to a new turtle state `t` that
-- describes the penstate, position and angle of the turtle after
-- executing the command `c` when started in state `s`.
--
--   e.g. interpretCmd (Fd 10) (State {pen=PenUp, pos=(0,500), ang=0})
--          = State {pen=PenUp, pos=(10,500), ang=0}
--
--        interpretCmd (Lt 90) (State {pen=PenUp, pos=(0,500), ang=0})
--          = State {pen=PenUp, pos=(10,500), ang=pi/2}
--
--        interpretCmd (Fd 50) (State {pen=PenUp, pos=(0,500), ang=pi/2})
--          = State {pen=PenUp, pos=(0,450), ang=pi/2}
--
--        interpretCmd Dn (State {pen=PenUp, pos=(0,500), ang=pi/2})
--          = State {pen=PenDown, pos=(0,500), ang=pi/2}
--
interpretCmd :: Command -> TurtleState -> TurtleState
interpretCmd Up s = s {pen = PenUp}
interpretCmd Dn s = s {pen = PenDown}
interpretCmd (Fd d) s = s {pos = computeNewPosition d (ang s) (pos s)}
interpretCmd (Lt n) s = s {ang = ang s + degToRad n}
interpretCmd (Rt n) s = s {ang = ang s - degToRad n}

--
-- `interpretProg p s` evaluates to a new turtle state describing
-- the penstate, position and angle of the turtle after executing
-- the program `p` when starting in state `s`.
--
--   e.g. Supposing `s = State {pen=PenUp, pos=(0,500), ang=0}`:
--
--        interpretProg (parse "fd(10)") s
--          = s { pos = (10,500) }
--
--        interpretProg (parse "fd(10); fd(20)") s
--          = s { pos = (30,500) }
--
--        interpretProg (parse "fd(10); left(90); fd(50)") s
--          = s { pos = (10,450), ang = pi/2 }
--
interpretProg :: Program -> TurtleState -> TurtleState
interpretProg (Cmd c) = interpretCmd c
interpretProg (Seq p1 p2) = interpretProg p2 . interpretCmd p1

--
-- If `s` is a valid Turt program then `interpret s` evaluates
-- to a turtle state `t` which is the state reached after
-- interpreting the program `parse s` starting from `initialTurtleState`.
--
interpret :: String -> TurtleState
interpret s = interpretProg (parse s) initialTurtleState
