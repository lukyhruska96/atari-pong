module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Environment
import Data.Maybe

main :: IO ()
    
main = do
    (width, height) <- getScreenSize
    play gameDisplay bgColor fps (initModel width height) toPicture handleInput step

-- display mode
gameDisplay :: Display
gameDisplay = FullScreen

-- Background color
bgColor :: Color
bgColor = makeColor 0 0 0 1

-- Number of simulation steps to take for each second of real time
fps :: Int
fps = 60

playerWidth :: Float
playerWidth = 80

playerMoveStep :: Float
playerMoveStep = 4.0


pThickness :: Float
pThickness = 2.0

ballRadius :: Float
ballRadius = 2.0

ballMoveSpeed :: Float
ballMoveSpeed = 150.0

winningPoints :: Int
winningPoints = 11

-- Initial game state
initModel :: Int -> Int -> GameState
initModel width height = Game 
                0 0
                (-(fromIntegral width)/2+pThickness+ballRadius, 0) (2, -1) 0 0 
                Up Up Up Up False width height

-- A function to convert the world a picture
toPicture :: GameState -> Picture
toPicture state = Pictures $ (renderWin state):[
    Color white $ line [(p1x, (p1location state)-(playerWidth/2)), (p1x, (p1location state)+(playerWidth/2))],
    Color white $ line [(p2x, (p2location state)-(playerWidth/2)), (p2x, (p2location state)+(playerWidth/2))],
    translate (fst $ ballLocation state) (snd $ ballLocation state) $ Color white $ circleSolid ballRadius,
    translate (-(fromIntegral $ width state)/4-30) 0.0 $ Color white $ Text $ show (p1score state),
    translate ((fromIntegral $ width state)/4-30) 0.0 $ Color white $ Text $ show (p2score state)
    ]
    where
        p1x = - fromIntegral (width state) / 2 + pThickness
        p2x = fromIntegral (width state) / 2 - pThickness

-- A function to handle input events
handleInput :: Event -> GameState -> GameState
handleInput (EventKey (Char 'q') state _ _) game = game {p1UState = state}
handleInput (EventKey (Char 'a') state _ _) game = game {p1DState = state}
handleInput (EventKey (Char 'p') state _ _) game = game {p2UState = state}
handleInput (EventKey (Char 'l') state _ _) game = game {p2DState = state}
handleInput _ game = game

-- A function to step the world one iteration
type Period = Float
step :: Period -> GameState -> GameState
step p state
    | stopped state = state
    | otherwise     = checkScore $ 
    moveBall p $
    setP1Location (movePlayer (p1location state) (height state) (p1UState state) (p1DState state)) $ 
    setP2Location (movePlayer (p2location state) (height state) (p2UState state) (p2DState state)) state

checkScore :: GameState -> GameState
checkScore state
    | (p2s >= winningPoints || p1s >= winningPoints) && abs(p2s - p1s) > 2  = state {stopped=True}
    | otherwise                                                             = state
    where
        p1s = p1score state
        p2s = p2score state
    

-- Add the vector of movement to ball
moveBall :: Period -> GameState -> GameState
moveBall p state 
    | isJust pCollision = let value = fromJust pCollision
                              (x, y) = rotateVect (negate $ vectorSize $ ballVector state, 0) value
                        in
                            state {ballVector = (x,y), ballLocation=(minMaxOrVal (p1x+ballRadius+1) (p2x-ballRadius-1) x2, y2)}
    | isNothing pCollision && x2 < p1x-ballRadius  = state {
                            ballLocation = (p1x+ballRadius, 0), 
                            p1location = 0,
                            p2location = 0,
                            ballVector = (abs $ fst $ ballVector state, snd $ ballVector state),
                            p2score = (p2score state)+1
                        }
    | isNothing pCollision && x2 > p2x+ballRadius  = state {
                            ballLocation = (p2x-ballRadius, 0), 
                            p1location = 0,
                            p2location = 0,
                            ballVector = (negate $ abs $ fst $ ballVector state, snd $ ballVector state),
                            p1score = (p1score state)+1
                        }
    | wCollision    = state {ballLocation=(x1+x2, y2-y1), ballVector=(fst $ ballVector state, negate $ snd $ ballVector state)}
    | otherwise     = state {ballLocation=(x1+x2, minMaxOrVal (-max) max (y1+y2))}
    where 
        max = fromIntegral (height state) / 2
        pCollision = playerColision state
        wCollision = wallCollision state
        (x1, y1) = mulVector ballMoveSpeed $ mulVector p (ballVector state)
        (x2, y2) = ballLocation state
        p1x = - fromIntegral (width state) / 2 + pThickness
        p2x = fromIntegral (width state) / 2 - pThickness
                
minMaxOrVal :: (Ord a) => a -> a -> a -> a
minMaxOrVal min max val
        | max < val = max
        | min > val = min
        | otherwise = val

-- Move player by actual keystate
movePlayer :: Float -> Int -> KeyState -> KeyState -> Float
movePlayer y height up down
    | up == Down && down == Down                                    = y
    | up == Down && y+(playerWidth/2) >= (fromIntegral height)/2    = y
    | down == Down && y-(playerWidth/2) < -(fromIntegral height)/2  = y
    | up == Down                                                    = y+playerMoveStep
    | down == Down                                                  = y-playerMoveStep
    | otherwise                                                     = y


vectorSize :: Coords -> Float
vectorSize (a, b)
    | a < 0     = -sqrt(a*a + b*b)
    | a > 0     = sqrt(a*a + b*b)

rotateVect :: Coords -> Float -> Coords
rotateVect (x, y) rot = (x * cos theta - y * sin theta, x * sin theta + y * cos theta)
                        where
                            theta = rot * pi / 2

playerColision :: GameState -> Maybe Float
playerColision state
    | p1x >= x-ballRadius && y < p1y + (playerWidth/2) && y > p1y - (playerWidth/2)    = Just $ (p1y-y)/(playerWidth/2)
    | p2x <= x+ballRadius && y < p2y + (playerWidth/2) && y > p2y - (playerWidth/2)    = Just $ (p2y-y)/(playerWidth/2)
    | otherwise                                                                         = Nothing
    where
        (x, y) = ballLocation state
        p1y = p1location state
        p2y = p2location state
        p1x = - fromIntegral (width state) / 2 + pThickness
        p2x = fromIntegral (width state) / 2 - pThickness

wallCollision :: GameState -> Bool
wallCollision state
        | y >= (fromIntegral $ height state)/2 || y <= -(fromIntegral $ height state)/2  = True
        | otherwise                     = False
        where
            (_, y) = ballLocation state

renderWin :: GameState -> Picture
renderWin state
    | stopped state && p2s > p1s    = translate ((fromIntegral $ width state)/4-200) 150.0 $ Color red $ Text "WINNER"
    | stopped state && p1s > p2s    = translate (-(fromIntegral $ width state)/4-200) 150.0 $ Color red $ Text "WINNER"
    | otherwise                     = Blank
    where
        p2s = p2score state
        p1s = p1score state

setP1Location :: Float -> GameState -> GameState
setP1Location location (Game _ p2 bl bv p1s p2s p1u p1d p2u p2d s w h)
                    = Game location p2 bl bv p1s p2s p1u p1d p2u p2d s w h
                    
setP2Location :: Float -> GameState -> GameState
setP2Location location (Game p1 _ bl bv p1s p2s p1u p1d p2u p2d s w h)
                    = Game p1 location bl bv p1s p2s p1u p1d p2u p2d s w h

mulVector :: Float -> Coords -> Coords
mulVector f (x, y) = (f*x, f*y)

type Coords = (Float, Float)
-- game state data structure
data GameState = Game 
    {
        p1location :: Float,
        p2location :: Float,
        ballLocation :: Coords,
        ballVector :: Coords,
        p1score :: Int,
        p2score :: Int,
        p1UState :: KeyState,
        p1DState :: KeyState,
        p2UState :: KeyState,
        p2DState :: KeyState,
        stopped :: Bool,
        width :: Int,
        height :: Int
    }