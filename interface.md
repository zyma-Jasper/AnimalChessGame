```Haskell

isGameOver :: Game ->(Bool, Game)
--- @Description:: Judge whehter the game is over. Return whether it is over and the new game state.

initRandomMap :: Bool -> Game
--- @Param: Bool: whether init the game with demo map.
---- @Description: Init the random chess map. 

selectRequest :: Int->Int->Game->Bool
--- @Description: UI will send a request to select one grid, return whether the player can select this grid.
--- @Param: Int Int : the grid point(x,y from {0,1,2,3})
--- @Param: Game: Current game state
--- @Return: True if the player can choose the grid for further operation, False if the play can't select this grid.

sendFlipRequest :: Int->Int->Game->(Bool,Game)
--- @Description: UI will send a request to flip the chess in the given grid, return whether the user can do this. 
                  Also return new game state after this action. If the action is not allowed, just return current 
                  game state. 
--- @Param: Int Int : the grid point(from {0,1,2,3})
--- @Param: Game: Current game state
--- @Return: whether the flip action will be allowed and the new game state.

sendMoveRequest :: (Int,Int)->(Int,Int)->Game->(Bool,Game)
--- @Description: just similar with the flip request. 
--- @Param: The first Int pair will be source point and the second Int pair is the destination point.
```


