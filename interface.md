```Haskell

initRandom :: ([[String]], [[UIHelper.Player]])



selectRequest :: Int->Int->UIHelper.Player->[[String]]->[[UIHelper.Player]] -> Bool
selectRequest x y player = True
--- @Description: UI will send a request to select one grid, return whether the player can select this grid.
--- @Param: x,y : the grid point(x,y from {0,1,2,3}), player: the current player 
--- @Return: True if the player can choose the grid for further operation, False if the play can't select this grid.

{-
Data Structure [[String]]:

This is the display map(4*4) for the UI. Each content should be a String, e.g. if one chess is flipped and its content is "ant", then that position of the map should be "ant". If one chess is not flipped, then the String should be "?".
e.g.
[
    ["?", "ant", "ant", "?"],
    ["elephant", "?", "?", "?"],
    ["?", "?", "?", "?"],
    ["?", "?", "?", "?"],
]
Data Structure [[UIHelper.Player]]: 

This is also a 4*4 player map with each position indicating the chess in this position belongs to whom.
e.g.(this matches the last example)
[
    [UIHelper.Unknown, UIHelper.Red, UIHelper.Blue,UIHelper.Unknown],
    [UIHelper.Red,UIHelper.Unknown,UIHelper.Unknown,UIHelper.Unknown],
    [UIHelper.Unknown,UIHelper.Unknown,UIHelper.Unknown,UIHelper.Unknown],
    [UIHelper.Unknown,UIHelper.Unknown,UIHelper.Unknown,UIHelper.Unknown],
]

-}

sendFlipRequest :: Int->Int->UIHelper.Player->[[String]]->[[UIHelper.Player]]->(Bool,[[String]], [[UIHelper.Player]])
sendFlipRequest x y player = (True, 4*4 String Map, 4*4 Player Map)
--- @Description: UI will send a request to flip the chess in the given grid, return whether he can do this. Also return display map and player map after this action(Note: no matter this action is permitted or not, return these two. If the action is not allowed, just return current state). 
--- @Param: x,y: position, palyer: current player
--- @Return: whether the action will be allowed and the display and player map after this request. 


sendMoveRequest :: Int->Int->Int->Int->UIHelper.Player->[[String]]->[[UIHelper.Player]]->(Bool,[[String]], [[UIHelper.Player]])
sendMoveRequest x_old y_old x_new y_new player = (True, 4*4 String Map, 4*4 Player Map)
--- Note: this is for both move and eat action

gameIsOver :: (Bool,UIHelper.Player)
--- Note: return a tuple. The first elem is True(game over)/ False. Second elem is the winner(UIHelper.Red/UIHelper.Blue) if the game is over and there is a winner. Second elem will be UIHelper.Unknown either game is over but no winner(draw) or game is not over.


```


