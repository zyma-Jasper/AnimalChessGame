# CSE230FA21PJ-Proposal
We are planning to create a two-player board game called “beast chess.” We will create a user interface using Haskell to display a chessboard and all the chess. The chessboard will be 4 times 4 checkboard. Initially, all the 16 chess(8 red chess and 8 blue chess) will be put on the chessboard upside down. Each player will be holding either the red side or the blue side. The player who holds the red side will start to flip chess. Players take turns to make a move, each round each player must make exactly one move. A move is defined as either flipping upside-down chess, or moving chess from the player’s side in one of the four directions: up, down, left, and right. While moving chess, the player can either move the chess to a blank spot (a spot on the chessboard which is not occupied by any other chess) or eat the opponent chess with a weaker beast. The value of the beast is elephant > lion > tiger > cheetah > dog > cat > rat > ant. However, both rat and ant can eat elephant. If the same beast from two sides meets, whichever initiates the attack eats the other. A weaker beast is not allowed to suicide by a walk into a stronger beast’s spot and getting taken over. We will make a timer that counts down and limits the time each player takes to make a move. We will also prompt the user to move. Our program should detect legal and illegal moves and decide who wins the game (or draw). The initial layout of all the upside-down chess should be created by the program randomly. We will make our program enforce the game logic, and easy for users to use.
