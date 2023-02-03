



class Checkers:
    def __init__(self):
        self.board = [
            ["E","E","E","E","E","E","E","E"],
            ["E","E","E","E","E","E","E","E"],
            ["E","E","E","E","E","E","E","E"],
            ["E","E","E","E","E","E","E","E"],
            ["E","E","E","E","E","E","E","E"],
            ["E","E","E","E","E","E","E","E"],
            ["E","E","E","E","E","E","E","E"],
            ["E","E","E","E","E","E","E","E"]
        ]
        self.currentPlayer = "White"
    
    def display(self):
        print("")
        print(f"Current Player: {self.currentPlayer}")
        print("")
        for row in self.board:
            print(" ".join(row))
        print("")

    def getNextMove(self)






newGame = Checkers()


newGame.display()

