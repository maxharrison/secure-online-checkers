import openai
import requests
import string
import random
import time

text1 = """
i am playing a game of checkers and need a move suggestion. the current board state is as follows.
Keep in mind the rules of checker, only kings (captial letters) can move backwards,
and unless you are jumping you can only move one peice:
"""


text2 = """
I am playing as the white pieces. please provide the next move using coordinates, like (row column) , (row column), eg "9X , 9Z".
"""

def askGPT(string):

  openai.api_key = "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"
  response = openai.Completion.create(
    engine = "text-davinci-003",
    prompt = string,
    temperature = 0.6,
    max_tokens = 25
    )

  return response.choices[0].text

def getAIRoute(data):
  loop = True
  counter = 0
  newtext = text1 + data + text2

  while loop:

    counter = counter + 1

    if counter == 5:
      newtext = text1 + data + text2
      counter = 0

    result = askGPT(newtext)

    print(result)
    loop = False

    #if input() == "y":
    #  loop = False
    #else:
    #  newtext = newtext + " " + result + "\nInvalid move, try again:\n"
    #  print(newtext)

  return result





class Checkers:
  def __init__(self):

    # Start the game
    self.start_game()

    self.board = ""

    # Board
    self.update_board()

    self.loop()

  def loop(self):
    while True:
      time.sleep(0.5)
      self.update_board()
      if "White" in self.data:
        self.route = getAIRoute(self.data)
        self.send_route()
        
        



  def start_game(self):
    self.id = ''.join(random.choice(string.ascii_lowercase) for i in range(8))
    r = requests.post('http://localhost:3000/binary', data=f'start {self.id} True')
    if r.status_code != 200:
      print('Failed to start game')
      exit()





  def send_route(self):
    #curl -X POST -H "Content-Type: application/octet-stream" --data-binary $'move b6,a5' localhost:3000/binary
    data = f'move {self.id} '
    for square in self.route:
      x = chr(square[1] + 64)
      y = 8 - square[0]
      data += f"{x}{y}"
      if square != self.route[-1]:
        data += ','
    print(data)
    r = requests.post('http://localhost:3000/binary', data=data)


     


  def download_board(self):
    try:
      r = requests.post('http://localhost:3000/binary', data=f'poll {self.id}')
      plaintext = self.decryptDES(r.text)
      board = self.parse_board(plaintext)
      return board
    except requests.exceptions.ConnectionError:
      print('Error: could not connect to server')
      return self.board

  def parse_board(self, board_str):
    lines = board_str.split('\n')
    board = [row.split(' ')[1:-1] for row in lines[2:10]]
    return board

  def update_board(self):
    board = self.download_board()
    self.board = board if board != self.board else self.board

  def decryptDES(self, data : str):
    from Crypto.Cipher import DES
    from Crypto.Util.Padding import unpad
    from Crypto.Util import Counter
    ciphertext = int(data, 2)
    bytes = ciphertext.to_bytes((ciphertext.bit_length() + 7) // 8, 'big')
    key = b'iwrsnfhl'
    ctr = Counter.new(64, prefix=b'', initial_value=1)
    cipher = DES.new(key, DES.MODE_CTR, counter=ctr)
    decrypted = unpad(cipher.decrypt(bytes), DES.block_size)
    self.data = decrypted.decode('utf-8')
    return self.data


    

checkers = Checkers()




