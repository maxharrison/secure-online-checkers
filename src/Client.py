import pygame
import requests
import string
import random
from Crypto.Cipher import DES
from Crypto.Util.Padding import unpad
from Crypto.Util import Counter
from cryptography.hazmat.primitives.asymmetric import ec
from cryptography.hazmat.primitives import serialization
import time
import threading
import secrets



class Checkers:
  def __init__(self):
    pygame.init()

    print("What is the ip address of the server? ")
    self.ip = input()
    
    # sizes
    self.board_height = 800
    self.board_width = 720
    self.square_size = 80
    self.num_squares = 8
    self.radius = self.square_size//3
    self.hovered_square = None


    # Colours
    self.white_board_colour = (156,118,96,255)
    self.green_board_colour = (6,101,75,255)
    self.white_piece_colour = (173,157,150,255)
    self.red_piece_colour = (87,8,9,255)
    self.black_colour = (0,0,0,255)

    # Start the game
    self.winner = "P"
    self.play = True
    self.ai = True
    self.player = ""
    self.nonce = b''
    self.key = self.start_game()


    # Game window
    self.game_display = pygame.display.set_mode((self.board_width, self.board_height))
    pygame.display.set_caption('Checkers')

    # Board
    self.route = []
    self.board = [['-' for i in range(8)] for j in range(8)]
    self.update_board()
    self.draw_board()

    update_thread = threading.Thread(target=self.update_board_loop)
    update_thread.start()

    # Game loop
    self.loop()

    
  def update_board_loop(self):
    while self.play:
      time.sleep(1)  # Adjust the sleep time to control the update frequency
      self.update_board()
      if self.winner != "P":
        self.play = False
        if self.winner == "W":
          print("White Wins")
        elif self.winner == "B":
          print("Black Wins")
        elif self.winner == "D":
          print("Draw")


  def derive_64_bit_key(self, shared_secret):
    # Truncate the hash to 64 bits (8 bytes)
    key_bytes = shared_secret[-8:]

    return key_bytes
    

  def start_game(self):
    if input("Would you like to play against an AI? (y/n)" ) == 'y':
      self.ai = True
    else:
      self.ai = False


    private_key = ec.generate_private_key(ec.SECP256K1())
    public_key = private_key.public_key()

    public_key_bytes = public_key.public_bytes(encoding=serialization.Encoding.X962, format=serialization.PublicFormat.UncompressedPoint)
    public_key_string = str(int.from_bytes(public_key_bytes, byteorder='big'))

    self.id = ''.join(random.choice(string.ascii_lowercase) for i in range(8))
    response = requests.post(f'http://{self.ip}:3000/binary', data=f'start {self.id} {self.ai} {public_key_string}')
    if response.status_code != 200:
      print('Failed to start game')
      exit()

    other_public_key = self.bytes_to_public_key(response.content)

    shared_secret = private_key.exchange(ec.ECDH(), other_public_key)

    key = self.derive_64_bit_key(shared_secret)

    return key

  def bytes_to_public_key(self, public_key_bytes):
    header = public_key_bytes[0]
    if header != 4:
      print(public_key_bytes)
      raise ValueError("Invalid public key format")
    x = int.from_bytes(public_key_bytes[1:33], "big")
    y = int.from_bytes(public_key_bytes[33:], "big")
    numbers = ec.EllipticCurvePublicNumbers(x, y, ec.SECP256K1())
    return numbers.public_key()


  def loop(self):
    while self.play:
      pygame.time.delay(25)
      for event in pygame.event.get():
        match event.type:
          case pygame.QUIT:
            self.play = False

          # Hover
          case pygame.MOUSEMOTION:
            (hx, hy) = self.get_square(event.pos)
            self.hovered_square = (hx, hy-1)

          # Mouse click
          case pygame.MOUSEBUTTONDOWN:
            if event.button == 1: # Left button
              square = self.get_square(event.pos)
              if square[0] in range(8) and square[1] in [1,2,3,4,5,6,7,8]:
                self.route.append(square)
              elif square == (9,0):
                # red button
                self.route = []
              elif square == (9,1):
                # green button
                self.send_route()
                self.route = []

      # Redraw the board
      self.draw_board()
      pygame.display.update()

    pygame.quit()

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

  def route_string(self):
    route = ""
    for square in self.route:
      x = chr(square[1] + 64)
      y = 8 - square[0]
      route += f"({x},{y}) "
    return route
          
  def draw_board(self):
    pygame.display.set_caption(f'Checkers - {self.player}')
    self.game_display.fill(self.black_colour)
    for row in range(self.num_squares):
      for column in range(self.num_squares):
        # Draw squares
        x = (column * self.square_size) + self.square_size
        y = row * self.square_size
        square_colour = self.white_board_colour if (row + column) % 2 == 0 else self.green_board_colour

        # Modify the color slightly if this is the hovered square
        if self.hovered_square == (row, column):
          square_colour = tuple(min(255, c + 30) for c in square_colour)

        if (row, column+1) in self.route:
          square_colour = tuple(min(255, c + 30) for c in square_colour)

        pygame.draw.rect(self.game_display, square_colour, [x, y, self.square_size, self.square_size])

        # Draw pieces
        centre = (x+self.square_size/2,y+self.square_size/2)
        match self.board[row][column]:
          case 'w':
            pygame.draw.circle(self.game_display, self.white_piece_colour, centre, self.radius)
          case 'W':
            pygame.draw.circle(self.game_display, self.white_piece_colour, centre, self.radius)
            self.draw_text('K', self.black_colour, 45, x, y+3)
          case 'b':
            pygame.draw.circle(self.game_display, self.red_piece_colour, centre, self.radius)
          case 'B':
            pygame.draw.circle(self.game_display, self.red_piece_colour, centre, self.radius)
            self.draw_text('K', self.black_colour, 45, x, y+3)


    for i in range(self.num_squares):
      # Draw numbers
      numbers = [str(i+1) for i in range(8)][::-1]
      x1 = 0
      y1 = i * self.square_size
      pygame.draw.rect(self.game_display, self.white_piece_colour, [x1, y1, self.square_size, self.square_size])
      self.draw_text(numbers[i], self.black_colour, 48, x1, y1)
      
      # Draw letters
      letters = [chr(i) for i in range(65, 73)]
      x2 = (i * self.square_size) + self.square_size
      y2 = self.num_squares * self.square_size
      pygame.draw.rect(self.game_display, self.white_piece_colour, [x2, y2, self.square_size, self.square_size])
      self.draw_text(letters[i], self.black_colour, 48, x2, y2)

    # Draw buttons
    x = 0
    y = 9 * self.square_size
    pygame.draw.rect(self.game_display, self.red_piece_colour, [x, y, self.square_size, self.square_size])
    x = self.square_size
    y = 9 * self.square_size
    pygame.draw.rect(self.game_display, self.green_board_colour, [x, y, self.square_size, self.square_size])
    
    # Draw route
    x = 5 * self.square_size
    y = 9 * self.square_size
    self.draw_text(self.route_string(), self.white_piece_colour, 32, x, y)

  def draw_text(self, text, colour, size, x, y):
    font = pygame.font.Font(None, size)
    text_surface = font.render(text, True, colour)
    text_rect = text_surface.get_rect()
    text_rect.center = (x + self.square_size // 2, y + self.square_size // 2)
    self.game_display.blit(text_surface, text_rect)

  def get_square(self, mouse_pos):
    x, y = mouse_pos
    row = y // self.square_size
    column = x // self.square_size
    return row, column

  def download_board(self):
    self.nonce = secrets.token_bytes(4)
    nonceNumber = int.from_bytes(self.nonce, 'big')
    try:
      r = requests.post('http://localhost:3000/binary', data=f'poll {self.id} {nonceNumber}', headers={'Content-Type': 'text/plain'})
      plaintext = self.decryptDES(r.content)
      board = self.parse_board(plaintext)
      return board
      
    except requests.exceptions.ConnectionError:
      print('Error: could not connect to server')
      return self.board

  def parse_board(self, board_str):
    self.player = board_str[:5]
    self.winner = board_str[6:7]
    lines = board_str.split('\n')
    board = [row.split(' ')[1:-1] for row in lines[2:10]]
    return board

  def update_board(self):
    board = self.download_board()
    self.board = board if board != self.board else self.board

  def decryptDES(self, data: bytes):
    ctr = Counter.new(32, prefix=self.nonce, initial_value=1)
    cipher = DES.new(self.key, DES.MODE_CTR, counter=ctr)
    decrypted = unpad(cipher.decrypt(data), DES.block_size)
    return decrypted.decode('utf-8')
  
  def gameFinished(self):
    wp = sum(item == 'w' for row in self.board for item in row)
    wk = sum(item == 'W' for row in self.board for item in row)
    w = wp + wk
    bp = sum(item == 'b' for row in self.board for item in row)
    bk = sum(item == 'B' for row in self.board for item in row)
    b = bp + bk
    if w > b:
      print("White Wins!")
    elif b > w:
      print("Black Wins!")
    else:
      print("Game over!")


checkers = Checkers()