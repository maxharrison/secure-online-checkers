from Crypto.Cipher import DES
from Crypto.Random import get_random_bytes
import time

# DES key must be exactly 8 bytes long
key = get_random_bytes(8)
des_cipher = DES.new(key, DES.MODE_ECB)

plaintext = b'abcdefgh'  # DES operates on 8-byte blocks
assert len(plaintext) == 8

start_time = time.perf_counter()

ciphertext = des_cipher.encrypt(plaintext)
print(ciphertext)

end_time = time.perf_counter()
time_taken = end_time - start_time

print(f"Encrypted DES block: {ciphertext.hex()}")
print(f"Time taken to encrypt one block of DES: {time_taken:.10f} seconds")
