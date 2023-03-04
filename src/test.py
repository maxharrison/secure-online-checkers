from Crypto.Cipher import DES
from Crypto.Util.Padding import unpad
from Crypto.Util import Counter

key = b'secretke'
ctr = Counter.new(64, prefix=b'', initial_value=0)
cipher = DES.new(key, DES.MODE_CTR, counter=ctr)
decrypted = unpad(cipher.decrypt(data), DES.block_size)

# encrypt the plaintext
plaintext = b'abcdefg'
paddedPlaintext = pad(plaintext, DES.block_size)
ciphertext = cipher.encrypt(pad(plaintext, DES.block_size))


newciphercode = 0b1011100010011101010000100010101000110001101011101000010111111010
bytes_value = newciphercode.to_bytes((newciphercode.bit_length() + 7) // 8, 'big')

ctr = Counter.new(64, prefix=b'', initial_value=0)
cipher = DES.new(key, DES.MODE_CTR, counter=ctr)
decrypted = unpad(cipher.decrypt(bytes_value), DES.block_size)

def bytes_to_binary_string(bytes):
    return ''.join('{:08b}'.format(byte) for byte in bytes)

print('Plaintext (binary) : ', bytes_to_binary_string(plaintext))
print('Ciphertext (binary): ', bytes_to_binary_string(ciphertext))
print('Decrypted (binary) : ', bytes_to_binary_string(decrypted))




