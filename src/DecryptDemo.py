
from Crypto.Cipher import DES
from Crypto.Util.Padding import unpad
from Crypto.Util import Counter

input = 0xe3193789bbf4183a197442a1bddf3ce24ee66487f8a2339518fe243db51b0839b5785cba31914a993593a4ef38ce68c54530b81f1f068cdd786d6b3d1573994cca2f00a220112785895a27384f54b66b7a42b61d3f6f31c4e66697d9b199023a92258e13ba80e47dd229882806951de6f8dbe964507d98fd73599d840d6a8a45d0e1bf6241f236e85511f26d6369d726adac5a77fb0240cad55967d074476d579200e2f5da0feb5d3d3aa55f4d2b29d104a1b31a5da38cc47a83dd596b52c76a74b7cdce980a96282d2b333739453267
key = 0xab33b76dc988566f
nonce = 0xE005AED6


input_byte_string = input.to_bytes((input.bit_length() + 7) // 8, 'big')
key_byte_string = key.to_bytes((key.bit_length() + 7) // 8, 'big')
nonce_byte_string = nonce.to_bytes((nonce.bit_length() + 7) // 8, 'big')

ctr = Counter.new(32, prefix=nonce_byte_string, initial_value=1)
cipher = DES.new(key_byte_string, DES.MODE_CTR, counter=ctr)
decrypted = unpad(cipher.decrypt(input_byte_string), DES.block_size).decode('utf-8')


print(f'Decrypted Data:\n\n`\n{decrypted}\n`')


