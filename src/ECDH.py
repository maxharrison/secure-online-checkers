from cryptography.hazmat.primitives.asymmetric import ec
from cryptography.hazmat.primitives import serialization
from cryptography.hazmat.primitives.serialization import load_der_public_key

import binascii

#04A8C641E1E378C9EB5C16895596B7C6634237AE06B8E2D8743041586E6A41DB9BB31B1412F6B814D53B42EB833E5B5AC8D374720D32590F75120727E7CAB3371C




def intToPublicKey(n):
    bytes = n.to_bytes((n.bit_length() + 7) // 8, 'big')
    x = int.from_bytes(bytes[1:33], "big")
    y = int.from_bytes(bytes[33:], "big")
    numbers = ec.EllipticCurvePublicNumbers(x, y, ec.SECP256K1())
    return numbers.public_key()

#private_key = ec.generate_private_key(ec.SECP256K1())
private_key = ec.derive_private_key(
    13702161781411822958966984823720940545953019644258813046364985774814650359813,
    ec.SECP256K1())
print("Private key: ", hex(private_key.private_numbers().private_value))


public_key = private_key.public_key()
public_key_bytes = public_key.public_bytes(encoding=serialization.Encoding.X962, format=serialization.PublicFormat.UncompressedPoint)
public_key_hex = binascii.hexlify(public_key_bytes).decode('ascii')
print("Public key: ", public_key_hex)
 
other_public_key = intToPublicKey(0x04A8C641E1E378C9EB5C16895596B7C6634237AE06B8E2D8743041586E6A41DB9BB31B1412F6B814D53B42EB833E5B5AC8D374720D32590F75120727E7CAB3371C)

shared_secret = private_key.exchange(ec.ECDH(), other_public_key)

print(f'shared secret: {int.from_bytes(shared_secret, "big")}')



