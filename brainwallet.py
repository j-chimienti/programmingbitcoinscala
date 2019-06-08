from ecc import S256Point, G, N
from random import randint
from helper import double_sha256
e = int.from_bytes(double_sha256(b'my secret'), 'big') # <1>
z = int.from_bytes(double_sha256(b'my message'), 'big') # <2>
k = randint(0, N)
r = (k*G).x.num # <3>
k_inv = pow(k, N-2, N)
s = (z+r*e) * k_inv % N # <4>
point = e*G # <5>
print(point)
# S256Point(028d003eab2e428d11983f3e97c3fa0addf3b42740df0d211795ffb3be2f6c52,0ae987b9ec6ea159c78cb2a937ed89096fb218d9e7594f02b547526d8cd309e2)
print(hex(z))
# 0x231c6f3d980a6b0fb7152f85cee7eb52bf92433d9919b9c5218cb08e79cce78
print(hex(r))
# 0x3b5847f623a77be3be544c00b8abb83540ad44c691a1e0df7f60fcedd912d311
print(hex(s))
# 0x40dbad2b4e539ffe797a6f41d414de5e38c5bd09aafe54b87a6dffe68c60f224

"""
----
<1> This would be something like a "brain wallet". Please don't use this for a real secret.
<2> This is the message that we're signing.
<3> kG = (r,y) so we take the x coordinate only
<4> s = (z+re)/k. We mod by N because we know this is a cyclical group of order N
<5> The public point needs to be known by the verifier
"""
