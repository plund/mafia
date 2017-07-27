#!/bin/bash

# 1. Create server.pass.key
openssl genrsa -des3 -passout pass:x -out server.pass.key 2048

# 2. Create server.key
openssl rsa -passin pass:x -in server.pass.key -out server.key


# 3. Create server.csr
openssl req -new -key server.key -out server.csr
#You are about to be asked to enter information that will be incorporated
#into your certificate request.
#What you are about to enter is what is called a Distinguished Name or a DN.
#There are quite a few fields but you can leave some blank
#For some fields there will be a default value,
#If you enter '.', the field will be left blank.
#-----
#Country Name (2 letter code) [AU]:SE
#State or Province Name (full name) [Some-State]:Stockholm
#Locality Name (eg, city) []:Stockholm
#Organization Name (eg, company) [Internet Widgits Pty Ltd]:Peter Lund
#Organizational Unit Name (eg, section) []:
#Common Name (e.g. server FQDN or YOUR name) []:mafia.peterlund.se
#Email Address []:

#Please enter the following 'extra' attributes
#to be sent with your certificate request
#A challenge password []:
#An optional company name []:


# 4. Create server.crt (10 years)
openssl x509 -req -days 3650 -in server.csr -signkey server.key -out server.crt
