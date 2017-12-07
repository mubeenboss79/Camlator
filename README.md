# Camlator
Final project repo for CS 3110 


Dependencies to run Camlator: 
Node 
Lablgtk 
Lwt
CoHttp 

How to Run Camlator: 

To start Camlator you must first configure the IP address you will be using in the source code by going to line 90 of chat_server.ml and changing the variable ip_addr to your IP address. Similarly, you have to do the same in the chat client.ml in line 42. 

After configuring the IP address in chat_server.ml and chatclient.ml, then you are all set to run Camlator. Just cd down to /…/Camlator/src and run make. Once you run make, make sure to start up the server first by running ./chat_server.byte. Once that is all set up, you can go to a new terminal session and cd into the same directory (/…/Camlator/src) and you can run the chatbox GUI by running ./chatbox.byte in the terminal. 

Make sure the other computer is also connected to the same wifi, and you then you can run the same steps on that computer, except only ONE person has to run the server, the other can just run ./chatbox.byte (after following previous instructions of course). 
