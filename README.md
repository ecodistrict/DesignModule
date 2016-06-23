# DesignModule
All parts of the design module

- Client
- WS2IMB
- Publishing server
- Tiler server


## Client
This is the web client part of the design module. This module is written in html/css/javascript and uses 2 libraries: leaflet and D3.

## WS2IMB
This patr of the design module is a C# web service that connects the web client via web sockets to the IMB framework. every web socket connected from the web client connects to an event registered on the publishing server that uses a specific object to represent the client within the publishing server. Messages are all strings containing JSON. Data from the publishing server to the web client are also sent over this web socket (bi-directional). 

## Publishing server
This part of the design module handles all the server side work for the web client. The web clients talks over a web socket via IMB to this server. The publishing server internally all its actions via classes
- module (1 per project type)
- projects
- scenarios
- scenario elements
	- layers
	- charts
	- kpis
	- ..
- 