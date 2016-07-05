from cmd:

whoami
>result is username used below

ipconfig
>use ip address in client.html (VM 192.168.179.131)

from cmd with admin rights:

netsh http add iplisten ipaddress=0.0.0.0:8081

netsh http add urlacl url=http://+:8081/hello user=desktop-o2sh33d\hans
rem netsh http add urlacl url=http://+:8081/hello user=duopenotti\johan

netsh advfirewall firewall add rule name="WS2IMB" dir=in action=allow protocol=TCP localport=8081

enable Internet Information Service
	Web Management Tools
 		IIS Management Console
	World Wide Web Service
		Common HTTP Features
			Static Content
			Default Document
		Application Development Features
			ISAPI Extensions
			ISAPI Filters
			WebSocket Protocol
