from cmd:

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


<port> is normally 80
<portssl> is normally 443

change ports in WS2IMBSvc.exe.config for the service to use given ports

hostname
>result is <host> used below

whoami
>result is <username> used below

from cmd with admin rights:

netsh http add iplisten ipaddress=0.0.0.0:<port>
netsh http add iplisten ipaddress=0.0.0.0:<portssl>

netsh http add urlacl url=http://+:<port>/sessions user=<host>\<username>
netsh http add urlacl url=http://+:<portssl>/sessions user=<host>\<username>

netsh advfirewall firewall add rule name="WS2IMB" dir=in action=allow protocol=TCP localport=<port>
netsh advfirewall firewall add rule name="WS2IMB" dir=in action=allow protocol=TCP localport=<portssl>


install as windows service: installutil.exe in dotnet folder (C:\Windows\Microsoft.NET\Framework\vx.x.xxxxx)
installutil.exe <path>\WS2IMBSvc.exe
