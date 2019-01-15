using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Diagnostics;
using System.Linq;
using System.ServiceProcess;
using System.Text;
using System.Threading.Tasks;
using System.IO;

using System.Net.WebSockets;
using System.ServiceModel;
using System.ServiceModel.Activation;
using System.ServiceModel.Channels;
using System.ServiceModel.Description;
using System.ServiceModel.Dispatcher;
using System.Web;
using IMB;
using Newtonsoft.Json;

// see readme for setup details

// netsh http add urlacl url=http://+:80/sessions user=vps17642\cornelissenja
// netsh http add urlacl url=https://+:443/sessions user=vps17642\cornelissenja

// install as windows service: installutil.exe in dotnet folder (C:\Windows\Microsoft.NET\Framework\vx.x.xxxxx)
// installutil.exe <path>\WS2IMBSvc.exe
// this will install the dotnet assembly as a windows service

// web socket service under iis
// http://www.codemag.com/article/1210051
// https://msdn.microsoft.com/en-us/library/aa751792(v=vs.110).aspx


// root event: service
//   session event: project
//     channel event: individual client ie web socket

namespace WS2IMBSvc
{
    public partial class WS2IMBService : ServiceBase
    {
        string WebSocketUrl = Properties.Settings.Default.WebSocketUrl; 
        string IMBHub = Properties.Settings.Default.IMBHub;

        const string IMBModelName = "WS2IMB";
        const int IMBModelID = 0;

        public WS2IMBService()
        {
            InitializeComponent();
        }

        public void DoStart(string[] args)
        {
            OnStart(args);
        }

        public void DoStop()
        {
            OnStop();
        }

        protected override void OnStart(string[] args)
        {
            string logfile = Properties.Settings.Default.Log;
            TextWriterTraceListener[] listeners = new TextWriterTraceListener[] {
                new TextWriterTraceListener(logfile),
                new TextWriterTraceListener(Console.Out)
            };
            Debug.AutoFlush = true;
            Debug.Listeners.AddRange(listeners);
            Console.WriteLine("added listener for console and logging (to file " + logfile + ")");

            try
            {
                // initialize imb connection
                ServiceState.imbConnection = new TSocketConnection(IMBModelName, IMBModelID, "", IMBHub);
                ServiceState.imbConnection.onDisconnect += IMBHandleDisconnect;
                ServiceState.imbConnection.onException += IMBHandleException;


                // initialize web socket
                var baseAddresses = new List<Uri>();
                foreach (var url in WebSocketUrl.Split('|'))
                    baseAddresses.Add(new Uri(url));
                ServiceState.serviceHost = new ServiceHost(typeof(WebSocketsServer), baseAddresses.ToArray());

                // Enable metadata publishing.
                ServiceMetadataBehavior serviceMetadataBehavior = new ServiceMetadataBehavior { HttpGetEnabled = true };
                serviceMetadataBehavior.MetadataExporter.PolicyVersion = PolicyVersion.Policy15;
                ServiceState.serviceHost.Description.Behaviors.Add(serviceMetadataBehavior);

                // http://www.rauch.io/2015/06/25/all-wcf-timeouts-explained/
                // https://msdn.microsoft.com/en-us/library/hh924831(v=vs.110).aspx
                CustomBinding binding = new CustomBinding
                {
                    ReceiveTimeout = new TimeSpan(3, 0, 0),
                    SendTimeout = new TimeSpan(3, 0, 0)
                };
                binding.Elements.Add(new ByteStreamMessageEncodingBindingElement());

                HttpTransportBindingElement transport = new HttpTransportBindingElement();
                transport.WebSocketSettings.TransportUsage = WebSocketTransportUsage.Always;
                transport.WebSocketSettings.CreateNotificationOnConnection = true;
                transport.WebSocketSettings.KeepAliveInterval = new TimeSpan(3, 0, 0);
                transport.KeepAliveEnabled = true; // default true?
                binding.Elements.Add(transport);

                var endPoint = ServiceState.serviceHost.AddServiceEndpoint(typeof(IWebSocketsServer), binding, "");
                endPoint.EndpointBehaviors.Add(new ClientTrackerEndpointBehavior());

                try
                {
                    // http://www.rauch.io/2015/06/25/all-wcf-timeouts-explained/
                    // https://msdn.microsoft.com/en-us/library/hh924831(v=vs.110).aspx
                    CustomBinding bindingSSL = new CustomBinding
                    {
                        ReceiveTimeout = new TimeSpan(3, 0, 0),
                        SendTimeout = new TimeSpan(3, 0, 0)
                    };
                    bindingSSL.Elements.Add(new ByteStreamMessageEncodingBindingElement());

                    HttpsTransportBindingElement transportSSL = new HttpsTransportBindingElement();
                    transportSSL.WebSocketSettings.TransportUsage = WebSocketTransportUsage.Always;
                    transportSSL.WebSocketSettings.CreateNotificationOnConnection = true;
                    transportSSL.WebSocketSettings.KeepAliveInterval = new TimeSpan(3, 0, 0);
                    transportSSL.KeepAliveEnabled = true;
                    bindingSSL.Elements.Add(transportSSL);

                    var endPointSSL = ServiceState.serviceHost.AddServiceEndpoint(typeof(IWebSocketsServer), bindingSSL, "");
                    endPointSSL.EndpointBehaviors.Add(new ClientTrackerEndpointBehavior());
                }
                catch(Exception e)
                {
                    Debug.WriteLine(DateTime.Now + ": >> Could not bind SSL endpoint: " + e.Message);
                }

                // finally open web socket(s)
                ServiceState.serviceHost.Open();

                // done starting service
                Debug.WriteLine(DateTime.Now + ": Started WS2IMB service");
            }
            catch(Exception e)
            {
                Debug.WriteLine(DateTime.Now + ": ## Exception Starting WS2IMB service: " + e.Message);
            }
        }

        private void IMBHandleDisconnect(TConnection aConnection)
        {
            Debug.WriteLine(DateTime.Now + ": ## disconnected");
        }

        private void IMBHandleException(TConnection aConnection, Exception e)
        {
            if (e is System.IO.IOException)
            {
                // try to close imb connection and reconnect again
                Debug.WriteLine(DateTime.Now + ": >> Recovering from IO exception in IMB reader thread: " + e.Message);
                aConnection.recover();
                Debug.WriteLine(DateTime.Now + ":    Recovered");
            }
            else
                Debug.WriteLine(DateTime.Now + ": ## exception in IMB reader thread: " + e.Message);
        }

        protected override void OnStop()
        {
            try
            {
                try
                {
                    ServiceState.serviceHost.Close();
                    ((IDisposable)ServiceState.serviceHost).Dispose();
                    ServiceState.serviceHost = null;
                    Debug.WriteLine(DateTime.Now + ": Stopped WS2IMB service");
                }
                finally
                {
                    ServiceState.imbConnection.close();
                }
            }
            catch(Exception e)
            {
                Debug.WriteLine(DateTime.Now + ": ## Exception Stopping WS2IMB service: " + e.Message);
            }
        }
    }

    public class ServiceState
    {
        public const string RootEventName = "USIdle.Sessions.WS2IMB";
        private const int actionStatus = 4;

        public static ServiceHost serviceHost = null;
        public static TConnection imbConnection = null;
        public static TEventEntry rootEvent = null;
        public static Dictionary<object, TEventEntry> channelToEvent = new Dictionary<object, TEventEntry>();
        public static Dictionary<object, string> channelToClientType = new Dictionary<object, string>();
        public static Dictionary<object, string> channelToRemoteAddress = new Dictionary<object, string>();
        
        public static void HookupRoot()
        {
            if (rootEvent == null)
            {
                rootEvent = imbConnection.subscribe(RootEventName, false);
                rootEvent.onIntString += RootEvent_onIntString;
            }
        }

        private static void RootEvent_onIntString(TEventEntry aEventEntry, int aInt, string aString)
        {
            try
            {
                if (aInt == TEventEntry.actionInquire)
                {
                    var returnEvent = aString != "" ? imbConnection.publish(aString, false) : aEventEntry;
                    Debug.WriteLine(DateTime.Now + ": received inquire on root " + aEventEntry.eventName + " (" + aString + "), return on "+returnEvent.eventName);
                    try
                    {
                        foreach (var ctep in channelToEvent)
                        {
                            string channelEventNamePostfix;
                            try
                            {
                                var clientType = ServiceState.channelToClientType[ctep.Key];
                                channelEventNamePostfix = (clientType != null) && (clientType.Length > 0) ? "&" + clientType : "";
                            }
                            catch
                            {
                                channelEventNamePostfix = "";
                            }
                            returnEvent.signalIntString(TEventEntry.actionNew, ctep.Value.eventName + channelEventNamePostfix); // answer inquire on root with all event names of channels ie clients
                        }
                    }
                    finally
                    {
                        if (returnEvent != aEventEntry)
                            returnEvent.unPublish();
                    }
                }
                else if (aInt == actionStatus)
                {

                    // build status to return
                    var status = "{\"id\":\"" + imbConnection.modelName + " @ " + serviceHost.BaseAddresses[0].AbsoluteUri + "\",\"status\":\"" + serviceHost.State.ToString() + "\",\"info\":\"" + channelToEvent.Count.ToString() + " channels\"}";
                    // signal status
                    var returnEvent = aString != "" ? imbConnection.publish(aString, false) : aEventEntry;
                    Debug.WriteLine(DateTime.Now + ": received status request on root " + aEventEntry.eventName + " (" + aString + "), return on " + returnEvent.eventName);
                    try
                    {
                        returnEvent.signalString(status);
                    }
                    finally
                    {
                        if (returnEvent != aEventEntry)
                            returnEvent.unPublish();
                    }
                }
            }
            catch (Exception e)
            {
                Debug.WriteLine(DateTime.Now + ": ## Exception in RootEvent_onIntString (" + aInt.ToString()+", "+aString+"): " + e.Message);
            }
        }

        public static bool CheckForLastChannel(string aSessionEventNamePrefix)
        {
            foreach (var ctep in channelToEvent)
                // check if channel belongs to session
                if (ctep.Value.eventName.StartsWith(aSessionEventNamePrefix, StringComparison.InvariantCultureIgnoreCase))
                    return false;
            return true;
        }
    }

    public class ClientTrackerChannelInitializer : IChannelInitializer
    {
        public void Initialize(IClientChannel channel)
        {
            Debug.WriteLine(DateTime.Now + ": Client connect: " + channel.SessionId.ToString());
            // link handlers
            channel.Closed += ClientDisconnected;
            channel.Faulted += ClientDisconnected;
        }

        static void ClientDisconnected(object sender, EventArgs e)
        {
            Debug.WriteLine(DateTime.Now + ": Client disconnect: " + ((IClientChannel)sender).SessionId.ToString());
            // cleanup event
            if (ServiceState.channelToEvent.TryGetValue(sender, out TEventEntry channelEvent))
            {
                // decode session part of event name
                var sessionEventName = channelEvent.eventName.Substring(0, channelEvent.eventName.LastIndexOf('.'));
                var connection = channelEvent.connection;
                // signal client is removed
                channelEvent.signalIntString(TEventEntry.actionDelete, ((IClientChannel)sender).SessionId); // channel, action delete (optional session id)
                // remove link between channel and event
                channelEvent.unPublish();
                channelEvent.unSubscribe();
                channelEvent.Tag = null;
                ServiceState.channelToEvent.Remove(sender);

                lock (ServiceState.rootEvent)
                {
                    // check if last channel on session then session can be discarded
                    if (ServiceState.CheckForLastChannel(sessionEventName + "."))
                    {
                        var sessionEvent = connection.publish(sessionEventName, false); // already published so is just a lookup
                        // remove handler
                        sessionEvent.onIntString -= SessionEvent_onIntString;
                        // unlink
                        sessionEvent.unPublish();
                        sessionEvent.unSubscribe();
                        Debug.WriteLine("   Last client on session " + sessionEventName);
                    }
                }
            }
        }

        public static void SessionEvent_onIntString(TEventEntry aEventEntry, int aInt, string aString)
        {
            if (aInt == TEventEntry.actionInquire)
            {
                // return all clients on session
                var returnEvent = aString != "" ? aEventEntry.connection.publish(aString, false) : aEventEntry;
                Debug.WriteLine(DateTime.Now + ": received inquire on " + aEventEntry.eventName + " (" + aString + "), return on "+returnEvent.eventName);
                try
                {
                    var eventNameFilter = aEventEntry.eventName + ".";
                    foreach (var ctep in ServiceState.channelToEvent)
                        // check if channel belongs to session
                        if (ctep.Value.eventName.StartsWith(eventNameFilter))
                        {
                            string channelEventNamePostfix;
                            try
                            {
                                var clientType = ServiceState.channelToClientType[ctep.Key];
                                channelEventNamePostfix = (clientType != null) && (clientType.Length > 0) ? "&" + clientType : "";
                            }
                            catch
                            {
                                channelEventNamePostfix = "";
                            }
                            returnEvent.signalIntString(TEventEntry.actionNew, ctep.Value.eventName + channelEventNamePostfix); // event names of clients on session
                        }
                }
                finally
                {
                    if (returnEvent != aEventEntry)
                        returnEvent.unPublish();
                }
            }
        }


    }

    class ClientTrackerEndpointBehavior : IEndpointBehavior
    {
        public void AddBindingParameters(ServiceEndpoint endpoint, BindingParameterCollection bindingParameters) { }
        public void ApplyClientBehavior(ServiceEndpoint endpoint, ClientRuntime clientRuntime) { }
        public void Validate(ServiceEndpoint endpoint) { }

        public void ApplyDispatchBehavior(ServiceEndpoint endpoint, EndpointDispatcher endpointDispatcher)
        {
            endpointDispatcher.ChannelDispatcher.ChannelInitializers.Add(new ClientTrackerChannelInitializer());
        }
    }

    [ServiceContract(CallbackContract = typeof(IMessageToClient))]
    public interface IWebSocketsServer
    {
        [OperationContract(IsOneWay = true, Action = "*")]
        void SendMessageToServer(Message msg);
    }

    [ServiceContract]
    interface IMessageToClient
    {
        [OperationContract(IsOneWay = true, Action = "*")]
        void SendMessageToClient(Message msg);
    }

    public class WebSocketsServer : IWebSocketsServer
    {
        public void SendMessageToServer(Message msg)
        {
            try
            {
                var callback = OperationContext.Current.GetCallbackChannel<IMessageToClient>();
                var channel = OperationContext.Current.Channel;

                if (!ServiceState.channelToEvent.TryGetValue(channel, out TEventEntry channelEvent))
                {
                    var queryParameters = HttpUtility.ParseQueryString((OperationContext.Current.IncomingMessageProperties["WebSocketMessageProperty"] as WebSocketMessageProperty).WebSocketContext.RequestUri.Query);
                    var sessionName = queryParameters["session"];
                    string clientType = "";
                    try { clientType = queryParameters["clienttype"]; } catch { }


                    if (sessionName == null || sessionName == "") sessionName = "unknown";
                    //var remoteAddress = OperationContext.Current.IncomingMessageHeaders.From;
                    var repmp = OperationContext.Current.IncomingMessageProperties[RemoteEndpointMessageProperty.Name] as RemoteEndpointMessageProperty;
                    var remoteAddress = repmp.Address + ":" + repmp.Port.ToString();

                    // signal start of client on session
                    Debug.WriteLine(DateTime.Now + ": Start of client " + remoteAddress + " on session: " + sessionName + " (" + channel.SessionId.ToString() + ", " + clientType + ")");
                    try
                    {
                        ServiceState.channelToClientType[channel] = clientType;
                        ServiceState.channelToRemoteAddress[channel] = remoteAddress;
                        var sessionEvent = ServiceState.imbConnection.publish(ServiceState.RootEventName + "." + sessionName, false);
                        // make sure we have a root event
                        ServiceState.HookupRoot();
                        // check if first channel on session
                        lock (ServiceState.rootEvent)
                        {
                            if (!sessionEvent.isSubscribed)
                            {
                                // yes, is the first channel on session
                                Debug.WriteLine("   First client on session " + sessionEvent.eventName);
                                sessionEvent.subscribe();
                                // make client list queryable by session module
                                sessionEvent.onIntString += ClientTrackerChannelInitializer.SessionEvent_onIntString;
                            }
                        }
                        // setup channel
                        channelEvent = ServiceState.imbConnection.subscribe(sessionEvent.eventName + "." + channel.SessionId, false);
                        ServiceState.channelToEvent[channel] = channelEvent;

                        channelEvent.Tag = callback;
                        channelEvent.onString += ChannelEvent_onString;
                        channelEvent.onStreamCreate += ChannelEvent_onStreamCreate;
                        channelEvent.onStreamEnd += ChannelEvent_onStreamEnd;
                        // make client event manageable by session module
                        channelEvent.onIntString += ChannelEvent_onIntString;
                        // new channel with event name on session event
                        string channelEventNamePostfix;
                        try
                        {
                            channelEventNamePostfix = (clientType != null) && (clientType.Length > 0) ? "&" + clientType : "";
                        }
                        catch
                        {
                            channelEventNamePostfix = "";
                        }

                        sessionEvent.signalIntString(TEventEntry.actionNew, channelEvent.eventName + channelEventNamePostfix);
                        // todo: signal new client on root event?
                    }
                    catch (Exception e)
                    {
                        Debug.WriteLine(DateTime.Now + ": ## exception registering channel: " + e.Message);
                    }
                }
                if (!msg.IsEmpty && ((IChannel)callback).State == CommunicationState.Opened)
                {
                    byte[] returnMessage = msg.GetBody<byte[]>();
                    if (returnMessage.Length > TConnection.imbMaximumPayloadSize / 10) // switch to stream for very large messages
                        channelEvent.signalStream("string", new MemoryStream(returnMessage));
                    else
                        channelEvent.signalString(returnMessage);
                }
            }
            catch (Exception e)
            {
                Debug.WriteLine(DateTime.Now + ": ## unhandled exception in WebSocketsServer.SendMessageToServer: " + e.Message);
            }
        }

        private void ChannelEvent_onIntString(TEventEntry aEventEntry, int aInt, string aString)
        {
            if (aInt == TEventEntry.actionDelete)
            {
                // handle disconnect of session server side..
                // NOT: just unpublish to free up event, but keep on listening because web part is still up
                // NOT: aEventEntry.unPublish();
                // signal web client session server is disconnected
                if (aEventEntry.Tag != null)
                    (aEventEntry.Tag as IMessageToClient).SendMessageToClient(CreateMessage(
                        //"{\"type\":\"connection\",\"payload\":{\"message\":\"session server has disconnected\"}}"
                        JsonConvert.SerializeObject(new {
                            type = "connection",
                            payload = new { message = "session server has disconnected" }
                        })
                    ));
            }
        }

        private void ChannelEvent_onString(TEventEntry aEventEntry, string aString)
        {
            if (aEventEntry.Tag != null)
                (aEventEntry.Tag as IMessageToClient).SendMessageToClient(CreateMessage(aString));
        }

        private Stream ChannelEvent_onStreamCreate(TEventEntry aEventEntry, string aName)
        {
            return new MemoryStream();
        }

        private void ChannelEvent_onStreamEnd(TEventEntry aEventEntry, string aName, Stream stream, bool aCancel)
        {
            if (aEventEntry.Tag != null)
                (aEventEntry.Tag as IMessageToClient).SendMessageToClient(CreateMessage((stream as MemoryStream).ToArray()));
        }

        public Message CreateMessage(string msgText)
        {
            Message msg = ByteStreamMessage.CreateMessage(new ArraySegment<byte>(Encoding.UTF8.GetBytes(msgText)));
            msg.Properties["WebSocketMessageProperty"] = new WebSocketMessageProperty { MessageType = WebSocketMessageType.Text };
            return msg;
        }

        public Message CreateMessage(byte[] msgBytes)
        {
            Message msg = ByteStreamMessage.CreateMessage(new ArraySegment<byte>(msgBytes));
            msg.Properties["WebSocketMessageProperty"] = new WebSocketMessageProperty { MessageType = WebSocketMessageType.Text };
            return msg;
        }
    }
}
