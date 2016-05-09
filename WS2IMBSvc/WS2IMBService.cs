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

// netsh http add iplisten ipaddress=0.0.0.0:4052

// netsh http add urlacl url=http://+:4502/sessions user=vps17642\cornelissenja
// netsh http add urlacl url=http://+:4502/sessions user=duopenotti\johan

// netsh http add urlacl url=http://+:4502/sessions user="NT AUTHORITY\NETWORK SERVICE" ?? -> no, has to run under local system


namespace WS2IMBSvc
{
    public partial class WS2IMBService : ServiceBase
    {
        //const string WebSocketUrl = "http://vps17642.public.cloudvps.com:4502/sessions";
        const string WebSocketUrl = "http://localhost:4502/sessions";
        const string IMBHub = "vps17642.public.cloudvps.com";

        const string IMBModelName = "WS2IMB";
        const int IMBModelID = 0;

        public WS2IMBService()
        {
            InitializeComponent();
        }

        protected override void OnStart(string[] args)
        {
            //Lookups.eventLog = eventLog;
            Lookups.connection = new TSocketConnection(IMBModelName, IMBModelID, "", IMBHub);
            Uri baseAddress = new Uri(WebSocketUrl);
            Lookups.host = new ServiceHost(typeof(WebSocketsServer), baseAddress);

            // Enable metadata publishing.
            ServiceMetadataBehavior smb = new ServiceMetadataBehavior();
            smb.HttpGetEnabled = true;
            smb.MetadataExporter.PolicyVersion = PolicyVersion.Policy15;
            Lookups.host.Description.Behaviors.Add(smb);

            CustomBinding binding = new CustomBinding();
            binding.Elements.Add(new ByteStreamMessageEncodingBindingElement());
            HttpTransportBindingElement transport = new HttpTransportBindingElement();
            transport.WebSocketSettings.TransportUsage = WebSocketTransportUsage.Always;
            transport.WebSocketSettings.CreateNotificationOnConnection = true;
            binding.Elements.Add(transport);

            var endPoint = Lookups.host.AddServiceEndpoint(typeof(IWebSocketsServer), binding, "");
            endPoint.EndpointBehaviors.Add(new ClientTrackerEndpointBehavior());

            Lookups.host.Open();
            //var dispatcher = Lookups.host.ChannelDispatchers[0] as ChannelDispatcher;
            //eventLog.WriteEntry("Started WS2IMB service");
        }

        protected override void OnStop()
        {
            Lookups.host.Close();
            ((IDisposable)Lookups.host).Dispose();
            Lookups.host = null;
            Lookups.connection.close();
            //eventLog.WriteEntry("Stopped WS2IMB service");
        }
    }

    public class Lookups
    {
        public static ServiceHost host = null;
        //public static System.Diagnostics.EventLog eventLog = null;
        public static string RootEventName = "USIdle.Sessions.WS2IMB";
        public static TConnection connection = null;
        public static Dictionary<object, TEventEntry> channelToEvent = new Dictionary<object, TEventEntry>();
        public static TEventEntry rootEvent = null;

        public static void HookupRoot()
        {
            if (rootEvent == null)
            {
                rootEvent = connection.subscribe(RootEventName, false);
                rootEvent.onIntString += RootEvent_onIntString;
            }
        }

        private static void RootEvent_onIntString(TEventEntry aEventEntry, int aInt, string aString)
        {
            if (aInt == TEventEntry.actionInquire)
            {
                var returnEvent = aString != "" ? connection.publish(aString, false) : aEventEntry;
                try
                {
                    foreach (var ctep in channelToEvent)
                        returnEvent.signalIntString(TEventEntry.actionNew, ctep.Value.eventName);
                }
                finally
                {
                    if (aString != "")
                        aEventEntry.unPublish();
                }
            }
        }
    }

    public class ClientTrackerChannelInitializer : IChannelInitializer
    {
        public void Initialize(IClientChannel channel)
        {
            var sessionID = channel.SessionId;
            //Console.WriteLine("Client connect: {0}", sessionID);
            //Lookups.eventLog.WriteEntry("WS2IMB", "Client connect: " + sessionID.ToString());
            channel.Closed += ClientDisconnected;
            channel.Faulted += ClientDisconnected;
        }

        static void ClientDisconnected(object sender, EventArgs e)
        {
            //Console.WriteLine("Client disconnect: {0}", ((IClientChannel)sender).SessionId);
            //Lookups.eventLog.WriteEntry("WS2IMB", "Client disconnect: " + ((IClientChannel)sender).SessionId.ToString());
            // cleanup event
            TEventEntry channelEvent;
            if (Lookups.channelToEvent.TryGetValue(sender, out channelEvent))
            {
                // signal client is removed
                channelEvent.signalIntString(TEventEntry.actionDelete, ((IClientChannel)sender).SessionId);
                // remove link between channel and event
                channelEvent.unPublish();
                channelEvent.unSubscribe();
                channelEvent.Tag = null;
                Lookups.channelToEvent.Remove(sender);
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
            var callback = OperationContext.Current.GetCallbackChannel<IMessageToClient>();
            var channel = OperationContext.Current.Channel;

            TEventEntry channelEvent;
            if (!Lookups.channelToEvent.TryGetValue(channel, out channelEvent))
            {
                var queryParameters = HttpUtility.ParseQueryString((OperationContext.Current.IncomingMessageProperties["WebSocketMessageProperty"] as WebSocketMessageProperty).WebSocketContext.RequestUri.Query);
                var sessionName = queryParameters["session"];
                if (sessionName == null || sessionName == "") sessionName = "unknown";
                //Console.WriteLine("Start of client on session: " + sessionName);
                //Lookups.eventLog.WriteEntry("WS2IMB", "Start of client on session: " + sessionName);
                // signal start of client on session
                TEventEntry sessionEvent = Lookups.connection.publish(Lookups.RootEventName + "." + sessionName, false);
                channelEvent = Lookups.connection.subscribe(sessionEvent.eventName + "." + channel.SessionId, false);
                Lookups.channelToEvent[channel] = channelEvent;
                channelEvent.Tag = callback;
                channelEvent.onString += ChannelEvent_onString;
                channelEvent.onStreamCreate += ChannelEvent_onStreamCreate;
                channelEvent.onStreamEnd += ChannelEvent_onStreamEnd;
                channelEvent.onIntString += ChannelEvent_onIntString;
                sessionEvent.signalIntString(TEventEntry.actionNew, channelEvent.eventName);
                // todo: sessionEvent.onIntString += SessionEvent_onIntString;
                // sessionEvent.subscribe();
                // make client list queryable by session module
                Lookups.HookupRoot();
            }
            if (!msg.IsEmpty && ((IChannel)callback).State == CommunicationState.Opened)
            {
                //eventEntry.signalString(Encoding.UTF8.GetString(msg.GetBody<byte[]>()));
                byte[] returnMessage = msg.GetBody<byte[]>();
                if (returnMessage.Length > TConnection.imbMaximumPayloadSize / 10) // switch to stream for very large messages
                    channelEvent.signalStream("string", new MemoryStream(returnMessage));
                else
                    channelEvent.signalString(returnMessage);
            }
        }

        /*
        private void SessionEvent_onIntString(TEventEntry aEventEntry, int aInt, string aString)
        {
            if (aInt == TEventEntry.actionDelete)
            {
                // handle disconnect of session server side..
                // just unpublish to free up event, but keep on listening because web part is still up
                aEventEntry.unPublish();
            }
        }
        */

        private void ChannelEvent_onIntString(TEventEntry aEventEntry, int aInt, string aString)
        {
            if (aInt==TEventEntry.actionDelete)
            {
                // handle disconnect of session server side..
                // just unpublish to free up event, but keep on listening because web part is still up
                aEventEntry.unPublish();
                // signal web client session server is disconnected
                if (aEventEntry.Tag != null)
                    (aEventEntry.Tag as IMessageToClient).SendMessageToClient(CreateMessage("{\"connection\":{\"message\":\"session server has disconnected\"}}"));
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
