using System;
using System.Diagnostics;
using System.Collections.Generic;
using System.Text;

using System.IO;
using System.Net.Sockets;
using System.Net.Security;
using System.Threading;
using System.Security.Cryptography.X509Certificates;
using System.Security.Authentication;

namespace IMB
{
    public class TByteBuffer
    {
        // protobuf wire types
        public const int wtVarInt = 0;                        // int32, int64, uint32, uint64, sint32, sint64, bool, enum
        public const int wt64Bit = 1;                         // double or fixed int64/uint64
        public const int wtLengthDelimited = 2;               // string, bytes, embedded messages, packed repeated fields
        public const int wtStartGroup = 3;                    // deprecated
        public const int wtEndGroup = 4;                      // deprecated
        public const int wt32Bit = 5;                         // float (single) or fixed int32/uint32

        private byte[] fBuffer;
        private int fOffset = 0;
        private int fLimit = 0;

        public TByteBuffer(int aCapacity)
        {
            if (aCapacity > 0)
                fBuffer = new byte[aCapacity];
            fOffset = 0;
            fLimit = fBuffer.Length;
        }

        public TByteBuffer(byte[] aBuffer)
        {
            fBuffer = aBuffer;
            fOffset = 0;
            fLimit = fBuffer.Length;
        }

        public TByteBuffer(params byte[][] aBuffers)
        {
            int capacity = 0;
            foreach (byte[] b in aBuffers)
                capacity += b.Length;
            if (capacity > 0)
            {
                fBuffer = new byte[capacity];
                foreach (byte[] b in aBuffers)
                {
                    Buffer.BlockCopy(b, 0, this.fBuffer, this.fLimit, fBuffer.Length);
                    this.fLimit += b.Length;
                }
            }
        }

        public TByteBuffer Copy()
        {
            var res = new TByteBuffer(this.limit) { fOffset = this.fOffset };
            Buffer.BlockCopy(fBuffer, 0, res.fBuffer, 0, this.limit);
            return res;
        }

        public byte[] buffer { get { return fBuffer; } }
        public int offset { get { return fOffset; } set { fOffset = value; } }
        public int limit
        {
            get { return fLimit; }
            set
            {
                if (value > fBuffer.Length)
                    Array.Resize<byte>(ref fBuffer, value);
                fLimit = value;
            }
        }


        public int remaining { get { return fLimit - fOffset; } }

        public byte firstByte { get { return fBuffer[0]; } }

        public void ShiftLeft(byte aMostRightByte)
        {
            Buffer.BlockCopy(fBuffer, 1, fBuffer, 0, fBuffer.Length - 1);
            fBuffer[fBuffer.Length - 1] = aMostRightByte;
        }

        public UInt64 bb_read_uint64() // unsigned varint
        {
            int shiftLeft = 0;
            UInt64 b = 0;
            UInt64 res = 0;
            do
            {
                b = fBuffer[fOffset++];
                res |= (b & 0x7F) << shiftLeft;
                shiftLeft += 7;
            } while (b >= 128);
            return res;
        }

        public Int64 bb_read_int64() // signed varint
        {
            UInt64 ui64 = bb_read_uint64();
            // remove sign bit
            Int64 res = (Int64)(ui64 >> 1);
            // adjust for negative
            if ((ui64 & 1) == 1)
                res = -(res + 1);
            return res;
        }

        public UInt32 bb_read_uint32() // unsigned varint
        {
            int shiftLeft = 0;
            UInt32 b = 0;
            UInt32 res = 0;
            do
            {
                b = fBuffer[fOffset++];
                res |= (b & 0x7F) << shiftLeft;
                shiftLeft += 7;
            } while (b >= 128);
            return res;
        }

        public Int32 bb_read_int32() // signed varint
        {
            UInt32 ui32 = bb_read_uint32();
            // remove sign bit
            Int32 res = (Int32)(ui32 >> 1);
            // adjust for negative
            if ((ui32 & 1) == 1)
                res = -(res + 1);
            return res;
        }

        public UInt16 bb_read_uint16() // fixed 16 bit (cannot be tagged)
        {
            UInt16 u = BitConverter.ToUInt16(fBuffer, fOffset);
            fOffset += 2;
            return u;
        }

        public bool bb_read_bool() // 1 byte varint
        {
            return fBuffer[fOffset++] != 0;
        }

        public double bb_read_double() // 64 bit float
        {
            Double d = BitConverter.ToDouble(fBuffer, fOffset);
            fOffset += 8;
            return d;
        }

        public float bb_read_single() //  32 bit float
        {
            Single s = BitConverter.ToSingle(fBuffer, fOffset);
            fOffset += 4;
            return s;
        }

        public Guid bb_read_guid() // length delimited
        {
            return new Guid(bb_read_bytes());
        }

        public string bb_read_string() // length delimited
        {
            int len = (int)bb_read_uint64();
            if (len > 0)
            {
                string s = Encoding.UTF8.GetString(fBuffer, fOffset, len);
                fOffset += len;
                return s;
            }
            else
                return "";
        }

        public byte[] bb_read_bytes() // length delimited
        {
            int len = (int)bb_read_uint64();
            byte[] r = new byte[len];
            Buffer.BlockCopy(fBuffer, fOffset, r, 0, len);
            fOffset += len;
            return r;
        }

        // skip reading
        public void bb_read_skip(int aWiretype)
        {
            switch (aWiretype)
            {
                case wtVarInt:
                    bb_read_uint64();
                    break;
                case wt32Bit:
                    fOffset += 4;
                    break;
                case wt64Bit:
                    fOffset += 4;
                    break;
                case wtLengthDelimited:
                    UInt64 len = bb_read_uint64();
                    fOffset += (int)len;
                    break;
                default:
                    throw new Exception("Unsupported wire type (" + aWiretype.ToString() + ") in TByteBuffer.bb_read_skip");
            }
        }

        public void bb_read_skip_bytes(int aNumberOfBytes) { fOffset += aNumberOfBytes; }

        // join array of byte[] to 1 array of bytes
        public static byte[] bb_join(params byte[][] aBuffers)
        {
            int capacity = 0;
            foreach (byte[] b in aBuffers)
                capacity += b.Length;
            byte[] r = new byte[capacity];
            int offset = 0;
            foreach (byte[] b in aBuffers)
            {
                Buffer.BlockCopy(b, 0, r, offset, b.Length);
                offset += b.Length;
            }
            return r;
        }

        // field writing
        public static byte[] bb_bool(bool aValue) // unsigned varint
        {
            if (aValue)
                return new byte[] { 1 };
            else
                return new byte[] { 0 };
        }

        public static byte[] bb_uint16(UInt16 aValue) // fixed 16 bit (cannot be tagged)
        {
            return BitConverter.GetBytes(aValue);
        }

        private static int bb_var_int_length(UInt64 aValue)
        {
            // encode in blocks of 7 bits (high order bit of byte is signal that more bytes are to follow
            // encode lower numbers directly for speed
            if (aValue < 128)
                return 1;
            else if (aValue < 128 * 128)
                return 2;
            else if (aValue < 128 * 128 * 128)
                return 3;
            else
            {
                // 4 bytes or more: change to dynamic size detection
                int res = 4;
                aValue >>= 7 * 4;
                while (aValue > 0)
                {
                    aValue >>= 7;
                    res++;
                }
                return res;
            }
        }

        public static byte[] bb_uint64(UInt64 aValue) // unsigned varint
        {
            byte[] ba = new byte[bb_var_int_length(aValue)];
            int offset = 0;
            while (aValue >= 128)
            {
                ba[offset++] = (byte)((aValue & 0x7F) | 0x80); // msb: signal more bytes are to follow
                aValue >>= 7;
            }
            ba[offset++] = (byte)aValue; // aValue<128 (msb already 0)
            return ba;
        }

        public static byte[] bb_uint32(UInt32 aValue) // unsigned varint
        {
            byte[] ba = new byte[bb_var_int_length(aValue)];
            int offset = 0;
            while (aValue >= 128)
            {
                ba[offset++] = (byte)((aValue & 0x7F) | 0x80); // msb: signal more bytes are to follow
                aValue >>= 7;
            }
            ba[offset++] = (byte)aValue; // aValue<128 (msb already 0)
            return ba;
        }

        public static byte[] bb_int64(Int64 aValue)// signed varint
        {
            if (aValue < 0)
                return bb_uint64(((UInt64)(-(aValue + 1)) << 1) | 1);
            else
                return bb_uint64((UInt64)aValue << 1);
        }

        public static byte[] bb_int32(Int32 aValue)// unsigned varint
        {
            if (aValue < 0)
                return bb_uint32(((UInt32)(-(aValue + 1)) << 1) | 1);
            else
                return bb_uint32((UInt32)aValue << 1);
        }

        public static byte[] bb_single(float aValue) // length delimited
        {
            return BitConverter.GetBytes(aValue);
        }

        public static byte[] bb_double(double aValue) // length delimited
        {
            return BitConverter.GetBytes(aValue);
        }

        public static byte[] bb_bytes(byte[] aValue) // length delimited
        {
            UInt64 len = (UInt64)aValue.Length;
            return bb_join(bb_uint64(len), aValue);
        }

        public static byte[] bb_string(string aValue) // length delimited
        {
            byte[] ba = Encoding.UTF8.GetBytes(aValue);
            UInt64 len = (UInt64)ba.Length;
            return bb_join(bb_uint64(len), ba);
        }

        public static byte[] bb_guid(Guid aValue) // length delimited
        {
            byte[] ba = aValue.ToByteArray();
            UInt64 len = (UInt64)ba.Length;
            return bb_join(bb_uint64(len), ba);
        }

        // taged field writing
        public static byte[] bb_tag_int32(UInt32 aTag, Int32 aValue)
        {
            return bb_join(bb_uint32(aTag << 3 | wtVarInt), bb_int32(aValue));
        }

        public static byte[] bb_tag_uint32(UInt32 aTag, UInt32 aValue)
        {
            return bb_join(bb_uint32(aTag << 3 | wtVarInt), bb_uint32(aValue));
        }

        public static byte[] bb_tag_int64(UInt32 aTag, Int64 aValue)
        {
            return bb_join(bb_uint32(aTag << 3 | wtVarInt), bb_int64(aValue));
        }

        public static byte[] bb_tag_uint64(UInt32 aTag, UInt64 aValue)
        {
            return bb_join(bb_uint32(aTag << 3 | wtVarInt), bb_uint64(aValue));
        }

        public static byte[] bb_tag_bool(UInt32 aTag, bool aValue)
        {
            return bb_join(bb_uint32(aTag << 3 | wtVarInt), bb_bool(aValue));
        }

        public static byte[] bb_tag_single(UInt32 aTag, float aValue)
        {
            return bb_join(bb_uint32(aTag << 3 | wt32Bit), bb_single(aValue));
        }

        public static byte[] bb_tag_double(UInt32 aTag, double aValue)
        {
            return bb_join(bb_uint32(aTag << 3 | wt64Bit), bb_double(aValue));
        }

        public static byte[] bb_tag_guid(UInt32 aTag, Guid aValue)
        {
            return bb_join(bb_uint32(aTag << 3 | wtLengthDelimited), bb_guid(aValue));
        }

        public static byte[] bb_tag_string(UInt32 aTag, string aValue)
        {
            return bb_join(bb_uint32(aTag << 3 | wtLengthDelimited), bb_string(aValue));
        }

        public static byte[] bb_tag_bytes(UInt32 aTag, byte[] aValue)
        {
            return bb_join(bb_uint32(aTag << 3 | wtLengthDelimited), bb_bytes(aValue));
        }
    }

    class TStreamCacheEntry
    {
        string fStreamName;
        Stream fStream;

        public TStreamCacheEntry(string aStreamName, Stream aStream)
        {
            fStreamName = aStreamName;
            fStream = aStream;
        }

        public string streamName { get { return fStreamName; } set { fStreamName = value; } }
        public Stream stream { get { return fStream; } set { fStream = value; } }
    }

    public class TEventEntry
    {
        public const int imbMaxStreamBodyBuffer = 8 * 1024;

        // change object actions
        public const int actionNew = 0;
        public const int actionDelete = 1;
        public const int actionChange = 2;
        public const int actionInquire = 3;

        // basic event tags
        public const int icehIntString = 1;                 // <varint>
        public const int icehIntStringPayload = 2;          // <string>
        public const int icehString = 3;                    // <string>
        public const int icehChangeObject = 4;              // <int32: varint>
        public const int icehChangeObjectAction = 5;        // <int32: varint>
        public const int icehChangeObjectAttribute = 6;     // <string>

        public const int icehStreamHeader = 7;              // <string> filename
        public const int icehStreamBody = 8;                // <bytes>
        public const int icehStreamEnd = 9;                 // <bool> true: ok, false: cancel
        public const int icehStreamID = 10;                 // <id: bytes/string>
        
        public const int icehNoObjectID = 11;               // <id: bytes/string>
        public const int icehObjectID = 12;                 // <id: bytes/string>
        public const int icehNoAttribute = 13;              // <uint32: varint> key of attribute
        public const int icehAttributeBase = 14;            // attributes of object are icehAttributeBase..
        public const int icehAttributeTop = 899;            // todo: may be lower?

        public const int icehWorldCommandBase = 900;        // .. 999

        public const int icehMonitoringBase = 1000;         // 1100..1199 monitor

        public const int icehLogLine = 1200;                // <string>
        public const int icehLogLevel = 1201;               // <uint32: varint>
        public const int icehLogSource = 1202;              // <string>

        // constructor
        public TEventEntry(TConnection aConnection, UInt32 aEventID, string aEventName)
        {
            fConnection = aConnection;
            fEventID = aEventID;
            fEventName = aEventName;
        }

        // private methods
        public void handleEvent(TByteBuffer aBuffer)
        {
            if (onEvent != null)
                onEvent(this, aBuffer);
            else
            {
                string eventString = "";
                int action = -1;
                string attribute = "";
                Guid streamID = Guid.Empty;
                TStreamCacheEntry sce; // streamCacheEntry
                UInt32 logLevel = 0;
                string logSource = "";
                //var startingOffset = aBuffer.offset; // store offset to handle event in separate onEvent handler
                while (aBuffer.remaining > 0)
                {
                    UInt32 fieldInfo = aBuffer.bb_read_uint32();
                    switch (fieldInfo)
                    {
                        // int string
                        case (icehIntString << 3) | TByteBuffer.wtVarInt:
                            int eventInt = aBuffer.bb_read_int32();
                            if (onIntString != null)
                                onIntString(this, eventInt, eventString);
                            break;
                        case (icehIntStringPayload << 3) | TByteBuffer.wtLengthDelimited:
                            eventString = aBuffer.bb_read_string();
                            break;
                        // string
                        case (icehString << 3) | TByteBuffer.wtLengthDelimited:
                            eventString = aBuffer.bb_read_string();
                            if (onString != null)
                                onString(this, eventString);
                            break;
                        // change object
                        case (icehChangeObjectAction << 3) | TByteBuffer.wtVarInt:
                            action = aBuffer.bb_read_int32();
                            break;
                        case (icehChangeObjectAttribute << 3) | TByteBuffer.wtLengthDelimited:
                            attribute = aBuffer.bb_read_string();
                            break;
                        case (icehChangeObject << 3) | TByteBuffer.wtVarInt:
                            int objectID = aBuffer.bb_read_int32();
                            if (onChangeObject != null)
                                onChangeObject(this, action, objectID, attribute);
                            break;
                        // streams
                        case (icehStreamHeader << 3) | TByteBuffer.wtLengthDelimited:
                            string streamName = aBuffer.bb_read_string();
                            if (onStreamCreate != null)
                            {
                                Stream stream = onStreamCreate(this, streamName);
                                if (stream != null)
                                {
                                    if (fStreamCache.TryGetValue(streamID, out sce))
                                    {
                                        sce.stream = stream;
                                        sce.streamName = streamName;
                                    }
                                    else
                                        fStreamCache.Add(streamID, new TStreamCacheEntry(streamName, stream));
                                }
                            }
                            break;
                        case (icehStreamBody << 3) | TByteBuffer.wtLengthDelimited:
                            byte[] block = aBuffer.bb_read_bytes();
                            if (fStreamCache.TryGetValue(streamID, out sce))
                                sce.stream.Write(block, 0, block.Length);
                            break;
                        case (icehStreamEnd << 3) | TByteBuffer.wtVarInt:
                            bool cancel = aBuffer.bb_read_bool();
                            if (fStreamCache.TryGetValue(streamID, out sce))
                            {
                                if (onStreamEnd != null)
                                    onStreamEnd(this, sce.streamName, sce.stream, cancel);
                                sce.stream.Close();
                                fStreamCache.Remove(streamID);
                            }
                            break;
                        case (icehStreamID << 3) | TByteBuffer.wtLengthDelimited:
                            streamID = aBuffer.bb_read_guid();
                            break;
                        case (icehLogLevel << 3) | TByteBuffer.wtVarInt:
                            logLevel = aBuffer.bb_read_uint32();
                            break;
                        case (icehLogSource << 3) | TByteBuffer.wtLengthDelimited:
                            logSource = aBuffer.bb_read_string();
                            break;
                        case (icehLogLine << 3) | TByteBuffer.wtLengthDelimited:
                            var logLine = aBuffer.bb_read_string();
                            if (onLog != null)
                                onLog(this, logLine, logLevel, logSource);
                            break;
                        default:
                            if (onTag != null)
                                onTag(this, fieldInfo, aBuffer);
                            else
                                aBuffer.bb_read_skip((int)(fieldInfo & 7));
                            break;
                    }
                }
            }
        }

        public void handleSubAndPub(UInt32 aCommand)
        {
            switch (aCommand)
            {
                case TConnection.icehSubscribe:
                    fSubscribers = true;
                    break;
                case TConnection.icehPublish:
                    fPublishers = true;
                    break;
                case TConnection.icehUnsubscribe:
                    fSubscribers = false;
                    break;
                case TConnection.icehUnpublish:
                    fPublishers = false;
                    break;
            }
            if (onSubAndPub != null)
                onSubAndPub(this, aCommand);
        }

        private void signalSubscribe()
        {
            fConnection.writeCommand(
                TByteBuffer.bb_tag_string(TConnection.icehEventName, eventName),
                TByteBuffer.bb_tag_uint32(TConnection.icehSubscribe, eventID));
        }

        private void signalPublish()
        {
            fConnection.writeCommand(
                TByteBuffer.bb_tag_string(TConnection.icehEventName, eventName),
                TByteBuffer.bb_tag_uint32(TConnection.icehPublish, eventID));
        }

        private void signalUnSubscribe()
        {
            fConnection.writeCommand(
                TByteBuffer.bb_tag_uint32(TConnection.icehUnsubscribe, eventID));
        }

        private void signalUnPublish()
        {
            fConnection.writeCommand(
                TByteBuffer.bb_tag_uint32(TConnection.icehUnpublish, eventID));
        }

        // fields
        private TConnection fConnection;
        private UInt32 fEventID;
        private string fEventName;

        private bool fIsSubscribed = false;
        private bool fIsPublished = false;
        private bool fSubscribers = false;
        private bool fPublishers = false;
        private Dictionary<Guid, TStreamCacheEntry> fStreamCache = new Dictionary<Guid, TStreamCacheEntry>();

        // properties
        public TConnection connection { get { return fConnection; } }
        public string eventName { get { return fEventName; } }
        public UInt32 eventID { get { return fEventID; } }
        public bool isSubscribed { get { return fIsSubscribed; } }
        public bool isPublished { get { return fIsPublished; } }
        public bool subscribers { get { return fSubscribers; } }
        public bool publishers { get { return fPublishers; } }

        public TEventEntry publish()
        {
            if (!fIsPublished)
            {
                signalPublish();
                fIsPublished = true;
            }
            return this;
        }

        public TEventEntry subscribe()
        {
            if (!fIsSubscribed)
            {
                signalSubscribe();
                fIsSubscribed = true;
            }
            return this;
        }

        public TEventEntry unPublish()
        {
            if (fIsPublished)
            {
                signalUnPublish();
                fIsPublished = false;
            }
            return this;
        }

        public TEventEntry unSubscribe()
        {
            if (fIsSubscribed)
            {
                signalUnSubscribe();
                fIsSubscribed = false;
            }
            return this;
        }

        public void signalEvent(params byte[][] aPayloads)
        {
            publish();
            byte[] bufferEventID = TByteBuffer.bb_uint16((UInt16)fEventID);
            byte[] payload = TByteBuffer.bb_join(aPayloads);
            Int64 size = bufferEventID.Length + payload.Length;
            fConnection.writePacket(TByteBuffer.bb_join(
                new byte[1] { TConnection.imbMagic },
                TByteBuffer.bb_int64(size),
                bufferEventID,
                payload
                ));
        }

        // signal event
        public void signalChangeObject(int aAction, int aObjectID, string aAttribute = "")
        {
            signalEvent(
                TByteBuffer.bb_tag_int32(icehChangeObjectAction, aAction),
                TByteBuffer.bb_tag_string(icehChangeObjectAttribute, aAttribute),
                TByteBuffer.bb_tag_int32(icehChangeObject, aObjectID)
                );
        }

        public void signalString(string aString)
        {
            signalEvent(
                TByteBuffer.bb_tag_string(icehString, aString)
                );
        }

        public void signalString(byte[] aStringInUTF8Bytes) // string in UTF8 bytes
        {
            signalEvent(
                TByteBuffer.bb_tag_bytes(icehString, aStringInUTF8Bytes)
                );
        }

        public void signalIntString(int aInt, string aString)
        {
            signalEvent(
                TByteBuffer.bb_tag_string(icehIntStringPayload, aString),
                TByteBuffer.bb_tag_int32(icehIntString, aInt)
                );
        }

        public void signalStream(string aName, Stream aStream)
        {
            byte[] bufferStreamID = TByteBuffer.bb_tag_guid(icehStreamID, Guid.NewGuid());
            // header
            signalEvent(bufferStreamID, TByteBuffer.bb_tag_string(icehStreamHeader, aName));
            // body
            byte[] buffer = new byte[imbMaxStreamBodyBuffer];
            int readSize = aStream.Read(buffer, 0, imbMaxStreamBodyBuffer);
            while (readSize > 0)
            {
                if (readSize < buffer.Length)
                    Array.Resize<byte>(ref buffer, readSize);
                signalEvent(bufferStreamID, TByteBuffer.bb_tag_bytes(icehStreamBody, buffer));
                readSize = aStream.Read(buffer, 0, readSize);
            }
            // end
            signalEvent(bufferStreamID, TByteBuffer.bb_tag_bool(icehStreamEnd, readSize != 0));
        }

        public void signalLogEntry(string aLine, UInt32 aLevel, string aSource = "")
        {
            if (aSource != "")
                signalEvent(
                    TByteBuffer.bb_tag_uint32(icehLogLevel, aLevel),
                    TByteBuffer.bb_tag_string(icehLogSource, aSource),
                    TByteBuffer.bb_tag_string(icehLogLine, aLine)
                    );
            else
                signalEvent(
                    TByteBuffer.bb_tag_uint32(icehLogLevel, aLevel),
                    TByteBuffer.bb_tag_string(icehLogLine, aLine)
                    );
        }


        // handlers
        public delegate void TOnChangeObject(TEventEntry aEventEntry, Int32 aAction, Int32 aObjectID, string aAttribute);
        public event TOnChangeObject onChangeObject = null;

        public delegate void TOnString(TEventEntry aEventEntry, string aString);
        public event TOnString onString = null;

        public delegate void TOnIntString(TEventEntry aEventEntry, int aInt, string aString);
        public event TOnIntString onIntString = null;

        public delegate void TOnTag(TEventEntry aEventEntry, UInt32 aFieldInfo, TByteBuffer aPayload);
        public event TOnTag onTag = null;

        public delegate Stream TOnStreamCreate(TEventEntry aEventEntry, string aName);
        public event TOnStreamCreate onStreamCreate = null;

        public delegate void TOnStreamEnd(TEventEntry aEventEntry, string aName, Stream stream, bool aCancel);
        public event TOnStreamEnd onStreamEnd = null;

        public delegate void TOnSubAndPub(TEventEntry aEventEntry, UInt32 aCommand);
        public event TOnSubAndPub onSubAndPub = null;

        public delegate void TOnLog(TEventEntry aEventEntry, string aLine, UInt32 aLevel, string aSource);
        public event TOnLog onLog = null;

        public Action<TEventEntry, TByteBuffer> onEvent = null; // overrules all other tag events, can only call single entry 

        public object Tag { get; set; }
    }

    public abstract class TConnection
    {
        public const string imbDefaultRemoteHost = "vps17642.public.cloudvps.com"; // "localhost";  "localhost"; // 

        public const string imbDefaultPrefix = "ecodistrict"; // "nl.imb";

        public const byte imbMagic = 0xFE;

        public const int imbMinimumPacketSize = 16;
        public const int imbMaximumPayloadSize = 10 * 1024 * 1024;
        public const int imbSocketDefaultLingerTimeInSec = 2; // in sec

        // client state
        public const int icsUninitialized = 0;
        public const int icsInitialized = 1;
        public const int icsClient = 2;
        public const int icsHub = 3;
        public const int icsEnded = 4;
        public const int icsTimer = 10;
        public const int icsGateway = 100;

        // command tags
        public const int icehRemark = 1;                      // <string>
        public const int icehSubscribe = 2;                   // <uint32: varint>
        public const int icehPublish = 3;                     // <uint32: varint>
        public const int icehUnsubscribe = 4;                 // <uint32: varint>
        public const int icehUnpublish = 5;                   // <uint32: varint>
        public const int icehSetEventIDTranslation = 6;       // <uint32: varint>
        public const int icehEventName = 7;                   // <string>
        public const int icehEventID = 8;                     // <uint32: varint>

        public const int icehUniqueClientID = 11;             // <guid>
        public const int icehHubID = 12;                      // <guid>
        public const int icehModelName = 13;                  // <string>
        public const int icehModelID = 14;                    // <int32: varint> ?
        public const int icehState = 16;                      // <uint32: varint>
        public const int icehEventNameFilter = 17;            // <string>
        public const int icehClose = 21;                      // <bool: varint>

        public const UInt32 imbInvalidEventID = 0xFFFFFFFF;

        // abstract methods to override
        protected abstract bool getConnected();
        protected abstract void setConnected(bool aValue);
        protected abstract int readBytes(byte[] aBuffer, int aOffset, int aLimit);
        public abstract void writePacket(byte[] aPacket/*, bool aCallCloseOnError = true*/);

        public TConnection(string aModelname, int aModelID, string aPrefix = imbDefaultPrefix)
        {
            fModelName = aModelname;
            fModelID = aModelID;
            fPrefix = aPrefix;
        }

        ~TConnection()
        {
            connected = false;
            fLocalEventEntries.Clear();
            fRemoteEventEntries.Clear();
        }

        // fields
        protected string fModelName;
        protected int fModelID;
        protected string fPrefix;
        protected List<TEventEntry> fLocalEventEntries = new List<TEventEntry>();
        protected Dictionary<UInt32, TEventEntry> fRemoteEventEntries = new Dictionary<UInt32, TEventEntry>();
        protected Guid fUniqueClientID;
        protected Guid fHubID;

        public string modelName { get { return fModelName; } }
        public int modelID { get { return fModelID; } }
        public string prefix { get { return fPrefix; } }
        public Guid uniqueClientID { get { return fUniqueClientID; } }
        public Guid hubID { get { return fHubID; } }

        public delegate void TOnDisconnect(TConnection aConnection);
        public event TOnDisconnect onDisconnect = null;
        public delegate void TOnException(TConnection aConnection, Exception aException);
        public event TOnException onException = null;

        public string monitorEventName
        {
            get
            {
                if (fHubID.CompareTo(Guid.Empty) == 0)
                    return "";
                else
                    return "Hubs." + fHubID.ToString("N").ToUpper() + ".Monitor";
            }
        }

        public string privateEventName
        {
            get
            {
                if (fUniqueClientID.CompareTo(Guid.Empty) == 0)
                    return "";
                else
                    return "Clients." + fUniqueClientID.ToString("N").ToUpper() + ".Private";
            }
        }

        protected void waitForConnected(int aSpinCount = 20)
        {
            while (fUniqueClientID.CompareTo(Guid.Empty) == 0 && aSpinCount != 0)
            {
                Thread.Sleep(500);
                aSpinCount--;
            }
        }

        protected void handleCommand(TByteBuffer aBuffer)
        {
            TEventEntry eventEntry = null;
            string eventName = "";
            UInt32 localEventID = imbInvalidEventID;
            // process tags
            while (aBuffer.remaining > 0)
            {
                UInt32 fieldInfo = aBuffer.bb_read_uint32();
                switch (fieldInfo)
                {
                    case (icehSubscribe << 3) | TByteBuffer.wtVarInt:
                        UInt32 remoteEventID = aBuffer.bb_read_uint32();
                        if (fRemoteEventEntries.TryGetValue(remoteEventID, out eventEntry))
                            eventEntry.handleSubAndPub(icehSubscribe);
                        break;
                    case (icehPublish << 3) | TByteBuffer.wtVarInt:
                        remoteEventID = aBuffer.bb_read_uint32();
                        if (fRemoteEventEntries.TryGetValue(remoteEventID, out eventEntry))
                            eventEntry.handleSubAndPub(icehPublish);
                        break;
                    case (icehUnsubscribe << 3) | TByteBuffer.wtVarInt:
                        eventName = "";
                        remoteEventID = aBuffer.bb_read_uint32();
                        if (fRemoteEventEntries.TryGetValue(remoteEventID, out eventEntry))
                            eventEntry.handleSubAndPub(icehUnsubscribe);
                        break;
                    case (icehUnpublish << 3) | TByteBuffer.wtVarInt:
                        eventName = "";
                        remoteEventID = aBuffer.bb_read_uint32();
                        if (fRemoteEventEntries.TryGetValue(remoteEventID, out eventEntry))
                            eventEntry.handleSubAndPub(icehUnpublish);
                        break;
                    case (icehEventName << 3) | TByteBuffer.wtLengthDelimited:
                        eventName = aBuffer.bb_read_string();
                        break;
                    case (icehEventID << 3) | TByteBuffer.wtVarInt:
                        localEventID = aBuffer.bb_read_uint32();
                        break;
                    case (icehSetEventIDTranslation << 3) | TByteBuffer.wtVarInt:
                        remoteEventID = aBuffer.bb_read_uint32();
                        lock (fLocalEventEntries)
                        {
                            if (fRemoteEventEntries.ContainsKey(remoteEventID))
                                fRemoteEventEntries.Remove(remoteEventID);
                            if (localEventID < fLocalEventEntries.Count)
                            {
                                eventEntry = fLocalEventEntries[(int)localEventID];
                                fRemoteEventEntries.Add(remoteEventID, eventEntry);
                            }
                            // else local event id is invalid so invalidate remote event id -> keep removed
                        }
                        break;
                    case (icehClose << 3) | TByteBuffer.wtVarInt:
                        aBuffer.bb_read_bool();
                        close(false);
                        break;
                    case (icehHubID << 3) | TByteBuffer.wtLengthDelimited:
                        fHubID = aBuffer.bb_read_guid();
                        break;
                    case (icehUniqueClientID << 3) | TByteBuffer.wtLengthDelimited:
                        fUniqueClientID = aBuffer.bb_read_guid();
                        break;
                    default:
                        aBuffer.bb_read_skip((int)fieldInfo & 7);
                        break;
                }
            }
        }

        public void close(bool aSendCloseCmd = true)
        {
            if (connected)
            {
                if (onDisconnect != null)
                    onDisconnect(this);
                if (aSendCloseCmd)
                    try
                    {
                        writeCommand(TByteBuffer.bb_tag_bool(icehClose, false));
                    }
                    catch { } //  catch and ignore all
                connected = false;
            }
        }

        protected void signalConnectInfo(string aModelName, int aModelID)
        {
            writeCommand(
                TByteBuffer.bb_tag_string(icehModelName, aModelName),
                TByteBuffer.bb_tag_int32(icehModelID, aModelID),
                TByteBuffer.bb_tag_uint32(icehState, icsClient),
                // bb_tag_bool(icehReconnectable, False),
                // bb_tag_string(icehEventNameFilter, ''),
                TByteBuffer.bb_tag_guid(icehUniqueClientID, fUniqueClientID)); // trigger
        }

        public void signalHeartbeat(string aRemark = "")
        {
            if (aRemark == "")
                writePacket(new byte[2] { TConnection.imbMagic, 0 });
            else
                writeCommand(TByteBuffer.bb_tag_string(icehRemark, aRemark));
        }

        protected Thread fHeartbeatThread = null;
        protected int fHeartbeatInterval = 0;

        protected void sendHeartbeats() // heartbeat thread loop
        {
            while (connected)
            {
                signalHeartbeat();
                if (connected)
                    Thread.Sleep(fHeartbeatInterval);
            }
        }

        public void setHeartBeat(int aIntervalms)
        {
            fHeartbeatInterval = aIntervalms;
            if (aIntervalms > 0)
            {
                if (fHeartbeatThread == null)
                {
                    fHeartbeatThread = new Thread(sendHeartbeats);
                    fHeartbeatThread.Name = "IMB heartbeat";
                    fHeartbeatThread.Start();
                }
            }
            else
            {
                if (fHeartbeatThread != null)
                {
                    fHeartbeatThread.Abort();
                    fHeartbeatThread = null;
                }
            }
        }

        protected void readPackets() // event reader thread loop
        {
            // read from socket and process
            TByteBuffer packet = new TByteBuffer(imbMinimumPacketSize);
            do
            {
                try
                {
                    packet.offset = 0;
                    packet.limit = imbMinimumPacketSize;
                    int receivedBytes = readBytes(packet.buffer, packet.offset, packet.limit);
                    if (receivedBytes == imbMinimumPacketSize)
                    {
                        while (packet.firstByte != imbMagic)
                        {
                            TByteBuffer oneByte = new TByteBuffer(1);
                            if (readBytes(oneByte.buffer, oneByte.offset, oneByte.limit) == 1)
                                packet.ShiftLeft(oneByte.firstByte);
                            else
                            {
                                close(false);
                                break;
                            }
                        }
                        packet.bb_read_skip_bytes(1);
                        // we have magic ate the first byte
                        int size = (int)packet.bb_read_int64();
                        int extraBytesOffset = packet.limit;
                        packet.limit = packet.offset + Math.Abs(size);
                        if (packet.limit - extraBytesOffset > 0)
                            receivedBytes = readBytes(packet.buffer, extraBytesOffset, packet.limit);
                        if (size > 0)
                        {
                            // event
                            UInt16 eventID = packet.bb_read_uint16();
                            TEventEntry eventEntry;
                            if (fRemoteEventEntries.TryGetValue(eventID, out eventEntry))
                                eventEntry.handleEvent(packet);
                        }
                        else
                        {
                            // command
                            handleCommand(packet);
                        }
                    }
                    else
                    {
                        close(false);
                        break;
                    }
                }
                catch (Exception e)
                {
                    if ((e is ThreadAbortException))
                    {
                        Thread.ResetAbort();
                        close(false);
                        break;
                    }
                    else
                    {
                        if (connected && onException != null)
                            onException(this, e);
                    }
                }
            } while (connected);
        }

        public List<TEventEntry> eventEntries { get { return fLocalEventEntries; } }
        public bool connected { get { return getConnected(); } set { setConnected(value); } }

        public TEventEntry subscribe(string aEventName, bool aUsePrefix = true)
        {
            string longEventName = aEventName;
            if (aUsePrefix)
                longEventName = fPrefix + "." + longEventName;
            string upperLongEventName = longEventName.ToUpper();
            lock (fLocalEventEntries)
            {
                foreach (TEventEntry eventEntry in fLocalEventEntries)
                {
                    if (eventEntry.eventName.ToUpper().CompareTo(upperLongEventName) == 0)
                        return eventEntry.subscribe();
                }
                // if we come here we have not found an existing event entry
                TEventEntry newEventEntry = new TEventEntry(this, (UInt32)fLocalEventEntries.Count, longEventName);
                fLocalEventEntries.Add(newEventEntry);
                return newEventEntry.subscribe();
            }
        }

        public bool unsubscribe(string aEventName, bool aUsePrefix = true)
        {
            string longEventName = aEventName;
            if (aUsePrefix)
                longEventName = fPrefix + "." + longEventName;
            string upperLongEventName = longEventName.ToUpper();
            lock (fLocalEventEntries)
            {
                foreach (TEventEntry eventEntry in fLocalEventEntries)
                {
                    if (eventEntry.eventName.ToUpper().CompareTo(upperLongEventName) == 0)
                    {
                        eventEntry.unSubscribe();
                        return true;
                    }
                }
                return false;
            }
        }

        public TEventEntry publish(string aEventName, bool aUsePrefix = true)
        {
            string longEventName = aEventName;
            if (aUsePrefix)
                longEventName = fPrefix + "." + longEventName;
            string upperLongEventName = longEventName.ToUpper();
            lock (fLocalEventEntries)
            {
                foreach (TEventEntry eventEntry in fLocalEventEntries)
                {
                    if (eventEntry.eventName.ToUpper().CompareTo(upperLongEventName) == 0)
                        return eventEntry.publish();
                }
                // if we come here we have not found an existing event entry
                TEventEntry newEventEntry = new TEventEntry(this, (UInt32)fLocalEventEntries.Count, longEventName);
                fLocalEventEntries.Add(newEventEntry);
                return newEventEntry.publish();
            }
        }

        public bool unpublish(string aEventName, bool aUsePrefix = true)
        {
            string longEventName = aEventName;
            if (aUsePrefix)
                longEventName = fPrefix + "." + longEventName;
            string upperLongEventName = longEventName.ToUpper();
            lock (fLocalEventEntries)
            {
                foreach (TEventEntry eventEntry in fLocalEventEntries)
                {
                    if (eventEntry.eventName.ToUpper().CompareTo(upperLongEventName) == 0)
                    {
                        eventEntry.unPublish();
                        return true;
                    }
                }
                return false;
            }
        }

        public void writeCommand(params byte[][] aPayloads)
        {
            byte[] payload = TByteBuffer.bb_join(aPayloads);
            Int64 size = payload.Length;
            writePacket(TByteBuffer.bb_join(
                new byte[1] { TConnection.imbMagic },
                TByteBuffer.bb_int64(-size),
                payload
                ));
        }

    }

    public class TSocketConnection : TConnection
    {
        public const int imbDefaultSocketRemotePort = 4004;

        public TSocketConnection(
            string aModelName, int aModelID = 0,
            string aPrefix = imbDefaultPrefix,
            string aRemoteHost = imbDefaultRemoteHost, int aRemotePort = imbDefaultSocketRemotePort)
            : base(aModelName, aModelID, aPrefix)
        {
            fRemoteHost = aRemoteHost;
            fRemotePort = aRemotePort;
            connected = true;
        }

        protected string fRemoteHost;
        protected int fRemotePort;
        protected Thread fReaderThread = null;
        protected TcpClient fClient = null;
        protected NetworkStream fNetStream = null;

        protected override bool getConnected()
        {
            return fClient != null;
        }

        protected override void setConnected(bool aValue)
        {
            if (aValue)
            {
                if (!connected)
                {

                    fClient = new TcpClient(fRemoteHost, fRemotePort);
                    fNetStream = fClient.GetStream();
                    fReaderThread = new Thread(readPackets);
                    fReaderThread.Name = "IMB reader";
                    fReaderThread.Start();
                    // send connect info
                    signalConnectInfo(fModelName, fModelID);
                    // wait for unique client id as a signal that we are connected
                    waitForConnected();
                }
            }
            else
            {
                if (connected)
                {
                    writeCommand(TByteBuffer.bb_tag_bool(icehClose, false));

                    fClient.Close();
                    fClient = null; // new TcpClient(); // cannot use old connection so create new one to make later call to OpenLow possible
                    fNetStream.Close();
                    fNetStream = null;
                }
            }
        }

        protected override int readBytes(byte[] aBuffer, int aOffset, int aLimit)
        {
            if (connected)
            {
                int totBytesReceived = 0;
                int bytesReceived;
                do
                {
                    bytesReceived = fNetStream.Read(aBuffer, aOffset, aLimit - aOffset);
                    aOffset += bytesReceived;
                    totBytesReceived += bytesReceived;
                }
                while (aLimit - aOffset > 0 && bytesReceived > 0);
                return totBytesReceived;
            }
            else
                return -1;
        }

        public override void writePacket(byte[] aPacket /*, bool aCallCloseOnError = true*/)
        {
            lock (fNetStream)
            {
                fNetStream.Write(aPacket, 0, aPacket.Length);
                if (imbMinimumPacketSize > aPacket.Length)
                {
                    // send filler bytes
                    var fillerBytes = new byte[imbMinimumPacketSize - aPacket.Length];
                    fNetStream.Write(fillerBytes, 0, fillerBytes.Length);
                }
            }
        }
    }

    public class TTLSConnection : TConnection
    {
        public const int imbDefaultTLSRemotePort = 4443;

        public TTLSConnection(
            string aCertFile, string aCertFilePassword, string aRootCertFile,
            bool aStrictCertificateCheck,
            string aModelName, int aModelID = 0,
            string aPrefix = imbDefaultPrefix,
            string aRemoteHost = imbDefaultRemoteHost, int aRemotePort = imbDefaultTLSRemotePort)
            : base(aModelName, aModelID, aPrefix)
        {
            fRemoteHost = aRemoteHost;
            fRemotePort = aRemotePort;

            fStrictCertificateCheck = aStrictCertificateCheck;

            X509Certificate clientCertificate = new X509Certificate2(aCertFile, aCertFilePassword);
            fCertificates.Add(clientCertificate);
            if (!aStrictCertificateCheck)
            {
                X509Certificate rootCertificate = X509Certificate.CreateFromCertFile(aRootCertFile);
                fCertificates.Add(rootCertificate);
            }

            connected = true;
        }

        protected string fRemoteHost;
        protected int fRemotePort;
        protected Thread fReaderThread = null;
        protected TcpClient fClient = null;
        protected SslStream fTLSStream = null;
        bool fStrictCertificateCheck;
        private X509CertificateCollection fCertificates = new X509CertificateCollection();

        protected override bool getConnected()
        {
            return fClient != null;
        }

        protected bool CheckRemoteCertificate(object sender, X509Certificate certificate, X509Chain chain, SslPolicyErrors sslPolicyErrors)
        {
            switch (sslPolicyErrors)
            {
                case SslPolicyErrors.RemoteCertificateNameMismatch:
                    Debug.WriteLine("## Server name mismatch");
                    return false;
                case SslPolicyErrors.RemoteCertificateNotAvailable:
                    Debug.WriteLine("## Server's certificate not available");
                    return false;
                case SslPolicyErrors.RemoteCertificateChainErrors:

                    try
                    {
                        if (chain.ChainStatus[0].Status == System.Security.Cryptography.X509Certificates.X509ChainStatusFlags.UntrustedRoot)
                        {
                            // todo: check if remote root certificate is same as local root certificate?
                            // this did not happen without the host entry in the remote certificate?
                            if (fStrictCertificateCheck)
                            {
                                Debug.WriteLine(">> Server's certificate validation has untrusted root -> failed");
                                return false;
                            }
                            else
                            {
                                Debug.WriteLine(">> Server's certificate validation has untrusted root -> pass");
                                Debug.WriteLine(">> Add root certificate to trusted certificates?");
                                return true;
                            }
                        }
                        else
                        {
                            Debug.WriteLine("## Server's certificate validation failed");
                            return false;
                        }
                    }
                    catch (Exception e)
                    {
                        Debug.WriteLine("## Server's certificate validation failed with exception " + e.Message);
                        return false;
                    }
            }
            Debug.WriteLine("Server's authentication succeeded ...\n");
            return true;
        }

        protected X509Certificate SelectClientCertificate(object sender, string targetHost, X509CertificateCollection localCertificates, X509Certificate remoteCertificate, string[] acceptableIssuers)
        {
            return localCertificates[0]; // return the first certificate
        }

        protected override void setConnected(bool aValue)
        {
            if (aValue)
            {
                if (!connected)
                {
                    try
                    {
                        fClient = new TcpClient(fRemoteHost, fRemotePort);
                        fClient.LingerState.LingerTime = 1;
                        fClient.LingerState.Enabled = true;
                    }
                    catch
                    { } //  catch and ignore all exceptions, just do not connect
                    if (fClient != null)
                    {
                        // TLS part
                        fTLSStream = new SslStream(
                            fClient.GetStream(),
                            false,
                            new RemoteCertificateValidationCallback(CheckRemoteCertificate),
                            new LocalCertificateSelectionCallback(SelectClientCertificate),
                            EncryptionPolicy.RequireEncryption);
                        try
                        {
                            fTLSStream.AuthenticateAsClient(fRemoteHost, fCertificates, SslProtocols.Tls12, false); // true);  //checkCertificateRevocation

                            // start normal reader thread
                            fReaderThread = new Thread(readPackets);
                            fReaderThread.Name = "IMB reader";
                            fReaderThread.Start();
                            // send connect info
                            signalConnectInfo(fModelName, fModelID);
                            // wait for unique client id as a signal that we are connected
                            waitForConnected();
                        }
                        catch (Exception e)
                        {
                            Debug.WriteLine("## IMB TLS authentication exception " + e.Message);
                            fClient.Close();
                            fClient = null; // new TcpClient(); // cannot use old connection so create new one to make later call to OpenLow possible
                            fTLSStream.Close();
                            fTLSStream = null;
                        }
                    }
                }
            }
            else
            {
                if (connected)
                {

                    fClient.Close();
                    fClient = null; // new TcpClient(); // cannot use old connection so create new one to make later call to OpenLow possible
                    fTLSStream.Close();
                    fTLSStream = null;
                }
            }
        }

        protected override int readBytes(byte[] aBuffer, int aOffset, int aLimit)
        {
            if (connected)
            {
                int totBytesReceived = 0;
                int bytesReceived;
                do
                {
                    bytesReceived = fTLSStream.Read(aBuffer, aOffset, aLimit - aOffset);
                    aOffset += bytesReceived;
                    totBytesReceived += bytesReceived;
                }
                while (aLimit - aOffset > 0 && bytesReceived > 0);
                return totBytesReceived;
            }
            else
                return -1;
        }

        public override void writePacket(byte[] aPacket)
        {
            lock (fTLSStream)
            {
                fTLSStream.Write(aPacket, 0, aPacket.Length);
                if (imbMinimumPacketSize > aPacket.Length)
                {
                    // send filler bytes
                    var fillerBytes = new byte[imbMinimumPacketSize - aPacket.Length];
                    fTLSStream.Write(fillerBytes, 0, fillerBytes.Length);
                }
            }
        }
    }
}
