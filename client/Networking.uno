using Fuse;
using Uno;
using Uno.UX;
using Fuse.Scripting;
using Fuse.Platform;
using Uno.Collections;
using Uno.Compiler.ExportTargetInterop;
using Uno.Net.Sockets;
using Uno.Threading;

namespace HamHands
{
    public static class Connection
    {
        static Socket _socket;

        static int _idSize = sizeof(uint);
        static int _groupIDSize = sizeof(uint);
        static int _dataSize = sizeof(float4);

        static int _bufferSize = _idSize + _groupIDSize + _dataSize;
        static Buffer _buffer = new Buffer(_bufferSize);

        static HashSet<ITool> _tools = new HashSet<ITool>();

        static uint ANNOUNCE_SOURCE_ID = (uint)((2^32)-1);
        static uint TIME_SYNC_ID = (uint)((2^32)-2);

        //--------------------------------
        // Foo

        internal static void ReplaceSocket(Socket socket)
        {
            if (_socket != null)
            {
                debug_log "We already have a socket, gotta fix this up";
            }
            else
            {
                _socket = socket;
                SendAnnounceMessages();
            }
        }

        //--------------------------------
        // Tools

        public static void RegisterTool(ITool tool)
        {
            if (_tools.Contains(tool)) return;

            _tools.Add(tool);

            if (_socket != null)
                SendAnnounceMessage(tool);
        }

        //--------------------------------
        // Communication

        public static void Send(ITool tool)
        {
            Send(tool.GroupID, tool.ID, tool.NormalizedData);
        }

        public static void Send(uint groupID, uint id, float4 data)
        {
            if (_socket==null) return;

            _buffer.Set(0, groupID);
            _buffer.Set(4, id);
            _buffer.Set(8, data);

            try
            {
                _socket.Send(extern<byte[]>(_buffer) "@{Uno.Buffer:Of($0)._data}");
            }
            catch (Exception e)
            {
                _socket = null;
            }
        }

        static void SendAnnounceMessages()
        {
            foreach (var tool in _tools)
                SendAnnounceMessage(tool);
        }

        static void SendAnnounceMessage(ITool tool)
        {
            var sourceID = tool.ID;
            var name = "dummyName";

            var bufferLen = 4 + 4 + 4 + 4 + (4 * name.Length);
            var buffer = new Buffer(bufferLen);

            buffer.Set(0, ANNOUNCE_SOURCE_ID);
            buffer.Set(4, sourceID);
            buffer.Set(8, sourceID);
            buffer.Set(12, name.Length);

            for (var i = 0; i < name.Length; i++) {
                buffer.Set(16+(4*i), (int)name[i]);
            }

            try
            {
                _socket.Send(extern<byte[]>(buffer) "@{Uno.Buffer:Of($0)._data}");
            }
            catch (Exception e)
            {
                _socket = null;
            }
        }
    }

    class ConnectToServer : Promise<bool>
    {
        public ConnectToServer(string ip, int port)
        {
            try
            {
                var socket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
                socket.Connect(ip, port);
                Connection.ReplaceSocket(socket);
                Resolve(true);
            }
            catch (Exception e)
            {
                Reject(e);
            }
        }
    }
}
