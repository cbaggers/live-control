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

        static uint ANNOUNCE_SOURCE_ID = 2^32-1;
        static uint TIME_SYNC_ID = 2^32-2;

        internal static void ReplaceSocket(Socket socket)
        {
            if (_socket != null)
            {
                debug_log "We already have a socket, gotta fix this up";
            }
            else
            {
                _socket = socket;
            }
        }

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
            _socket.Send(extern<byte[]>(_buffer) "@{Uno.Buffer:Of($0)._data}");
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
