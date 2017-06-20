using Fuse;
using Uno;
using Uno.UX;
using Fuse.Scripting;
using Fuse.Platform;
using Uno.Collections;
using Uno.Compiler.ExportTargetInterop;
using Uno.Threading;

namespace HamHands
{
    [UXGlobalModule]
    public class HamHandsModule : NativeEventEmitterModule
    {
        static HamHandsModule _instance;

        public HamHandsModule(): base(true, "lostConnection")
        {
            if (_instance != null) return;
            Uno.UX.Resource.SetGlobalKey(_instance = this, "HamHands");

            AddMember(new NativePromise<bool, bool>("connect", Connect));

            var lostConnection = new NativeEvent("lostConnection");
            On("lostConnection", lostConnection);
            AddMember(lostConnection);
            Connection.Lost += OnLostConnection;
        }

        static Future<bool> Connect(object[] args)
        {
            var ip = (string)args[0];
            var port = Int.Parse((string)args[1]);
            return new ConnectToServer(ip, port);
        }

        void OnLostConnection()
        {
            Emit("lostConnection");
        }
    }
}
