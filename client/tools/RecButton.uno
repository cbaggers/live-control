using Uno;
using Uno.UX;
using Uno.Graphics;

using Fuse;
using Fuse.Input;
using Fuse.Controls;
using Fuse.Controls.Native;
using Fuse.Scripting;
using Fuse.Drawing.Primitives;

using HamHands;

public partial class RecButton : ITool
{
    public uint ID { get { return 0; } }
    public uint GroupID { get { return 0; } }

    public float4 NormalizedData { get; set; }

    void OnPress(object sender, object args)
    {
        var a = (PointerPressedArgs)args;
        a.IsHandled = true;
        NormalizedData = float4(1f, 1f, 1f, 1f);
        debug_log "rec down";
        Connection.Send(this);
    }

    void OnRelease(object sender, object args)
    {
        var a = (PointerReleasedArgs)args;
        a.IsHandled = true;
        NormalizedData = float4(0f, 0f, 0f, 0f);
        debug_log "rec up";
        Connection.Send(this);
    }
}
