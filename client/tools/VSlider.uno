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

public partial class VSlider : ITool
{
    uint _id = UID.Create();
    public uint ID { get { return _id; } }
    public uint GroupID { get { return 0; } }

    public float4 NormalizedData { get; set; }

    protected override void OnProgressChanged()
    {
        base.OnProgressChanged();
        debug_log "progress=" + Progress;
        var val = 1f-(float)Progress;
        NormalizedData = float4(val, val, val, val);
        Connection.Send(this);
    }
}
