using Uno;
using Uno.Graphics;

using Fuse;
using Fuse.Input;
using Fuse.Controls;
using Fuse.Scripting;
using Fuse.Drawing.Primitives;

using HamHands;

public class PanelTool : Control, ITool
{
    float2 _currSize = float2(0,0);
    bool _isPressed = false;

    uint _id = UID.Create();
    public uint ID { get { return _id; } }
    public uint GroupID { get { return 0; } }

    public float4 NormalizedData { get; set; }

    static PanelTool() {
        //Register scriptClass stuff here
    }

    protected override void OnRooted()
    {
        base.OnRooted();
        Pointer.Pressed.AddHandler(this, OnPressed);
        Pointer.Moved.AddHandler(this, OnMoved);
        Pointer.Released.AddHandler(this, OnReleased);
        Placed += OnPlaced;
    }

    protected override void OnUnrooted()
    {
        Pointer.Pressed.RemoveHandler(this, OnPressed);
        Pointer.Moved.RemoveHandler(this, OnMoved);
        Pointer.Released.RemoveHandler(this, OnReleased);
        Placed -= OnPlaced;
        base.OnUnrooted();
    }

    void OnPlaced(object sender, PlacedArgs args)
    {
        _currSize = args.NewSize;
        InvalidateVisual();
        InvalidateRenderBounds();
    }

    void OnPressed(object sender, PointerPressedArgs args)
    {
        args.IsHandled = true;
        _isPressed = true;

        var _localPos = WindowToLocal(args.WindowPoint);
        debug_log _localPos;
        NormalizedData = float4(_localPos.X, _localPos.Y, 0f, 0f);
        Connection.Send(this);

        InvalidateVisual();
    }

    void OnMoved(object sender, PointerMovedArgs args)
    {
        if(_isPressed)
        {
            args.IsHandled = true;

            var _localPos = WindowToLocal(args.WindowPoint);
            debug_log _localPos;
            NormalizedData = float4(_localPos.X, _localPos.Y, 0f, 0f);
            Connection.Send(this);

            InvalidateVisual();
        }
    }

    void OnReleased(object sender, PointerReleasedArgs args)
    {
        args.IsHandled = true;
        _isPressed = false;
    }

    protected override VisualBounds HitTestLocalVisualBounds
    {
        get
        {
            var nb = base.HitTestLocalVisualBounds;
            nb = nb.AddRect( float2(0), ActualSize );
            return nb;
        }
    }

    protected override void OnHitTestLocalVisual(HitTestContext htc)
    {
        if (IsPointInside(htc.LocalPoint))
            htc.Hit(this);

        base.OnHitTestLocalVisual(htc);
    }

    protected override VisualBounds CalcRenderBounds()
    {
        var b = base.CalcRenderBounds();
        b = b.AddRect( float2(0), ActualSize );
        return b;
    }
}
