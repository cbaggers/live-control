using Uno;
using Uno.Graphics;

using Fuse;
using Fuse.Input;
using Fuse.Controls;
using Fuse.Scripting;
using Fuse.Drawing.Primitives;

public class UID
{
    static uint lastUID = 0;
    public static uint Create() { return lastUID += 1; }
}

public interface Identifiable
{
    uint ID { get; }
    uint GroupID { get; }
}

public interface ITool : Identifiable
{
    float4 NormalizedData { get; }
}
