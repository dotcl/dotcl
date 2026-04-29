using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;

internal sealed class CsharpSanityGame : Game
{
    private readonly GraphicsDeviceManager _graphics;

    public CsharpSanityGame()
    {
        _graphics = new GraphicsDeviceManager(this);
    }

    protected override void Draw(GameTime gameTime)
    {
        GraphicsDevice.Clear(new Color(255, 0, 0));
    }
}
