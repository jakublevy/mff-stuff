using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Drawing.Imaging;
using System.Linq;
using Modules;

namespace JakubLevy
{
  public class ModuleHSV : DefaultRasterModule
  {
    /// <summary>
    /// Mandatory plain constructor.
    /// </summary>
    public ModuleHSV ()
    {
      /*
        Pokus o napodobení neoimpresionistické technicky zvané pointilismus (https://en.wikipedia.org/wiki/Pointillism).
        Měl jsem obrovské problémy s obyčejným C# Random. Po nahrazení kryptograficky bezpečným randomem šum z obrázků zmizel.
      */
    }

    /// <summary>
    /// Author's full name.
    /// </summary>
    public override string Author => "LevyJakub";

    /// <summary>
    /// Name of the module (short enough to fit inside a list-boxes, etc.).
    /// </summary>
    public override string Name => "ArtSim";

    /// <summary>
    /// Tooltip for Param (text parameters).
    /// </summary>
    public override string Tooltip => "Sem nic zajímavého nepříjde, modul se konfiguruje přes PropertyGrid.";

    Params p = new Params();

    /// <summary>
    /// Usually read-only, optionally writable (client is defining number of inputs).
    /// </summary>
    public override int InputSlots => 1;

    /// <summary>
    /// Usually read-only, optionally writable (client is defining number of outputs).
    /// </summary>
    public override int OutputSlots => 1;

    /// <summary>
    /// Input raster image.
    /// </summary>
    protected Bitmap inImage = null;

    /// <summary>
    /// Output raster image.
    /// </summary>
    protected Bitmap outImage = null;

    /// <summary>
    /// Active HSV form.
    /// </summary>
    protected FormArtSim ArtSimForm = null;

    protected void updateParam ()
    {
      if (paramDirty)
      {
        paramDirty = false;

        // 'param' parsing.
      }

      formUpdate();
    }

    /// <summary>
    /// Send ModuleHSV values to the form elements.
    /// </summary>
    protected void formUpdate ()
    {
      if (ArtSimForm == null)
        return;



      ArtSimForm.Invalidate();
    }

    /// <summary>
    /// Notification: GUI window changed its values (sync GUI -> module is needed).
    /// </summary>
    public override void OnGuiWindowChanged ()
    {
      if (ArtSimForm == null)
        return;

      p = (Params) ArtSimForm.paramsPropertyGrid.SelectedObject;

     

      paramDirty = false;

      ParamUpdated?.Invoke(this);
    }

    /// <summary>
    /// Notification: GUI window has been closed.
    /// </summary>
    public override void OnGuiWindowClose ()
    {
      ArtSimForm?.Hide();
      ArtSimForm = null;
    }

    /// <summary>
    /// Assigns an input raster image to the given slot.
    /// Doesn't start computation (see #Update for this).
    /// </summary>
    /// <param name="inputImage">Input raster image (can be null).</param>
    /// <param name="slot">Slot number from 0 to InputSlots-1.</param>
    public override void SetInput (
      Bitmap inputImage,
      int slot = 0)
    {
      inImage = inputImage;
    }

    /// <summary>
    /// Recompute the output image[s] according to input image[s].
    /// Blocking (synchronous) function.
    /// #GetOutput() functions can be called after that.
    /// </summary>
    public override void Update ()
    {
      if (inImage == null)
        return;

      // Update module values from 'param' string.
      updateParam();

      int wid = inImage.Width;
      int hei = inImage.Height;
      PixelFormat iFormat = inImage.PixelFormat;

      //for k-means
      Dictionary<Color, List<Color>> clusters;
      HashSet<Color> colors = new HashSet<Color>();

      BitmapData dataIn = inImage.LockBits(new Rectangle(0, 0, wid, hei), ImageLockMode.ReadOnly, iFormat);
      

      int dI = Image.GetPixelFormatSize(iFormat) / 8; // pixel size in bytes
      unsafe
      {
        byte* iptr;

        //harvest all colors from image
        for (int yi = 0; yi < hei; yi++)
        {
          iptr = (byte*)dataIn.Scan0 + yi * dataIn.Stride;

          for (int xi = 0; xi < wid; xi++)
          {
            colors.Add(Color.FromArgb(iptr[2], iptr[1], iptr[0]));
            iptr += dI;
          }
        }
        inImage.UnlockBits(dataIn);
      }

      //Calculate K-means
      clusters = Utils.KMeans(p.K, colors.ToList(), p.Iterations);

      //Dots' color
      List<Color> usableColors = Utils.ExtractColors(clusters, p.ColorFromClusterCount);

      outImage = inImage.Clone(new RectangleF(0, 0, inImage.Width, inImage.Height), inImage.PixelFormat);
      using (Graphics g = Graphics.FromImage(outImage))
      {
        int step = (int)p.DotSizeMax > 0 ? (int)p.DotSizeMax : 1;
        g.SmoothingMode = SmoothingMode.AntiAlias;

        dataIn = inImage.LockBits(new Rectangle(0, 0, wid, hei), ImageLockMode.ReadOnly, iFormat);
        unsafe
        {
          byte* iptr;
          for (int i = 0; i < p.FilterIterations; ++i)
          {
            for (int yo = 0; yo < hei; yo += step)
            {
              iptr = (byte*)dataIn.Scan0 + yo * dataIn.Stride;
              for (int xo = 0; xo < wid; xo += step)
              {
                double s = Utils.NextDouble();
                if (s <= p.PutDotProbability)
                {
                  Color c = Color.FromArgb(iptr[2], iptr[1], iptr[0]);
                  List<double> sm = Utils.Softmin(c, usableColors, p.SoftminSoftness);
                  int idx = Utils.GenRandomFromDist(sm);
                  Color n = usableColors[idx];

                  double rad = Utils.NextDouble(p.DotSizeMin, p.DotSizeMax);
                  g.FillCircle(new SolidBrush(n), new PointF(xo, yo), (float)rad);
                }
                iptr += dI * step;
              }
            }
          }
          inImage.UnlockBits(dataIn);
        }
      }
    }

    /// <summary>
    /// Returns an output raster image.
    /// Can return null.
    /// </summary>
    /// <param name="slot">Slot number from 0 to OutputSlots-1.</param>
    public override Bitmap GetOutput (
      int slot = 0) => outImage;

    /// <summary>
    /// Returns true if there is an active GUI window associted with this module.
    /// You can open/close GUI window using the setter.
    /// </summary>
    public override bool GuiWindow
    {
      get => ArtSimForm != null;
      set
      {
        if (value)
        {
          // Show GUI window.
          if (ArtSimForm == null)
          {
            ArtSimForm = new FormArtSim(this);
            formUpdate();
            ArtSimForm.Show();
          }
        }
        else
        {
          ArtSimForm?.Hide();
          ArtSimForm = null;
        }
      }
    }
  }
}
