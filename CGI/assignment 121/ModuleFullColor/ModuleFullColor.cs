using System;
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Imaging;
using System.Linq;
using _117raster;
using Raster;
using Utilities;

namespace Modules
{
  public class ModuleFullColor : DefaultRasterModule
  {
    /// <summary>
    /// Mandatory plain constructor.
    /// </summary>
    public ModuleFullColor ()
    {
      // Default cell size (wid x hei).
      param = "wid=4096,hei=4096,pic2=5";
    }

    /// <summary>
    /// Author's full name.
    /// </summary>
    public override string Author => "LevyJakub";

    /// <summary>
    /// Name of the module (short enough to fit inside a list-boxes, etc.).
    /// </summary>
    public override string Name => "FullColor";

    /// <summary>
    /// Tooltip for Param (text parameters).
    /// </summary>
    ///
    ///                                                                                             sampleSize pouzit se vstupnim obrazkem
    ///                                                                                             za 1 minutu dobehne default sampleSize=12 (pro 4096x4096 output)
    ///                                                                                             se zvetsujicim sampleSize se zlepsuje kvalita reprodukce
      ///                                                                                           ale i potrebny cas k vypoctu
      /// 
    ///                                                                                             noise,pic1,pic2,pic3,pic4 jsou generovane obrazky bez vstupniho
    ///                                                                                             nejlepe vypadaji asi varianty pic2
    public override string Tooltip => "[wid=<width>][,hei=<height>][,ignore-input][,no-check][sampleSize=<uint>,noise|pic1|(pic2=1|2|3|4|5|6)|pic3|(pic4=1|2|3)]";
    
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
    protected Bitmap inImage;

    /// <summary>
    /// Output raster image.
    /// </summary>
    protected Bitmap outImage;

    /// <summary>
    /// Output message (color check).
    /// </summary>
    protected string message;

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

    private static List<Point> GenPoints (int w, int h)
    {
      List<Point> ret = new List<Point>();
      for (int i = 0; i < w; ++i)
      {
        for (int j = 0; j < h; ++j)
        {
          ret.Add(new Point(j, i));
        }
      }

      return ret;

    }

    private static int HammingWeight (int value)
    {
      value = value - ((value >> 1) & 0x55555555);
      value = (value & 0x33333333) + ((value >> 2) & 0x33333333);
      return (((value + (value >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;
    }

    private static double ColorDistance (Color c1, Color c2)
    {
      return Math.Sqrt((c2.R - c1.R) * 0.3*((c2.R - c1.R) * 0.3) + ((c2.G - c1.G) * 0.59)*((c2.G - c1.G) * 0.59) + ((c2.B - c1.B) * 0.11)*((c2.B - c1.B) * 0.11));
    }

    /// <summary>
    /// Recompute the output image[s] according to input image[s].
    /// Blocking (synchronous) function.
    /// #GetOutput() functions can be called after that.
    /// </summary>
    public override void Update ()
    {
      // Input image is optional.
      // Starts a new computation.
      UserBreak = false;

      // Default values.
      int wid = 4096;
      int hei = 4096;
      bool ignoreInput = false;
      bool check = true;
      bool randomPic = false;
      bool pic1 = false;
      bool pic2 = true;
      int pic2SortMode = 5;
      bool pic3 = false;
      bool pic4 = false;
      int pic4SortMode = 1;
      int sampleSize = 12;

      // We are not using 'paramDirty', so the 'Param' string has to be parsed every time.
      Dictionary<string, string> p = Util.ParseKeyValueList(param);
      if (p.Count > 0)
      {
        // wid=<int> [image width in pixels]
        if (Util.TryParse(p, "wid", ref wid))
        {
          wid = Math.Max(4096, wid);
        }

        // hei=<int> [image height in pixels]
        if (Util.TryParse(p, "hei", ref hei))
        {
          hei = Math.Max(4096, hei);
        }

        // ignore-input ... ignore input image even if it is present
        ignoreInput = p.ContainsKey("ignore-input");

        // no-check ... disable color check at the end
        check = !p.ContainsKey("no-check");

        randomPic = p.ContainsKey("noise");
        pic1 = p.ContainsKey("pic1");

        if(Util.TryParse(p, "pic2", ref pic2SortMode))
        {
          pic2 = true;
          pic2SortMode = Math.Max(1, pic2SortMode);
          pic2SortMode = Math.Min(pic2SortMode, 6);
        }
        else
        {
          pic2 = false;
        }

        pic3 = p.ContainsKey("pic3");

        if (Util.TryParse(p, "pic4", ref pic4SortMode))
        {
          pic4 = true;
          pic4SortMode = Math.Max(1, pic4SortMode);
          pic4SortMode = Math.Min(pic4SortMode, 3);
        }
        else
        {
          pic4 = false;
        }

        if (Util.TryParse(p, "sampleSize", ref sampleSize))
        {
          sampleSize = Math.Min(sampleSize, 1000000);
          sampleSize = Math.Max(sampleSize, 1);
        }
      }

      outImage = new Bitmap(wid, hei, PixelFormat.Format24bppRgb);

      // Generate full-color image.
      int xo, yo;
      if (!ignoreInput &&
          inImage != null)
      {
        // Input image is present => use it.

        // Convert pixel data (fast memory-mapped code).
        PixelFormat iFormat = inImage.PixelFormat;
        if (!PixelFormat.Format24bppRgb.Equals(iFormat) &&
            !PixelFormat.Format32bppArgb.Equals(iFormat) &&
            !PixelFormat.Format32bppPArgb.Equals(iFormat) &&
            !PixelFormat.Format32bppRgb.Equals(iFormat))
        {
          iFormat = PixelFormat.Format24bppRgb;
        }

        int width = inImage.Width;
        int height = inImage.Height;
        int xi, yi;
        BitmapData dataIn = inImage.LockBits(new Rectangle(0, 0, width, height), ImageLockMode.ReadOnly, iFormat);
        BitmapData dataOut =
          outImage.LockBits(new Rectangle(0, 0, wid, hei), ImageLockMode.WriteOnly, outImage.PixelFormat);
        unsafe
        {
          byte* iptr, optr;
          byte ri, gi, bi;
          int dI = Image.GetPixelFormatSize(iFormat) / 8; // pixel size in bytes
          int dO = Image.GetPixelFormatSize(outImage.PixelFormat) / 8; // pixel size in bytes

          yi = 0;
          
          List<Color> colors = GenAllColors();

          for (yo = 0; yo < hei; yo++)
          {
            // User break handling.
            if (UserBreak)
            {
              break;
            }

            iptr = (byte*)dataIn.Scan0 + yi * dataIn.Stride;
            optr = (byte*)dataOut.Scan0 + yo * dataOut.Stride;

            xi = 0;
            
            for (xo = 0; xo < wid; xo++)
            {
              // read input colors
              bi = iptr[0];
              gi = iptr[1];
              ri = iptr[2];

              Color c = Color.FromArgb(ri, gi, bi);
              Color newC = c;
              if (colors.Count > 0)
              {
                List<int> idxs = colors.RandomIdxSample(sampleSize);

                int closestIdx = idxs.OrderBy(i => ColorDistance(colors[i], c)).First();
                newC = colors[closestIdx];
                colors.FastRemoveAt(closestIdx);
              }
              // write output colors
              optr[0] = newC.B;
              optr[1] = newC.G;
              optr[2] = newC.R;

              iptr += dI;
              optr += dO;
              if (++xi >= width)
              {
                xi = 0;
                iptr = (byte*)dataIn.Scan0 + yi * dataIn.Stride;
              }
            }

            if (++yi >= height)
            {
              yi = 0;
            }
          }
        }

        outImage.UnlockBits(dataOut);
        inImage.UnlockBits(dataIn);
      }
      else
      {
        // No input => generate constant full-color image.

        
          // Generate pixel data (fast memory-mapped code).

          BitmapData dataOut = outImage.LockBits(new Rectangle(0, 0, wid, hei), ImageLockMode.WriteOnly,
            outImage.PixelFormat);
          unsafe
          {
            byte* optr;
            int dO = Image.GetPixelFormatSize(outImage.PixelFormat) / 8; // pixel size in bytes

            if (randomPic)
            {
              Queue<Color> colors = new Queue<Color>(GenColors(wid * hei, true));
              for (yo = 0; yo < hei; yo++)
              {
                // User break handling.
                if (UserBreak)
                {
                  break;
                }

                optr = (byte*)dataOut.Scan0 + yo * dataOut.Stride;

                for (xo = 0; xo < wid; xo++)
                {
                  Color c = colors.Dequeue();

                  optr[0] = c.B;
                  optr[1] = c.G;
                  optr[2] = c.R;

                  optr += dO;
                }
              }
            }
            else if(pic1)
            {
              int b = -1;
              int g = 0;
              for (yo = 0; yo < hei; yo++)
              {
                if (UserBreak)
                {
                  break;
                }

                optr = (byte*)dataOut.Scan0 + yo * dataOut.Stride;
                if (yo == 0)
                {
                  g = -1;
                }

                for (xo = 0; xo < wid; xo++)
                {
                  int r = xo % 256;

                  if (r == 0)
                  {
                    g = (g + 1) % 256;
                  }

                  if (r == 0 && g == 0)
                  {
                    b = (b + 1) % 256;
                  }
                  

                  optr[0] = (byte)r;
                  optr[1] = (byte)b;
                  optr[2] = (byte)g;
                  optr += dO;

                }
              }
            }
            else if (pic2)
            {
              List<Point> pts = GenPoints(wid, hei);
              pts.Sort((p1, p2) =>
              {

                int cmp = 0;

                if (pic2SortMode == 1)
                {
                  cmp = (HammingWeight(p1.X) + HammingWeight(p1.Y)) - (HammingWeight(p2.X) + HammingWeight(p2.Y));
                }


                else if (pic2SortMode == 2)
                {
                  cmp = (p1.X + p1.Y) - (p2.X + p2.Y);
                }

                else if (pic2SortMode == 3)
                {
                  cmp = HammingWeight(p1.X | p1.Y) - HammingWeight(p2.X | p2.Y);
                }

                else if (pic2SortMode == 4)
                {
                  cmp = HammingWeight(p1.X & p1.Y) - HammingWeight(p2.X & p2.Y);
                }

                else if (pic2SortMode == 5)
                {
                  cmp = HammingWeight(p1.X ^ p1.Y) - HammingWeight(p2.X ^ p2.Y);
                }

                else if (pic2SortMode == 6)
                {
                  cmp = HammingWeight(p1.X + p1.Y) - HammingWeight(p2.X + p2.Y);
                }


                if (cmp < 0)
                {
                  return -1;
                }
                if (cmp == 0)
                {
                  return 0;
                }

                return 1;
              });

              int idx = 0;
              for (int r = 0; r < 256; r++)
              {
                for (int g = 0; g < 256; g++)
                {

                  if (UserBreak)
                  {
                    break;
                  }

                  for (int b = 0; b < 256; b++)
                  {
                    Point pt = pts[idx];

                    optr = (byte*)dataOut.Scan0 + pt.Y * dataOut.Stride;
                    optr += pt.X * dO;
                    optr[0] = (byte)b;
                    optr[1] = (byte)g;
                    optr[2] = (byte)r;
                    idx++;
                  }
                }
              }
            }
            else if (pic3)
            {
              for (int r = 0; r < 256; r++)
              {
                for (int g = 0; g < 256; g++)
                {
                  if (UserBreak)
                  {
                    break;
                  }

                  for (int b = 0; b < 256; b++)
                  {
                    int x = ((r & 15) << 8) | g;
                    int y = ((r >> 4) << 8) | b;

                    optr = (byte*)dataOut.Scan0 + y * dataOut.Stride;
                    optr += x * dO;

                    optr[0] = (byte)b;
                    optr[1] = (byte)g;
                    optr[2] = (byte)r;
                  }
                }
              }

            }
            else if (pic4)
            {
              List<Color> colors = GenColors(wid * hei, false);


              colors.Sort((c1, c2) =>
              {
                int cmp = 0;
                if (pic4SortMode == 1)
                {
                  cmp = (HammingWeight(c1.R) + HammingWeight(c1.G) + HammingWeight(c1.B)) -
                         (HammingWeight(c2.R) + HammingWeight(c2.G) + HammingWeight(c2.B));
                }
                else if (pic4SortMode == 2)
                {
                  cmp = HammingWeight(c1.R ^ c1.G ^ c1.B) -
                        HammingWeight(c2.R ^ c2.G ^ c2.B);
                }

                else if (pic4SortMode == 3)
                {
                  cmp = Convert.ToInt32(c1.R < c2.R && c1.G > c2.G && c1.B == c2.B);
                }

                if (cmp < 0)
                {
                  return -1;
                }
                if (cmp == 0)
                {
                  return 0;
                }

                return 1;
              });
              int idx = 0;
              for (int y = 0; y < hei; y++)
              {

                if (UserBreak)
                {
                  break;
                }

                optr = (byte*)dataOut.Scan0 + y * dataOut.Stride;
                for (int x = 0; x < wid; x++)
                {
                  Color c = colors[idx];
                  optr[0] = c.B;
                  optr[1] = c.G;
                  optr[2] = c.R;

                  optr += dO;
                  ++idx;
                }
              } 
            }
          }
          outImage.UnlockBits(dataOut);
      }

      // Output message.
      if (check &&
          !UserBreak)
      {
        long colors = Draw.ColorNumber(outImage);
        message = colors == (1 << 24) ? "Colors: 16M, Ok" : $"Colors: {colors}, Fail";
      }
    }

    private List<Color> GenColors (int pixels, bool randomShuffle)
    {
      Random rnd = new Random();
      List<Color> retColors = new List<Color>(pixels);
      for (int r = 0; r < 256; ++r)
      {
        for (int g = 0; g < 256; ++g)
        {
          for (int b = 0; b < 256; ++b)
          {
            retColors.Add(Color.FromArgb(r,g,b));
          }
        }
      }

      int rest = (int) (pixels - Math.Pow(256, 3));
      for (int i = 0; i < rest; ++i)
      {
        retColors.Add(Color.FromArgb(rnd.Next(0, 256), rnd.Next(0, 256), rnd.Next(0, 256)));
      }

      if(randomShuffle)
      {
        return retColors.RandomShuffle().ToList();
      }

      return retColors;
    }

    private List<Color> GenAllColors ()
    {
      List<Color> ret = new List<Color>(256*256*256);
      for (int r = 0; r < 256; ++r)
      {
        for (int g = 0; g < 256; ++g)
        {
          for (int b = 0; b < 256; ++b)
          {
            ret.Add(Color.FromArgb(r, g, b));
          }
        }
      }
      return ret;
    }

    /// <summary>
    /// Returns an output raster image.
    /// Can return null.
    /// </summary>
    /// <param name="slot">Slot number from 0 to OutputSlots-1.</param>
    public override Bitmap GetOutput (
      int slot = 0) => outImage;

    /// <summary>
    /// Returns an optional output message.
    /// Can return null.
    /// </summary>
    /// <param name="slot">Slot number from 0 to OutputSlots-1.</param>
    public override string GetOutputMessage (
      int slot = 0) => message;
  }
}
