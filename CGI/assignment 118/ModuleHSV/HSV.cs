using System.Drawing;
using MathSupport;
using Raster;
using System;
using System.Collections.Generic;
using System.Drawing.Drawing2D;
using System.Linq;
using System.Net;
using System.Runtime.CompilerServices;
using System.Text.RegularExpressions;
using _117raster.ModuleHSV;
using Modules;

namespace Utilities
{
  public class PictureInfo
  {
    public HSVData Hsv { get; set; }
    public Color Rgb { get; set; }
    public PointF Location { get; set; }
    public Point OriginalLocation { get; set; }

  }
  public struct HSVData
  {
    public double H { get; set; }
    public double S { get; set; }
    public double V { get; set; }
  }

  public class HSV
  {
    public static bool hueOff = false;
    public static List<PictureInfo> pis = new List<PictureInfo>();
    public static void Compute (Bitmap bmp, string param)
    {
      pis.Clear();
      string p = param.ToLower();
      hueOff = p.IndexOf("hue-off") != -1;


      for (int y = 0; y < bmp.Height; y++)
      {
        for (int x = 0; x < bmp.Width; x++)
        {
          Color c = bmp.GetPixel(x, y);
          pis.Add(new PictureInfo{Rgb = c, OriginalLocation = new Point(x, y), Hsv = Rgb2Hsv(c)});
        }
      }
    }

    private static PointF ToGraphCoord (HSVData hsv, Bitmap bmp)
    {
      float x = (float) (bmp.Width * 0.05f + bmp.Width * 0.9f / 100f * hsv.V);
      float y = (float) (bmp.Height * 0.95f + -bmp.Height * 0.9f / 100f * hsv.S);
      return new PointF(x, y);
    }

    public static HSVData Rgb2Hsv (Color rgb)
    {
      double h, s, v;
      Arith.ColorToHSV(rgb, out h, out s, out v);
      HSVData hd = new HSVData {H = h, S = s*100, V = v*100};
      return hd;
    }

    public static void DrawImage (Bitmap graph)
    {

      using (Graphics g = Graphics.FromImage(graph))
      {
        g.SmoothingMode = SmoothingMode.AntiAlias;
        g.Clear(Color.White);

        Pen axisPen = new Pen(Color.Black);
        Font smallest = new Font(FontFamily.GenericSansSerif, 10);
        Font smallestB = new Font(FontFamily.GenericSansSerif, 10, FontStyle.Bold);
        StringFormat sfCenter = new StringFormat {Alignment = StringAlignment.Center};


        float x0 = graph.Width * 0.05f;
        float y0 = graph.Height * 0.95f;
        float kx = graph.Width * 0.9f / 100f;
        float ky = -graph.Height * 0.9f / 100f;
        float max = 100f;


        g.DrawLine(axisPen, x0, y0, x0 + 100f * kx, y0);
        g.DrawLine(axisPen, x0, y0, x0, y0 + max * ky);

        g.DrawString("V", smallest, Brushes.Black, x0 + 100f * kx + 15f, y0 - 15f, sfCenter);
        g.DrawString("S", smallest, Brushes.Black, x0, y0 + max * ky - 23f, sfCenter);



        foreach (PictureInfo pi in pis)
        {
          pi.Location = ToGraphCoord(pi.Hsv, graph);

          if (hueOff)
          {
            g.FillCircle(new SolidBrush(pi.Rgb), pi.Location, Math.Min(kx, Math.Abs(ky))/1.5f);
          }
          else
          {
            g.DrawArrow(new Pen(pi.Rgb), pi.Location, Math.Min(kx,Math.Abs(ky)), pi.Hsv.H);
          }
        }
        




        RectangleF lastBox = RectangleF.Empty;
        RectangleF currTextPos = RectangleF.Empty;
        RectangleF lastBox2 = RectangleF.Empty;



        lastBox = RectangleF.Empty;
        currTextPos = RectangleF.Empty;
        lastBox2 = new RectangleF(new PointF((x0 + 99 * kx + (x0 + 99f * kx + kx)) / 2f, y0), g.MeasureString(max.ToString(), smallest));
        g.DrawString(max.ToString(), smallestB, Brushes.Black, lastBox2.X, lastBox2.Y, sfCenter);

        for (int x = 0; x < 100; x++)
        {
          currTextPos = new RectangleF(new PointF((x0 + x * kx + (x0 + x * kx + kx)) / 2f, y0),
            g.MeasureString(x.ToString(), smallest));

          if (RectangleF.Intersect(currTextPos, lastBox) == RectangleF.Empty &&
              RectangleF.Intersect(currTextPos, lastBox2) == RectangleF.Empty)
          {
            g.DrawString(x.ToString(), smallest, Brushes.Black, currTextPos.X, currTextPos.Y, sfCenter);
            lastBox = currTextPos;
          }

        }

        SizeF velikost = g.MeasureString(max.ToString(), smallest);
        lastBox = new RectangleF(new PointF(x0, y0), g.MeasureString("0", smallest));
        lastBox2 = new RectangleF(new PointF(x0, y0 + max * ky), velikost);
        g.DrawString(max.ToString(), smallestB, Brushes.Black, lastBox2.X, lastBox2.Y - velikost.Height / 2f,
          new StringFormat { Alignment = StringAlignment.Far });

        for (int cetnost = 0; cetnost <= max; ++cetnost)
        {
          velikost = g.MeasureString(cetnost.ToString(), smallest);
          currTextPos = new RectangleF(new PointF(x0, y0 + cetnost * ky), velikost);
          if (RectangleF.Intersect(currTextPos, lastBox) == RectangleF.Empty &&
              RectangleF.Intersect(currTextPos, lastBox2) == RectangleF.Empty)
          {
            g.DrawString(cetnost.ToString(), smallest, Brushes.Black, currTextPos.X, currTextPos.Y - velikost.Height / 2f, new StringFormat { Alignment = StringAlignment.Far });
            lastBox = currTextPos;
          }
        }


      }
    }
  }
}
