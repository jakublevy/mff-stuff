using System;
using System.Collections.Generic;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace _117raster.ModuleHSV
{
  static class ExtensionMethods
  {
    public static void DrawArrow (this Graphics g, Pen p, PointF loc, float lineLength, double angleDeg)
    {
      PointF v = new PointF(0, -lineLength);
      PointF z = RotateVectorAC(v, 25 + angleDeg);
      PointF d = RotateVectorAC(v, -25 + angleDeg);

      PointF f1 = PointF.Subtract(loc, new SizeF(z));
      PointF f2 = PointF.Subtract(loc, new SizeF(d));

      g.DrawLine(p, loc, f1);
      g.DrawLine(p, loc, f2);
    }

    public static void DrawCircle (this Graphics g, Pen p, PointF center, float radius)
    {
      PointF topLeft = new PointF(center.X  - radius, center.Y - radius);
      g.DrawEllipse(p, new RectangleF(topLeft, new SizeF(radius, radius)));
    }

    public static void FillCircle (this Graphics g, Brush b, PointF center, float radius)
    {
      PointF topLeft = new PointF(center.X  - radius, center.Y - radius);
      g.FillEllipse(b, new RectangleF(topLeft, new SizeF(radius, radius)));
    }

    public static PointF RotateVectorAC(PointF vec, double angleDeg)
    {
      double angleRad = angleDeg * Math.PI / 180.0;

      return new PointF((float) (vec.X*Math.Cos(angleRad) - vec.Y*Math.Sin(angleRad)),
                          (float) (vec.X*Math.Sin(angleRad) + vec.Y*Math.Cos(angleRad)));
    }

    public static float Distance (PointF pt1, PointF pt2)
    {
      double d1 = Math.Abs(pt1.X - pt2.X);
      double d2 = Math.Abs(pt1.Y - pt2.Y);
      return (float)Math.Sqrt(d1*d1 + d2*d2);
    }
  }
}
