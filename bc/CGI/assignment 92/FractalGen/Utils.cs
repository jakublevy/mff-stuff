using System;
using System.Drawing;
using System.Windows.Forms;

namespace JakubLevy
{
  public static class Utils
  {
    public static void ShowErrorMessageBox (string message)
    {
      MessageBox.Show(message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
    }

    public static PointF ScaleVector (PointF p, double n)
    {
      return new PointF((float)(p.X * n), (float)(p.Y * n));
    }

    public static PointF AddVectors (PointF p1, PointF p2)
    {
      return new PointF(p1.X + p2.X, p1.Y + p2.Y);
    }

    public static PointF RotateVector (Point direction, double rotationAngleD)
    {
      double rotationAngleRad = rotationAngleD * Math.PI / 180.0;
      PointF rotatedDirection =
        new PointF((float)(direction.X * Math.Cos(rotationAngleRad) + direction.Y * Math.Sin(rotationAngleRad)),
          (float)(-direction.X * Math.Sin(rotationAngleRad) + direction.Y * Math.Cos(rotationAngleRad)));
      return rotatedDirection;
    }
  }
}
