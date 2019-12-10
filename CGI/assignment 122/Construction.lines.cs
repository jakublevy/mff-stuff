using System;
using System.Collections.Generic;
using System.Drawing;
using OpenTK;
using Utilities;

namespace Scene3D
{
  public class Construction
  {
    #region Form initialization

    /// <summary>
    /// Optional form-data initialization.
    /// </summary>
    /// <param name="name">Return your full name.</param>
    /// <param name="param">Optional text to initialize the form's text-field.</param>
    /// <param name="tooltip">Optional tooltip = param help.</param>
    public static void InitParams (out string name, out string param, out string tooltip)
    {
      name    = "Jakub Levý";
      param   = "pic=2";
      tooltip = "pic=[1|2|3|4|5] r=<radius>, t=<step>, seg=<segments>, density=<of winding for pic 1,3,4>, (a,b,c,d,e,f=<parameter for pic 5>), kx,dx,ky,dy,kz,dz .. frequencies and phase shifts";
    }

    #endregion

    #region Instance data

    // !!! If you need any instance data, put them here..

    private float radius = 1.0f;
    private double kx = 1.0;
    private double dx = 0.0;
    private double ky = 1.0;
    private double dy = 0.0;
    private double kz = 1.0;
    private double dz = 0.0;
    private int segments = 1000;
    private double maxT = 2.0 * Math.PI;
    private int pic = 1;

    //parameters for pic 5
    private double a = -0.1;
    private double b = 8;
    private double cc = 1.5;
    private double d = 6;
    private double e = 3.1;
    private double f = 2;

    //toroid winding (parameter for pic 3,4)
    private double winding = 128.0;

    private void parseParams (string param)
    {
      // Defaults.
      radius   = 1.0f;
      kx       = 1.0;
      dx       = 0.0;
      ky       = 1.0;
      dy       = 0.0;
      kz       = 1.0;
      dz       = 0.0;
      segments = 1000;
      pic = 1;

      Dictionary<string, string> p = Util.ParseKeyValueList(param);
      if (p.Count > 0)
      {

        Util.TryParse(p, "pic", ref pic);


        Util.TryParseRational(p, "kx", ref kx);
        Util.TryParseRational(p, "ky", ref ky);
        Util.TryParseRational(p, "kz", ref kz);

        //spring
        //pic=1, density=1, r=5, t=50, seg=1000, period=10.0, rad=2.0
        //pic=1, density=3, r=5, t=50, seg=1500, period=10.0, rad=2.0
        if (pic == 1)
        {
          radius = 5;
          maxT = 50;
          winding = 1;

        }

        //lorenz attractor
        //pic=2, r=28, t=38, seg=5000, period=10.0, rad=2.0
        //pic=2, r=99.96, t=28, seg=5000, period=10.0, rad=2.0
        else if (pic == 2)
        {
          radius = 28;
          segments = 5000;
          maxT = 38;
        }

        //toroid
        //pic=3, t=6.28, seg=5000, density=128, period=10.0, rad=2.0
        //pic=3, t=4.18, seg=5000, density=64, period=10.0, rad=2.0
        //pic=3, t=3.14, seg=100000, density=7000, period=10.0, rad=2.0
        else if (pic == 3)
        {
          winding = 128.0;
          segments = 5000;
        }

        //trigonometric tree
        //pic=4, t=100, seg=10000, density=6, period=10.0, rad=2.0
        else if (pic == 4)
        {
          winding = 8;
          maxT = 100;

        }

        //knot
        //pic=5, seg=5000, t=10, a=-0.1, b=8, c=1.5, d=6, e=3.1, f=2, period=10.0, rad=2.0
        //pic=5, seg=2000, t=10, a=0.4, b=7, c=1.2, d=6, e=1.3, f=0.4, period=10.0, rad=2.0
        //pic=5, seg=2000, t=10, a=-0.5, b=4, c=0.2, d=3, e=0, f=1.3, period=10.0, rad=2.0
        //pic=5, seg=2000, t=10, a=1.8, b=5, c=0.7, d=10, e=3.3, f=1.3, period=10.0, rad=2.0
        //pic=5, seg=2000, t=10, a=-0.7, b=8, c=1.4, d=3, e=5, f=1.4, period=10.0, rad=2.0
        //pic=5, seg=2000, t=10, a=-0.3, b=1, c=0.3, d=3, e=5, f=1.4, period=10.0, rad=2.0
        //pic=5, seg=2000, t=10, a=-1.1, b=1, c=0.5, d=3, e=3.5, f=2, period=10.0, rad=2.0
        else if (pic == 5)
        {
          maxT = 10;
        }

        // r=<double>
        Util.TryParse(p, "r", ref radius);

        // seg=<int>
        if (Util.TryParse(p, "seg", ref segments) &&
            segments < 10)
          segments = 10;

        // kx,dx,ky,dy,kz,dz .. frequencies and phase shifts.
        
        Util.TryParseRational(p, "dx", ref dx);
        
        Util.TryParseRational(p, "dy", ref dy);
        
        Util.TryParseRational(p, "dz", ref dz);

        //parameter for pic 3,4
        Util.TryParseRational(p, "density", ref winding);

        //parameters for pic 5
        Util.TryParseRational(p, "a", ref a);
        Util.TryParseRational(p, "b", ref b);
        Util.TryParseRational(p, "c", ref cc);
        Util.TryParseRational(p, "d", ref d);
        Util.TryParseRational(p, "e", ref e);
        Util.TryParseRational(p, "f", ref f);

        Util.TryParseRational(p, "t", ref maxT);

        // ... you can add more parameters here ...
      }

    }

    #endregion

    public Construction ()
    {
      // {{

      // }}
    }

    #region Mesh construction

    /// <summary>
    /// Construct a new Brep solid (preferebaly closed = regular one).
    /// </summary>
    /// <param name="scene">B-rep scene to be modified</param>
    /// <param name="m">Transform matrix (object-space to world-space)</param>
    /// <param name="param">Shape parameters if needed</param>
    /// <returns>Number of generated faces (0 in case of failure)</returns>
    public int AddMesh (SceneBrep scene, Matrix4 m, string param)
    {
      parseParams(param);

      // If there will be large number of new vertices, reserve space for them to save time.
      scene.Reserve(segments + 1);

      double t = 0.0;
      double dt = maxT / segments;
      double s = 0.0;       // for both texture coordinate & color ramp
      double ds = 1.0 / segments;

      int vPrev = 0;
      Vector3 A = Vector3.One;
      Vector3 prevVec = Vector3.One;

      if (pic == 2)
      {
        prevVec = new Vector3(0f,1f,1.05f);
      }

      for (int i = 0; i <= segments; i++)
      {
        //spring
        if (pic == 1)
        {
          A.X = (float)(radius * Math.Sin(winding*t));
          A.Y = (float)(radius * Math.Cos(winding*t));
          A.Z = (float)t;
        }

      //lorenz attractor
      //https://en.wikipedia.org/wiki/Lorenz_system
        else if (pic == 2)
        {
          Vector3 der = Lorenz(prevVec.X, prevVec.Y, prevVec.Z, radius);
          A.X = (float) (prevVec.X + (der.X * dt));
          A.Y = (float)(prevVec.Y + (der.Y * dt));
          A.Z = (float)(prevVec.Z + (der.Z * dt));
        }

        //toroid
        else if (pic == 3)
        {
          A.X = (float)(radius * ((2 + Math.Cos(winding * t)) * Math.Cos(t)));
          A.Y = (float)(radius * ((2 + Math.Cos(winding * t)) * Math.Sin(t)));
          A.Z = (float)(radius * Math.Sin(winding * t));
        }

        //trigonometric tree
        else if (pic == 4)
        {
          A.X = (float)(radius * (t * Math.Sin(winding * t) * Math.Cos(winding * t) * Math.Cos(t)));
          A.Y = (float)(radius * (t * Math.Sin(winding * t) * Math.Cos(winding * t) * Math.Sin(t)));
          A.Z = (float) (radius * t);
        }

        //knot
        else if (pic == 5)
        {
          float phi = (float)(cc * Math.PI * Math.Sin(d * t));
          float rad = (float)(f + a * Math.Sin(6 * t + e));
          float theta = (float)(b * t);

          A.X = (float)(rad * Math.Cos(phi) * Math.Cos(theta));
          A.Y = (float)(rad * Math.Cos(phi) * Math.Sin(theta));
          A.Z = (float)(Math.Sin(phi));
        }

        // New vertex.
        int v = scene.AddVertex(Vector3.TransformPosition(A, m));

        // Vertex attributes.
        scene.SetTxtCoord(v, new Vector2((float)s, (float)s));


        Color c;
        if (pic == 4)
        {
          c = Color.ForestGreen;
        }
        else if (pic == 5)
        {
          c = Color.OrangeRed;
        }
        else 
        {
          c = Raster.Draw.ColorRamp(0.5 * (s + 1.0));
        }
        scene.SetColor(v, new Vector3(c.R / 255.0f, c.G / 255.0f, c.B / 255.0f));

        // New line?
        if (i > 0)
          scene.AddLine(vPrev, v);

        // Next vertex.
        t += dt;
        s += ds;
        vPrev = v;
        prevVec = A;
      }

      // Thick line (for rendering).
      scene.LineWidth = 3.0f;

      return segments;

      // }}
    }

    private Vector3 Lorenz (float x, float y, float z, float r, float s = 10, float b = 2.66666666667f)
    {
      float xD = s * (y - x);
      //float yD = r * x - y - x * z;
      float yD = x * (r - z) - y;
      float zD = x * y - b * z;
      return new Vector3(xD, yD, zD);
    }
    #endregion
  }
}
