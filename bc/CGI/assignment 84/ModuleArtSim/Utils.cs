using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Linq;
using System.Security.Cryptography;

namespace JakubLevy
{
  class Utils
  {
    public static double ColorDistance (Color c1, Color c2)
    {
      return Math.Sqrt((c2.R - c1.R) * 0.3 * ((c2.R - c1.R) * 0.3) + ((c2.G - c1.G) * 0.59) * ((c2.G - c1.G) * 0.59) + ((c2.B - c1.B) * 0.11) * ((c2.B - c1.B) * 0.11));
    }

    public static Color RandomColor ()
    {
      RandomGenerator rnd = new RandomGenerator();
      return Color.FromArgb(rnd.Next(0, 256), rnd.Next(0, 256), rnd.Next(0, 256));
    }

                  //key                                 x_j                                    
    private static Color NearestCentroid (Color c, Dictionary<Color, List<Color>> clusters)
    {
      Color nearestCentroid = clusters.Keys.First();
      double nearestDistance = ColorDistance(c, nearestCentroid);
      foreach (var cluster in clusters)
      {
        Color centroid = cluster.Key;
        double newDistance = ColorDistance(c, centroid);
        if (newDistance < nearestDistance)
        {
          nearestCentroid = centroid;
          nearestDistance = newDistance;
        }
      }

      return nearestCentroid;
    }

    public static Dictionary<Color, List<Color>> KMeans (int k, List<Color> colors, int iterations)
    {
      Dictionary<Color, List<Color>> clusters = new Dictionary<Color, List<Color>>();
      for (int i = 0; i < k; ++i)
      {
        Color centroid = RandomColor();
        while (clusters.ContainsKey(centroid))
        {
          centroid = RandomColor();
        }

        clusters.Add(RandomColor(), new List<Color>());

        
      }


      for (int i = 0; i < iterations; ++i)
      {
        for (int j = 0; j < colors.Count; ++j)
        {
          Color nearestCentroid = NearestCentroid(colors[j], clusters);
          clusters[nearestCentroid].Add(colors[j]);
        }

        if (i + 1 == iterations)
        {
          return clusters;

        }

        Dictionary<Color, List<Color>> newClusters = new Dictionary<Color, List<Color>>();
        foreach (var cluster in clusters)
        {
          List<Color> xjs = cluster.Value;
          Color newCentroid = xjs.Any() ? AverageColor(xjs) : RandomColor();
          while (newClusters.ContainsKey(newCentroid))
          {
            newCentroid = RandomColor();
          }
          newClusters[newCentroid] = new List<Color>();
          //newClusters[newCentroid] = xjs;
        }

        clusters = newClusters;
      }

      return clusters;
    }

    private static Color AverageColor (List<Color> xjs)
    {
      double r = 0, g = 0, b = 0;
      for (int i = 0; i < xjs.Count; ++i)
      {
        r += xjs[i].R;
        g += xjs[i].G;
        b += xjs[i].B;
      }
      return Color.FromArgb((int)(r / xjs.Count), (int)(g / xjs.Count), (int)(b / xjs.Count));
    }

    public static List<Color> ExtractColors (Dictionary<Color, List<Color>> clusters, int colorFromClusterCount)
    {
      List<Color> output = new List<Color>();
      foreach (var cluster in clusters)
      {
        output.Add(cluster.Key);
        if (colorFromClusterCount >= cluster.Value.Count)
        {
          output.AddRange(cluster.Value);
        }
        else
        {
          HashSet<int> addedIdxs = new HashSet<int>();
          RandomGenerator rnd = new RandomGenerator();
          while (addedIdxs.Count < colorFromClusterCount)
          {
            int idx = rnd.Next(0, cluster.Value.Count);
            while (addedIdxs.Contains(idx))
            {
              idx = rnd.Next(0, cluster.Value.Count);
            }

            addedIdxs.Add(idx);
            output.Add(clusters[cluster.Key][idx]);
          }
        }
      }

      return output;
    }

    public static List<double> Softmin (Color original, List<Color> usableColors, double softness)
    {
      List<double> distances = new List<double>();
      for (int i = 0; i < usableColors.Count; ++i)
      {
        distances.Add(-ColorDistance(original, usableColors[i]) / softness);
      }
      List<double> exponents = new List<double>();
      double sum = 0;
      for (int i = 0; i < distances.Count; ++i)
      {
        exponents.Add(Math.Pow(Math.E, distances[i]));
        sum += exponents.Last();
      }
      List<double> ret = new List<double>();
      for(int i = 0; i < distances.Count; ++i)
      {
        ret.Add(exponents[i] / sum);
      }


      return ret;
    }

    public static int GenRandomFromDist (List<double> distribution)
    {
      double rndN = NextDouble();
      double acc = 0;
      for (int i = 0; i < distribution.Count; ++i)
      {
        if (rndN >= acc && rndN <= acc + distribution[i])
        {
          return i;
        }
        acc += distribution[i];
      }

      return distribution.Count - 1;
    }

    public static double NextDouble ()
    {
      var rng = new RNGCryptoServiceProvider();
      var bytes = new byte[8];
      rng.GetBytes(bytes);
      var ul = BitConverter.ToUInt64(bytes, 0) / (1 << 11);
      double d = ul / (double)(1UL << 53);
      return d;
    }

    public static double NextDouble (double min, double max)
    {
      return min + (max - min) * NextDouble();
    }
  }
  class Params
  {
    [Description("K-means K parameter\nMin = 1")]
    public int K { get; set; }

    [DisplayName("# colors from cluster")]
    [Description("Number of additional colors to take from each cluster\nMin = 0")]
    public int ColorFromClusterCount { get; set; }


    [DisplayName("# K-means iterations")]
    [Description("Number of K-means iterations to run\nMin = 0")]
    public int Iterations { get; set; }

    [DisplayName("softmin softness")]

    [Description("Determines how soft is the softmin function.\nBigger number ~ softer\nSmaller number ~ harder (Min > 0)")]
    public double SoftminSoftness { get; set; }
    [DisplayName("min dot size")]

    [Description("Minimum size of each individual dot\n> 0")]
    public double DotSizeMin { get; set; }

    [DisplayName("max dot size")]

    [Description("Maximum size of each individual dot\n>= min dot size")]
    public double DotSizeMax { get; set; }

    [DisplayName("dot probability")]
    [Description("Probability of making a dot on a pixel\n0 <= dot probability <= 1")]
    public double PutDotProbability { get; set; }

    [DisplayName("# filter iterations")]
    [Description("# times to apply dot filter\nMin = 0")]
    public int FilterIterations { get; set; }
  }

  public static class ExtensionMethods
  {
    public static void FillCircle (this Graphics g, Brush p, PointF pt, float rad)
    {
      g.FillEllipse(p, pt.X - rad, pt.Y - rad, rad * 2, rad * 2);
    }
  }
}
