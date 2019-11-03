using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace _117raster
{
  public static class ExtensionMethods
  {

    public static IEnumerable<T> RandomShuffle<T> (this IEnumerable<T> col)
    {
      Random rnd = new Random();
      return col.OrderBy(x => rnd.Next());
    }

    public static List<int> RandomIdxSample<T> (this List<T> l, int sampleSize)
    {
      Random rnd = new Random();
      List<int> idxs = new List<int>(sampleSize);
      if (l.Count <= sampleSize)
      {
        idxs.AddRange(Enumerable.Range(0, l.Count));
        return idxs;
      }

      while (idxs.Count < sampleSize)
      {
        int idx = rnd.Next(0, l.Count);
        if (!idxs.Contains(idx))
        {
          idxs.Add(idx);
        }
      }
      return idxs;
    }

    public static void FastRemoveAt<T> (this List<T> l, int idx)
    {
      l[idx] = l[l.Count - 1];
      l.RemoveAt(l.Count - 1);
    }
  }
}
