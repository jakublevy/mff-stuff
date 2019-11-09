namespace JakubLevy
{
  public static class Extensions
  {
    public static double FixedValue (this double value, double min, double max)
    {
      if (value >= min && value <= max)
        return value;
      if (value > max)
        return max;
      if (value < min)
      {
        return min;
      }

      return 1;
    }
  }
}
