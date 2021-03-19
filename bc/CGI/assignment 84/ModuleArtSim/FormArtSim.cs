using System;
using System.Globalization;
using System.Threading;
using System.Windows.Forms;
using Modules;

namespace JakubLevy
{
  public partial class FormArtSim : Form
  {
    /// <summary>
    /// Associated raster module (to be notified under various conditions).
    /// </summary>
    protected IRasterModule module;

    /// <summary>
    /// If true, any of the values was changed and needs to by send to the module.
    /// </summary>
    protected bool dirty = true;

    private Params config = new Params {K = 20, ColorFromClusterCount = 3, Iterations = 35, DotSizeMin = 0.1, DotSizeMax = 3, PutDotProbability = 0.6, FilterIterations = 1, SoftminSoftness = 20};

    public FormArtSim (IRasterModule hModule)
    {
      module = hModule;
      InitializeComponent();

      //prefer using . than , in decimal numbers
      Thread.CurrentThread.CurrentCulture = CultureInfo.GetCultureInfo("en-US");

      paramsPropertyGrid.SelectedObject = config;
    }

    private void buttonRecompute_Click (object sender, EventArgs e)
    {
      if (module == null)
        return;

      //input check
      config.K = Math.Max(1, config.K);
      config.ColorFromClusterCount = Math.Max(0, config.ColorFromClusterCount);
      config.Iterations = Math.Max(0, config.Iterations);
      config.SoftminSoftness = config.SoftminSoftness > 0 ? config.SoftminSoftness : 0.01;
      config.DotSizeMin = config.DotSizeMin > 0 ? config.DotSizeMin : 0.01;
      config.DotSizeMax = config.DotSizeMax >= config.DotSizeMin ? config.DotSizeMax : config.DotSizeMin + 3;

      if (config.PutDotProbability < 0 || config.PutDotProbability > 1)
      {
        config.PutDotProbability = 0.6;
      }

      config.FilterIterations = config.FilterIterations >= 0 ? config.FilterIterations : 1;

      paramsPropertyGrid.SelectedObject = config;

      if (dirty)
      {
        module.OnGuiWindowChanged();
        dirty = false;
      }

      module.UpdateRequest?.Invoke(module);
    }

    private void buttonReset_Click (object sender, EventArgs e)
    {

      paramsPropertyGrid.SelectedObject = new Params {K = 20, ColorFromClusterCount = 3, Iterations = 35, DotSizeMin = 0.1, DotSizeMax = 3, PutDotProbability = 0.6, FilterIterations = 1, SoftminSoftness = 20};

      module?.OnGuiWindowChanged();
      dirty = false;
    }

    private void buttonDeactivate_Click (object sender, EventArgs e)
    {
      if (module != null)
      {
        if (dirty)
          module.OnGuiWindowChanged();

        module.DeactivateRequest?.Invoke(module);
      }
    }

    private void FormHSV_FormClosed (object sender, FormClosedEventArgs e)
    {
      if (module != null)
      {
        if (dirty)
          module.OnGuiWindowChanged();

        module.OnGuiWindowClose();
      }
    }

    protected override bool ProcessCmdKey (ref Message msg, Keys keyData)
    {
      if (keyData == Keys.Enter)
        buttonRecompute.PerformClick();
      
      return base.ProcessCmdKey(ref msg, keyData);
    }
  }
}
