namespace _117raster.ModuleHSV
{
  partial class HSVForm
  {
    /// <summary>
    /// Required designer variable.
    /// </summary>
    private System.ComponentModel.IContainer components = null;

    /// <summary>
    /// Clean up any resources being used.
    /// </summary>
    /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
    protected override void Dispose (bool disposing)
    {
      if (disposing && (components != null))
      {
        components.Dispose();
      }
      base.Dispose(disposing);
    }

    #region Windows Form Designer generated code

    /// <summary>
    /// Required method for Designer support - do not modify
    /// the contents of this method with the code editor.
    /// </summary>
    private void InitializeComponent ()
    {
            this.SuspendLayout();
            // 
            // HSVForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(800, 450);
            this.DoubleBuffered = true;
            this.MinimumSize = new System.Drawing.Size(200, 100);
            this.Name = "HSVForm";
            this.Text = "HSVForm";
            this.FormClosed += new System.Windows.Forms.FormClosedEventHandler(this.HSVForm_FormClosed);
            this.Paint += new System.Windows.Forms.PaintEventHandler(this.HSVForm_Paint);
            this.MouseMove += new System.Windows.Forms.MouseEventHandler(this.HSVForm_MouseMove);
            this.Resize += new System.EventHandler(this.HSVForm_Resize);
            this.ResumeLayout(false);

    }

    #endregion
  }
}