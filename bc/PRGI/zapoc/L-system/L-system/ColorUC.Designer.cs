namespace L_system
{
    partial class ColorUC
    {
        /// <summary> 
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary> 
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Component Designer generated code

        /// <summary> 
        /// Required method for Designer support - do not modify 
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            this.charColorTextBox = new System.Windows.Forms.TextBox();
            this.colorPanel = new System.Windows.Forms.Panel();
            this.cd = new System.Windows.Forms.ColorDialog();
            this.removeColorChangeButton = new System.Windows.Forms.Button();
            this.errorProvider1 = new System.Windows.Forms.ErrorProvider(this.components);
            ((System.ComponentModel.ISupportInitialize)(this.errorProvider1)).BeginInit();
            this.SuspendLayout();
            // 
            // charColorTextBox
            // 
            this.charColorTextBox.Location = new System.Drawing.Point(3, 10);
            this.charColorTextBox.MaxLength = 1;
            this.charColorTextBox.Name = "charColorTextBox";
            this.charColorTextBox.Size = new System.Drawing.Size(24, 20);
            this.charColorTextBox.TabIndex = 1;
            this.charColorTextBox.TextChanged += new System.EventHandler(this.charColorTextBox_TextChanged);
            this.charColorTextBox.Leave += new System.EventHandler(this.charColorTextBox_Leave);
            // 
            // colorPanel
            // 
            this.colorPanel.BackColor = System.Drawing.Color.Black;
            this.colorPanel.Location = new System.Drawing.Point(39, 10);
            this.colorPanel.Name = "colorPanel";
            this.colorPanel.Size = new System.Drawing.Size(53, 20);
            this.colorPanel.TabIndex = 2;
            this.colorPanel.Click += new System.EventHandler(this.colorPanel_Click);
            // 
            // removeColorChangeButton
            // 
            this.removeColorChangeButton.Location = new System.Drawing.Point(98, 2);
            this.removeColorChangeButton.Name = "removeColorChangeButton";
            this.removeColorChangeButton.Size = new System.Drawing.Size(84, 39);
            this.removeColorChangeButton.TabIndex = 4;
            this.removeColorChangeButton.Text = "Remove color change";
            this.removeColorChangeButton.UseVisualStyleBackColor = true;
            this.removeColorChangeButton.Click += new System.EventHandler(this.removeColorChangeButton_Click);
            // 
            // errorProvider1
            // 
            this.errorProvider1.ContainerControl = this;
            // 
            // ColorUC
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.removeColorChangeButton);
            this.Controls.Add(this.colorPanel);
            this.Controls.Add(this.charColorTextBox);
            this.Name = "ColorUC";
            this.Size = new System.Drawing.Size(189, 42);
            ((System.ComponentModel.ISupportInitialize)(this.errorProvider1)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.TextBox charColorTextBox;
        private System.Windows.Forms.Panel colorPanel;
        private System.Windows.Forms.ColorDialog cd;
        private System.Windows.Forms.Button removeColorChangeButton;
        private System.Windows.Forms.ErrorProvider errorProvider1;
    }
}
