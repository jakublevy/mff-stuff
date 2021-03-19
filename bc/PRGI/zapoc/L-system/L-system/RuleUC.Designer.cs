namespace L_system
{
    partial class RuleUC
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
            this.charToRewriteTextBox = new System.Windows.Forms.TextBox();
            this.label1 = new System.Windows.Forms.Label();
            this.toRewriteTextBox = new System.Windows.Forms.TextBox();
            this.removeRuleButton = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // charToRewriteTextBox
            // 
            this.charToRewriteTextBox.Location = new System.Drawing.Point(3, 10);
            this.charToRewriteTextBox.MaxLength = 1;
            this.charToRewriteTextBox.Name = "charToRewriteTextBox";
            this.charToRewriteTextBox.Size = new System.Drawing.Size(24, 20);
            this.charToRewriteTextBox.TabIndex = 0;
            this.charToRewriteTextBox.Leave += new System.EventHandler(this.textBox_Leave);
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(34, 13);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(19, 13);
            this.label1.TabIndex = 1;
            this.label1.Text = "-->";
            // 
            // toRewriteTextBox
            // 
            this.toRewriteTextBox.Location = new System.Drawing.Point(59, 10);
            this.toRewriteTextBox.Name = "toRewriteTextBox";
            this.toRewriteTextBox.Size = new System.Drawing.Size(125, 20);
            this.toRewriteTextBox.TabIndex = 2;
            this.toRewriteTextBox.Leave += new System.EventHandler(this.textBox_Leave);
            // 
            // removeRuleButton
            // 
            this.removeRuleButton.Location = new System.Drawing.Point(190, 2);
            this.removeRuleButton.Name = "removeRuleButton";
            this.removeRuleButton.Size = new System.Drawing.Size(55, 39);
            this.removeRuleButton.TabIndex = 3;
            this.removeRuleButton.Text = "Remove rule";
            this.removeRuleButton.UseVisualStyleBackColor = true;
            this.removeRuleButton.Click += new System.EventHandler(this.removeRuleButton_Click);
            // 
            // RuleUC
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.removeRuleButton);
            this.Controls.Add(this.toRewriteTextBox);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.charToRewriteTextBox);
            this.Name = "RuleUC";
            this.Size = new System.Drawing.Size(251, 42);
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.TextBox charToRewriteTextBox;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.TextBox toRewriteTextBox;
        private System.Windows.Forms.Button removeRuleButton;
    }
}
