namespace L_system
{
    partial class LsystemForm
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

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(LsystemForm));
            this.drawPanel = new System.Windows.Forms.Panel();
            this.axiomTextBox = new System.Windows.Forms.TextBox();
            this.label1 = new System.Windows.Forms.Label();
            this.label2 = new System.Windows.Forms.Label();
            this.label3 = new System.Windows.Forms.Label();
            this.label4 = new System.Windows.Forms.Label();
            this.lineCoefficientNumUpDown = new System.Windows.Forms.NumericUpDown();
            this.label5 = new System.Windows.Forms.Label();
            this.directionComboBox = new System.Windows.Forms.ComboBox();
            this.label6 = new System.Windows.Forms.Label();
            this.startingLocationLabel = new System.Windows.Forms.Label();
            this.crossCheckBox = new System.Windows.Forms.CheckBox();
            this.rulesFlowLayoutPanel = new System.Windows.Forms.FlowLayoutPanel();
            this.label7 = new System.Windows.Forms.Label();
            this.addRuleButton = new System.Windows.Forms.Button();
            this.label8 = new System.Windows.Forms.Label();
            this.generationNumUpDown = new System.Windows.Forms.NumericUpDown();
            this.label9 = new System.Windows.Forms.Label();
            this.colorFlowLayoutPanel = new System.Windows.Forms.FlowLayoutPanel();
            this.changeColorButton = new System.Windows.Forms.Button();
            this.errorProvider1 = new System.Windows.Forms.ErrorProvider(this.components);
            this.fractalIdxNumUpDown = new System.Windows.Forms.NumericUpDown();
            this.label10 = new System.Windows.Forms.Label();
            this.menuStrip1 = new System.Windows.Forms.MenuStrip();
            this.examplesToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.treeToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.binaryTreeToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.flowerToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.bushToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.fernToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.kochsCurveToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.quadraticKochsIslandToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.islandsAndLakesToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.mapToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.brokenWindowToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.renderToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.aboutToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.clearCurrentFractal = new System.Windows.Forms.Button();
            this.lineLengthNumUpDown = new L_system.NumericUpDownUnit();
            this.rotationAngleNumUpDown = new L_system.NumericUpDownUnit();
            ((System.ComponentModel.ISupportInitialize)(this.lineCoefficientNumUpDown)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.generationNumUpDown)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.errorProvider1)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.fractalIdxNumUpDown)).BeginInit();
            this.menuStrip1.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.lineLengthNumUpDown)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.rotationAngleNumUpDown)).BeginInit();
            this.SuspendLayout();
            // 
            // drawPanel
            // 
            this.drawPanel.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.drawPanel.BackColor = System.Drawing.Color.White;
            this.drawPanel.Location = new System.Drawing.Point(12, 119);
            this.drawPanel.Name = "drawPanel";
            this.drawPanel.Size = new System.Drawing.Size(1120, 463);
            this.drawPanel.TabIndex = 0;
            this.drawPanel.Paint += new System.Windows.Forms.PaintEventHandler(this.drawPanel_Paint);
            this.drawPanel.MouseDown += new System.Windows.Forms.MouseEventHandler(this.drawPanel_MouseDown);
            this.drawPanel.MouseMove += new System.Windows.Forms.MouseEventHandler(this.drawPanel_MouseMove);
            this.drawPanel.MouseUp += new System.Windows.Forms.MouseEventHandler(this.drawPanel_MouseUp);
            // 
            // axiomTextBox
            // 
            this.axiomTextBox.Location = new System.Drawing.Point(97, 27);
            this.axiomTextBox.Name = "axiomTextBox";
            this.axiomTextBox.Size = new System.Drawing.Size(71, 20);
            this.axiomTextBox.TabIndex = 1;
            this.axiomTextBox.Leave += new System.EventHandler(this.axiomTextBox_Leave);
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(53, 32);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(38, 13);
            this.label1.TabIndex = 2;
            this.label1.Text = "Axiom:";
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(12, 58);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(79, 13);
            this.label2.TabIndex = 4;
            this.label2.Text = "Rotation angle:";
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(29, 85);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(65, 13);
            this.label3.TabIndex = 6;
            this.label3.Text = "Line length: ";
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(244, 5);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(80, 13);
            this.label4.TabIndex = 9;
            this.label4.Text = "Line Coeficient:";
            // 
            // lineCoefficientNumUpDown
            // 
            this.lineCoefficientNumUpDown.DecimalPlaces = 4;
            this.lineCoefficientNumUpDown.Increment = new decimal(new int[] {
            1,
            0,
            0,
            196608});
            this.lineCoefficientNumUpDown.Location = new System.Drawing.Point(330, 3);
            this.lineCoefficientNumUpDown.Maximum = new decimal(new int[] {
            10,
            0,
            0,
            0});
            this.lineCoefficientNumUpDown.Minimum = new decimal(new int[] {
            1,
            0,
            0,
            65536});
            this.lineCoefficientNumUpDown.Name = "lineCoefficientNumUpDown";
            this.lineCoefficientNumUpDown.Size = new System.Drawing.Size(59, 20);
            this.lineCoefficientNumUpDown.TabIndex = 4;
            this.lineCoefficientNumUpDown.Value = new decimal(new int[] {
            1,
            0,
            0,
            0});
            // 
            // label5
            // 
            this.label5.AutoSize = true;
            this.label5.Location = new System.Drawing.Point(230, 35);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(94, 13);
            this.label5.TabIndex = 11;
            this.label5.Text = "Drawing Direction:";
            // 
            // directionComboBox
            // 
            this.directionComboBox.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.directionComboBox.FormattingEnabled = true;
            this.directionComboBox.Items.AddRange(new object[] {
            "Left",
            "Up",
            "Right",
            "Down"});
            this.directionComboBox.Location = new System.Drawing.Point(330, 32);
            this.directionComboBox.Name = "directionComboBox";
            this.directionComboBox.Size = new System.Drawing.Size(59, 21);
            this.directionComboBox.TabIndex = 5;
            // 
            // label6
            // 
            this.label6.AutoSize = true;
            this.label6.Location = new System.Drawing.Point(233, 95);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(90, 13);
            this.label6.TabIndex = 13;
            this.label6.Text = "Starting Location:";
            // 
            // startingLocationLabel
            // 
            this.startingLocationLabel.Location = new System.Drawing.Point(327, 95);
            this.startingLocationLabel.Name = "startingLocationLabel";
            this.startingLocationLabel.Size = new System.Drawing.Size(100, 15);
            this.startingLocationLabel.TabIndex = 14;
            // 
            // crossCheckBox
            // 
            this.crossCheckBox.AutoSize = true;
            this.crossCheckBox.Checked = true;
            this.crossCheckBox.CheckState = System.Windows.Forms.CheckState.Checked;
            this.crossCheckBox.Location = new System.Drawing.Point(418, 94);
            this.crossCheckBox.Name = "crossCheckBox";
            this.crossCheckBox.Size = new System.Drawing.Size(52, 17);
            this.crossCheckBox.TabIndex = 7;
            this.crossCheckBox.Text = "Cross";
            this.crossCheckBox.UseVisualStyleBackColor = true;
            this.crossCheckBox.CheckedChanged += new System.EventHandler(this.crossCheckBox_CheckedChanged);
            // 
            // rulesFlowLayoutPanel
            // 
            this.rulesFlowLayoutPanel.AutoScroll = true;
            this.rulesFlowLayoutPanel.Location = new System.Drawing.Point(521, 22);
            this.rulesFlowLayoutPanel.Name = "rulesFlowLayoutPanel";
            this.rulesFlowLayoutPanel.Size = new System.Drawing.Size(276, 94);
            this.rulesFlowLayoutPanel.TabIndex = 17;
            // 
            // label7
            // 
            this.label7.AutoSize = true;
            this.label7.Location = new System.Drawing.Point(642, 4);
            this.label7.Name = "label7";
            this.label7.Size = new System.Drawing.Size(37, 13);
            this.label7.TabIndex = 18;
            this.label7.Text = "Rules:";
            // 
            // addRuleButton
            // 
            this.addRuleButton.Location = new System.Drawing.Point(803, 19);
            this.addRuleButton.Name = "addRuleButton";
            this.addRuleButton.Size = new System.Drawing.Size(75, 23);
            this.addRuleButton.TabIndex = 8;
            this.addRuleButton.Text = "Add rule";
            this.addRuleButton.UseVisualStyleBackColor = true;
            this.addRuleButton.Click += new System.EventHandler(this.addRuleButton_Click);
            // 
            // label8
            // 
            this.label8.AutoSize = true;
            this.label8.Location = new System.Drawing.Point(262, 64);
            this.label8.Name = "label8";
            this.label8.Size = new System.Drawing.Size(62, 13);
            this.label8.TabIndex = 20;
            this.label8.Text = "Generation:";
            // 
            // generationNumUpDown
            // 
            this.generationNumUpDown.Location = new System.Drawing.Point(330, 59);
            this.generationNumUpDown.Maximum = new decimal(new int[] {
            50,
            0,
            0,
            0});
            this.generationNumUpDown.Name = "generationNumUpDown";
            this.generationNumUpDown.Size = new System.Drawing.Size(59, 20);
            this.generationNumUpDown.TabIndex = 6;
            this.generationNumUpDown.Value = new decimal(new int[] {
            6,
            0,
            0,
            0});
            // 
            // label9
            // 
            this.label9.AutoSize = true;
            this.label9.Location = new System.Drawing.Point(1007, 5);
            this.label9.Name = "label9";
            this.label9.Size = new System.Drawing.Size(34, 13);
            this.label9.TabIndex = 22;
            this.label9.Text = "Color:";
            // 
            // colorFlowLayoutPanel
            // 
            this.colorFlowLayoutPanel.AutoScroll = true;
            this.colorFlowLayoutPanel.Location = new System.Drawing.Point(912, 23);
            this.colorFlowLayoutPanel.Name = "colorFlowLayoutPanel";
            this.colorFlowLayoutPanel.Size = new System.Drawing.Size(218, 94);
            this.colorFlowLayoutPanel.TabIndex = 18;
            // 
            // changeColorButton
            // 
            this.changeColorButton.Location = new System.Drawing.Point(802, 74);
            this.changeColorButton.Name = "changeColorButton";
            this.changeColorButton.Size = new System.Drawing.Size(76, 34);
            this.changeColorButton.TabIndex = 23;
            this.changeColorButton.Text = "Add color change";
            this.changeColorButton.UseVisualStyleBackColor = true;
            this.changeColorButton.Click += new System.EventHandler(this.changeColorButton_Click);
            // 
            // errorProvider1
            // 
            this.errorProvider1.ContainerControl = this;
            // 
            // fractalIdxNumUpDown
            // 
            this.fractalIdxNumUpDown.Location = new System.Drawing.Point(437, 20);
            this.fractalIdxNumUpDown.Maximum = new decimal(new int[] {
            9,
            0,
            0,
            0});
            this.fractalIdxNumUpDown.Name = "fractalIdxNumUpDown";
            this.fractalIdxNumUpDown.Size = new System.Drawing.Size(51, 20);
            this.fractalIdxNumUpDown.TabIndex = 24;
            this.fractalIdxNumUpDown.ValueChanged += new System.EventHandler(this.fractalIdxNumUpDown_ValueChanged);
            // 
            // label10
            // 
            this.label10.AutoSize = true;
            this.label10.Location = new System.Drawing.Point(434, 4);
            this.label10.Name = "label10";
            this.label10.Size = new System.Drawing.Size(58, 13);
            this.label10.TabIndex = 25;
            this.label10.Text = "Fractal idx:";
            // 
            // menuStrip1
            // 
            this.menuStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.examplesToolStripMenuItem,
            this.renderToolStripMenuItem,
            this.aboutToolStripMenuItem});
            this.menuStrip1.Location = new System.Drawing.Point(0, 0);
            this.menuStrip1.Name = "menuStrip1";
            this.menuStrip1.Size = new System.Drawing.Size(1144, 24);
            this.menuStrip1.TabIndex = 26;
            this.menuStrip1.Text = "menuStrip1";
            // 
            // examplesToolStripMenuItem
            // 
            this.examplesToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.treeToolStripMenuItem,
            this.binaryTreeToolStripMenuItem,
            this.flowerToolStripMenuItem,
            this.bushToolStripMenuItem,
            this.fernToolStripMenuItem,
            this.kochsCurveToolStripMenuItem,
            this.quadraticKochsIslandToolStripMenuItem,
            this.islandsAndLakesToolStripMenuItem,
            this.mapToolStripMenuItem,
            this.brokenWindowToolStripMenuItem});
            this.examplesToolStripMenuItem.Name = "examplesToolStripMenuItem";
            this.examplesToolStripMenuItem.Size = new System.Drawing.Size(68, 20);
            this.examplesToolStripMenuItem.Text = "Examples";
            // 
            // treeToolStripMenuItem
            // 
            this.treeToolStripMenuItem.Name = "treeToolStripMenuItem";
            this.treeToolStripMenuItem.Size = new System.Drawing.Size(198, 22);
            this.treeToolStripMenuItem.Text = "Tree";
            this.treeToolStripMenuItem.Click += new System.EventHandler(this.treeToolStripMenuItem_Click);
            // 
            // binaryTreeToolStripMenuItem
            // 
            this.binaryTreeToolStripMenuItem.Name = "binaryTreeToolStripMenuItem";
            this.binaryTreeToolStripMenuItem.Size = new System.Drawing.Size(198, 22);
            this.binaryTreeToolStripMenuItem.Text = "Binary tree";
            this.binaryTreeToolStripMenuItem.Click += new System.EventHandler(this.binaryTreeToolStripMenuItem_Click);
            // 
            // flowerToolStripMenuItem
            // 
            this.flowerToolStripMenuItem.Name = "flowerToolStripMenuItem";
            this.flowerToolStripMenuItem.Size = new System.Drawing.Size(198, 22);
            this.flowerToolStripMenuItem.Text = "Flower";
            this.flowerToolStripMenuItem.Click += new System.EventHandler(this.flowerToolStripMenuItem_Click);
            // 
            // bushToolStripMenuItem
            // 
            this.bushToolStripMenuItem.Name = "bushToolStripMenuItem";
            this.bushToolStripMenuItem.Size = new System.Drawing.Size(198, 22);
            this.bushToolStripMenuItem.Text = "Bush";
            this.bushToolStripMenuItem.Click += new System.EventHandler(this.bushToolStripMenuItem_Click);
            // 
            // fernToolStripMenuItem
            // 
            this.fernToolStripMenuItem.Name = "fernToolStripMenuItem";
            this.fernToolStripMenuItem.Size = new System.Drawing.Size(198, 22);
            this.fernToolStripMenuItem.Text = "Fern";
            this.fernToolStripMenuItem.Click += new System.EventHandler(this.fernToolStripMenuItem_Click);
            // 
            // kochsCurveToolStripMenuItem
            // 
            this.kochsCurveToolStripMenuItem.Name = "kochsCurveToolStripMenuItem";
            this.kochsCurveToolStripMenuItem.Size = new System.Drawing.Size(198, 22);
            this.kochsCurveToolStripMenuItem.Text = "Koch\'s snowflake";
            this.kochsCurveToolStripMenuItem.Click += new System.EventHandler(this.kochsCurveToolStripMenuItem_Click);
            // 
            // quadraticKochsIslandToolStripMenuItem
            // 
            this.quadraticKochsIslandToolStripMenuItem.Name = "quadraticKochsIslandToolStripMenuItem";
            this.quadraticKochsIslandToolStripMenuItem.Size = new System.Drawing.Size(198, 22);
            this.quadraticKochsIslandToolStripMenuItem.Text = "Quadratic Koch\'s island";
            this.quadraticKochsIslandToolStripMenuItem.Click += new System.EventHandler(this.quadraticKochsIslandToolStripMenuItem_Click);
            // 
            // islandsAndLakesToolStripMenuItem
            // 
            this.islandsAndLakesToolStripMenuItem.Name = "islandsAndLakesToolStripMenuItem";
            this.islandsAndLakesToolStripMenuItem.Size = new System.Drawing.Size(198, 22);
            this.islandsAndLakesToolStripMenuItem.Text = "Islands and lakes";
            this.islandsAndLakesToolStripMenuItem.Click += new System.EventHandler(this.islandsAndLakesToolStripMenuItem_Click);
            // 
            // mapToolStripMenuItem
            // 
            this.mapToolStripMenuItem.Name = "mapToolStripMenuItem";
            this.mapToolStripMenuItem.Size = new System.Drawing.Size(198, 22);
            this.mapToolStripMenuItem.Text = "Map";
            this.mapToolStripMenuItem.Click += new System.EventHandler(this.mapToolStripMenuItem_Click);
            // 
            // brokenWindowToolStripMenuItem
            // 
            this.brokenWindowToolStripMenuItem.Name = "brokenWindowToolStripMenuItem";
            this.brokenWindowToolStripMenuItem.Size = new System.Drawing.Size(198, 22);
            this.brokenWindowToolStripMenuItem.Text = "Broken window";
            this.brokenWindowToolStripMenuItem.Click += new System.EventHandler(this.brokenWindowToolStripMenuItem_Click);
            // 
            // renderToolStripMenuItem
            // 
            this.renderToolStripMenuItem.Enabled = false;
            this.renderToolStripMenuItem.Name = "renderToolStripMenuItem";
            this.renderToolStripMenuItem.Size = new System.Drawing.Size(56, 20);
            this.renderToolStripMenuItem.Text = "Render";
            this.renderToolStripMenuItem.Click += new System.EventHandler(this.renderToolStripMenuItem_Click);
            // 
            // aboutToolStripMenuItem
            // 
            this.aboutToolStripMenuItem.Name = "aboutToolStripMenuItem";
            this.aboutToolStripMenuItem.Size = new System.Drawing.Size(52, 20);
            this.aboutToolStripMenuItem.Text = "About";
            this.aboutToolStripMenuItem.Click += new System.EventHandler(this.aboutToolStripMenuItem_Click);
            // 
            // clearCurrentFractal
            // 
            this.clearCurrentFractal.Location = new System.Drawing.Point(437, 43);
            this.clearCurrentFractal.Name = "clearCurrentFractal";
            this.clearCurrentFractal.Size = new System.Drawing.Size(51, 23);
            this.clearCurrentFractal.TabIndex = 27;
            this.clearCurrentFractal.Text = "Clear";
            this.clearCurrentFractal.UseVisualStyleBackColor = true;
            this.clearCurrentFractal.Click += new System.EventHandler(this.clearCurrentFractal_Click);
            // 
            // lineLengthNumUpDown
            // 
            this.lineLengthNumUpDown.Location = new System.Drawing.Point(97, 83);
            this.lineLengthNumUpDown.Maximum = new decimal(new int[] {
            1000,
            0,
            0,
            0});
            this.lineLengthNumUpDown.Minimum = new decimal(new int[] {
            1,
            0,
            0,
            0});
            this.lineLengthNumUpDown.Name = "lineLengthNumUpDown";
            this.lineLengthNumUpDown.Size = new System.Drawing.Size(71, 20);
            this.lineLengthNumUpDown.Suffix = " px";
            this.lineLengthNumUpDown.TabIndex = 3;
            this.lineLengthNumUpDown.Value = new decimal(new int[] {
            5,
            0,
            0,
            0});
            // 
            // rotationAngleNumUpDown
            // 
            this.rotationAngleNumUpDown.Location = new System.Drawing.Point(97, 56);
            this.rotationAngleNumUpDown.Maximum = new decimal(new int[] {
            180,
            0,
            0,
            0});
            this.rotationAngleNumUpDown.Minimum = new decimal(new int[] {
            180,
            0,
            0,
            -2147483648});
            this.rotationAngleNumUpDown.Name = "rotationAngleNumUpDown";
            this.rotationAngleNumUpDown.Size = new System.Drawing.Size(71, 20);
            this.rotationAngleNumUpDown.Suffix = " °";
            this.rotationAngleNumUpDown.TabIndex = 2;
            this.rotationAngleNumUpDown.Value = new decimal(new int[] {
            90,
            0,
            0,
            0});
            // 
            // LsystemForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(1144, 603);
            this.Controls.Add(this.clearCurrentFractal);
            this.Controls.Add(this.label10);
            this.Controls.Add(this.fractalIdxNumUpDown);
            this.Controls.Add(this.changeColorButton);
            this.Controls.Add(this.colorFlowLayoutPanel);
            this.Controls.Add(this.label9);
            this.Controls.Add(this.generationNumUpDown);
            this.Controls.Add(this.label8);
            this.Controls.Add(this.addRuleButton);
            this.Controls.Add(this.label7);
            this.Controls.Add(this.rulesFlowLayoutPanel);
            this.Controls.Add(this.lineLengthNumUpDown);
            this.Controls.Add(this.crossCheckBox);
            this.Controls.Add(this.startingLocationLabel);
            this.Controls.Add(this.label6);
            this.Controls.Add(this.directionComboBox);
            this.Controls.Add(this.label5);
            this.Controls.Add(this.lineCoefficientNumUpDown);
            this.Controls.Add(this.label4);
            this.Controls.Add(this.label3);
            this.Controls.Add(this.rotationAngleNumUpDown);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.axiomTextBox);
            this.Controls.Add(this.drawPanel);
            this.Controls.Add(this.menuStrip1);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.MainMenuStrip = this.menuStrip1;
            this.Name = "LsystemForm";
            this.Text = "L-system";
            this.Resize += new System.EventHandler(this.LsystemForm_Resize);
            ((System.ComponentModel.ISupportInitialize)(this.lineCoefficientNumUpDown)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.generationNumUpDown)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.errorProvider1)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.fractalIdxNumUpDown)).EndInit();
            this.menuStrip1.ResumeLayout(false);
            this.menuStrip1.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.lineLengthNumUpDown)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.rotationAngleNumUpDown)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Panel drawPanel;
        private System.Windows.Forms.TextBox axiomTextBox;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Label label2;
        private NumericUpDownUnit rotationAngleNumUpDown;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.NumericUpDown lineCoefficientNumUpDown;
        private System.Windows.Forms.Label label5;
        private System.Windows.Forms.ComboBox directionComboBox;
        private System.Windows.Forms.Label label6;
        private System.Windows.Forms.Label startingLocationLabel;
        private System.Windows.Forms.CheckBox crossCheckBox;
        private NumericUpDownUnit lineLengthNumUpDown;
        private System.Windows.Forms.FlowLayoutPanel rulesFlowLayoutPanel;
        private System.Windows.Forms.Label label7;
        private System.Windows.Forms.Button addRuleButton;
        private System.Windows.Forms.Label label8;
        private System.Windows.Forms.NumericUpDown generationNumUpDown;
        private System.Windows.Forms.Label label9;
        private System.Windows.Forms.FlowLayoutPanel colorFlowLayoutPanel;
        private System.Windows.Forms.Button changeColorButton;
        private System.Windows.Forms.ErrorProvider errorProvider1;
        private System.Windows.Forms.Label label10;
        private System.Windows.Forms.NumericUpDown fractalIdxNumUpDown;
        private System.Windows.Forms.MenuStrip menuStrip1;
        private System.Windows.Forms.ToolStripMenuItem examplesToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem treeToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem binaryTreeToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem flowerToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem bushToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem fernToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem kochsCurveToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem quadraticKochsIslandToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem islandsAndLakesToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem mapToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem brokenWindowToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem renderToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem aboutToolStripMenuItem;
        private System.Windows.Forms.Button clearCurrentFractal;
    }
}

