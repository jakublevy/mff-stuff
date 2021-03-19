using System;
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Net;
using System.Reflection;
using System.Text.RegularExpressions;
using System.Windows.Forms;

namespace L_system
{
    public partial class LsystemForm : Form
    {
        private Point startingLocation;
        private PointF relativeLocation;
        private bool mouseDown = false;
        private Fractal[] fractals;
        public LsystemForm()
        {
            InitializeComponent();
            MakePanelDoubleBuffered(drawPanel);
            directionComboBox.SelectedIndex = 2;
            startingLocationLabel.Text = "Not set yet";
            fractals = new Fractal[10]; //maximum of 10 fractals drawn at the same time
        }
        /// <summary>
        /// Does panel.DoubleBuffered = true, using reflexion because it is private
        /// </summary>
        private void MakePanelDoubleBuffered(Panel panel)
        {
            Type t = panel.GetType();
            PropertyInfo pInfo = t.GetProperty("DoubleBuffered", BindingFlags.Instance | BindingFlags.NonPublic);
            pInfo.SetValue(panel, true, null);
        }

        /// <summary>
        /// Paint event method that draws the cursor
        /// </summary>
        private void drawPanel_Paint(object sender, PaintEventArgs e)
        {
            e.Graphics.SmoothingMode = SmoothingMode.AntiAlias;
            if (crossCheckBox.Checked && startingLocationLabel.Text != "Not set yet")
            {
                e.Graphics.DrawLine(Pens.DarkMagenta, startingLocation.X - 10, startingLocation.Y, startingLocation.X + 10, startingLocation.Y);
                e.Graphics.DrawLine(Pens.DarkMagenta, startingLocation.X, startingLocation.Y - 10, startingLocation.X, startingLocation.Y + 10);
            }
        }

        /// <summary>
        /// calls drawPanel_Paint
        /// </summary>
        private void crossCheckBox_CheckedChanged(object sender, EventArgs e)
        {
            drawPanel.Invalidate();
        }

        /// <summary>
        /// When the form is resized the cursors location is recalculated in order to stay relative
        /// </summary>
        private void LsystemForm_Resize(object sender, EventArgs e)
        {
            if (startingLocationLabel.Text != "Not set yet")
            {
                startingLocation = new Point(Convert.ToInt32(drawPanel.Width * relativeLocation.X),
                    Convert.ToInt32(drawPanel.Height * relativeLocation.Y));
                UpdateLocationLabel();
                drawPanel.Invalidate();
            }
        }

        /// <summary>
        /// Updates GUI with new cursors location
        /// </summary>
        /// <param name="newLocation"></param>
        private void ChangeStartingPointLocation(Point newLocation)
        {
            startingLocation = newLocation;
            relativeLocation = new PointF((float)startingLocation.X / drawPanel.Width, (float)startingLocation.Y / drawPanel.Height);
            UpdateLocationLabel();
        }

        private void UpdateLocationLabel()
        {
            startingLocationLabel.Text = startingLocation.ToString();
        }

        private void drawPanel_MouseDown(object sender, MouseEventArgs e)
        {
            mouseDown = true;
            ChangeStartingPointLocation(e.Location);
            drawPanel.Invalidate();
        }

        /// <summary>
        /// Check if Render button can be enabled by looking at whether cursor location is set and finding any ErrorProviders
        /// </summary>
        private void CheckRenderReq()
        {
            if (startingLocationLabel.Text != "Not set yet")
            {
                if (errorProvider1.GetError(axiomTextBox).Length == 0)
                {
                    foreach (var r in rulesFlowLayoutPanel.Controls)
                    {
                        RuleUC rule = (RuleUC) r;
                        ErrorProvider e = rule.ErrorProvider;
                        foreach (var c in rule.Controls)
                        {
                            if (e.GetError((Control) c).Length > 0)
                            {
                                renderToolStripMenuItem.Enabled = false;
                                return;
                            }
                        }
                        
                    }

                    renderToolStripMenuItem.Enabled = rulesFlowLayoutPanel.Controls.Count != 0;
                }
                else
                {
                    renderToolStripMenuItem.Enabled = false;
                }
            }
            else
            {
                renderToolStripMenuItem.Enabled = false;
            }
        }

        private void drawPanel_MouseMove(object sender, MouseEventArgs e)
        {
            if (mouseDown)
            {
                ChangeStartingPointLocation(e.Location);
                drawPanel.Invalidate();
            }
        }

        private void drawPanel_MouseUp(object sender, MouseEventArgs e)
        {
            mouseDown = false;
            CheckRenderReq();
        }

        /// <summary>
        /// Adds new empty rule to the flowLayoutPanel for rules
        /// </summary>
        private void addRuleButton_Click(object sender, EventArgs e)
        {
            RuleUC rule = new RuleUC();
            rule.Changed += rule_Changed;
            rulesFlowLayoutPanel.Controls.Add(rule);
        }

        private void rule_Changed(object sender, EventArgs e)
        {
            CheckRenderReq();
        }

        /// <summary>
        /// Add new empty colorChange to the flowLayoutPanel for colors 
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void changeColorButton_Click(object sender, EventArgs e)
        {
            ColorUC colorChange = new ColorUC();
            colorChange.Changed += changeColor_Changed;
            colorFlowLayoutPanel.Controls.Add(colorChange);
        }

        private void changeColor_Changed(object sender, EventArgs e)
        {
            CheckRenderReq();
        }

        /// <summary>
        /// Gathers the data from GUI and calls Lsystem to get correct instructions and turtle to draw
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void renderToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (axiomTextBox.Text == "")
            {
                Utils.ShowErrorMessageBox("Invalid axiom");
                return;
            }
            Lsystem lsys = new Lsystem(axiomTextBox.Text);
            foreach (var r in rulesFlowLayoutPanel.Controls)
            {
                RuleUC rule = (RuleUC) r;
                char letter = rule.Controls["charToRewriteTextBox"].Text.Length > 0 ? rule.Controls["charToRewriteTextBox"].Text[0] : '\0';
                string rewrite = rule.Controls["toRewriteTextBox"].Text;
                if (letter != '\0' && rewrite != "")
                {
                    lsys.Rules.Add(letter, rewrite);
                }
                else
                {
                    Utils.ShowErrorMessageBox("Invalid rule");
                    return;
                }
            }

            try
            {
                lsys.NthGeneration(Convert.ToInt32(generationNumUpDown.Value));
            }
            catch (KeyNotFoundException ex)
            {
                Utils.ShowErrorMessageBox(ex.Message);
                return;
            }

            Dictionary<char, Color> colors = new Dictionary<char, Color>();
            foreach (var c in colorFlowLayoutPanel.Controls)
            {
                ColorUC color = (ColorUC) c;
                char letter = color.Controls["charColorTextBox"].Text.Length > 0 ? color.Controls["charColorTextBox"].Text[0] : '\0';
                Color co = color.Controls["colorPanel"].BackColor;
                if (letter != '\0')
                {
                    colors.Add(letter, co);
                }
                else
                {
                    MessageBox.Show("Dangling color", "Warning", MessageBoxButtons.OK, MessageBoxIcon.Warning);
                }
            }
            fractals[Idx()]?.Turtle?.Dispose();
            if (fractals[Idx()] == null)
            {
                fractals[Idx()] = new Fractal();
            }
            fractals[Idx()].Turtle = new Turtle(lsys.Sentence, drawPanel, Convert.ToDouble(lineLengthNumUpDown.Value), Convert.ToDouble(rotationAngleNumUpDown.Value), startingLocation, Direction(directionComboBox.Text), colors, Convert.ToDouble(lineCoefficientNumUpDown.Value));
            fractals[Idx()].Lsys = lsys;

            fractals[Idx()].Turtle.Render();
            drawPanel.Invalidate();
        }

        /// <summary>
        /// Get active fractal index
        /// </summary>
        /// <returns></returns>
        private int Idx()
        {
            return (int) fractalIdxNumUpDown.Value;
        }

        /// <summary>
        /// Converts string equivalent of direction to vector
        /// </summary>
        private Point Direction(string text)
        {
            switch (text)
            {
                case "Left": return new Point(-1, 0);
                case "Right": return new Point(1, 0);
                case "Down": return new Point(0, 1);
                default: return new Point(0, -1);
            }
        }

        /// <summary>
        /// Check whether axiom input is OK and if not calls errorProvider
        /// </summary>
        private void axiomTextBox_Leave(object sender, EventArgs e)
        {
            if (axiomTextBox.Text == "")
            {
                errorProvider1.SetError(axiomTextBox, "Axiom cannot be empty");
            }
            else if (!Utils.LsystemRegex.IsMatch(axiomTextBox.Text))
            {
                errorProvider1.SetError(axiomTextBox, "Axiom contains disallowed characters");
            }
            else
            {
                errorProvider1.Clear();
            }
            CheckRenderReq();
        }
        private void aboutToolStripMenuItem_Click(object sender, EventArgs e)
        {
            AboutBox1 ab = new AboutBox1();
            ab.Show();
        }

        private void ClearRulesAndColorChangeContainer()
        {
            rulesFlowLayoutPanel.Controls.Clear();
            colorFlowLayoutPanel.Controls.Clear();
        }

        /// <summary>
        /// Adds rule to GUI
        /// </summary>
        /// <param name="l">key</param>
        /// <param name="rewrite">value</param>
        private void AddRuleToContainer(char l, string rewrite)
        {
            RuleUC rule = new RuleUC();
            rule.Controls["charToRewriteTextBox"].Text = l.ToString();
            rule.Controls["toRewriteTextBox"].Text = rewrite;
            rulesFlowLayoutPanel.Controls.Add(rule);
        }
        /// <summary>
        /// Adds colorChange to GUI
        /// </summary>
        /// <param name="l">key</param>
        /// <param name="c">value</param>
        private void AddColorChangeToContainer(char l, Color c)
        {
            ColorUC colU = new ColorUC();
            colU.Controls["charColorTextBox"].Text = l.ToString();
            colU.Controls["colorPanel"].BackColor = c;
            colorFlowLayoutPanel.Controls.Add(colU);
        }

        private void EnableAndClickRenderButton()
        {
            renderToolStripMenuItem.Enabled = true;
            renderToolStripMenuItem.PerformClick();
        }

        /// <summary>
        /// Whenever you click enter it is equivalent of clicking Render button
        /// </summary>
        protected override bool ProcessCmdKey(ref Message msg, Keys keyData)
        {
            if (keyData == Keys.Enter)
            {
                renderToolStripMenuItem.PerformClick();
                return true;
            }
            return base.ProcessCmdKey(ref msg, keyData);
        }

        private void ClearForNewFractal()
        {
            axiomTextBox.Text = "";
            rotationAngleNumUpDown.Value = 90;
            lineLengthNumUpDown.Value = 5;
            lineCoefficientNumUpDown.Value = 1;
            directionComboBox.Text = "Right";
            generationNumUpDown.Value = 6;
            startingLocationLabel.Text = "Not set yet";
            ClearRulesAndColorChangeContainer();
        }
        /// <summary>
        /// Clears the GUI to be prepared for new fractal
        /// </summary>
        private void clearCurrentFractal_Click(object sender, EventArgs e)
        {
            fractals[Idx()]?.Turtle?.Dispose();
            fractals[Idx()] = null;
            ClearForNewFractal();
            drawPanel.Invalidate();
        }

        /// <summary>
        /// Reverse of Direction method
        /// </summary>
        private string ConvertDirectionToText(Point dp)
        {
            if (dp == new Point(1, 0))
            {
                return "Right";
            }

            if (dp == new Point(-1, 0))
            {
                return "Left";
            }

            if (dp == new Point(0, 1))
            {
                return "Down";
            }

            return "Up";
        }

        /// <summary>
        /// The following methods just draws desired fractal from Examples 
        /// </summary>
        private void kochsCurveToolStripMenuItem_Click(object sender, EventArgs e)
        {
            ClearRulesAndColorChangeContainer();
            axiomTextBox.Text = "F++F++F";
            rotationAngleNumUpDown.Value = 60;
            lineLengthNumUpDown.Value = 4;
            lineCoefficientNumUpDown.Value = 1;
            directionComboBox.Text = "Right";
            generationNumUpDown.Value = 4;
            startingLocation = new Point(Convert.ToInt32(drawPanel.Width / 3.0), Convert.ToInt32(drawPanel.Height / 1.4));
            ChangeStartingPointLocation(startingLocation);
            AddRuleToContainer('F', "F-F++F-F");
            renderToolStripMenuItem.Enabled = true;
            renderToolStripMenuItem.PerformClick();

        }

        private void treeToolStripMenuItem_Click(object sender, EventArgs e)
        {
            ClearRulesAndColorChangeContainer();
            axiomTextBox.Text = "M";
            rotationAngleNumUpDown.Value = 45;
            lineLengthNumUpDown.Value = 3;
            lineCoefficientNumUpDown.Value = 1;
            directionComboBox.Text = "Up";
            generationNumUpDown.Value = 6;
            startingLocation = new Point(Convert.ToInt32(drawPanel.Width / 2.0), Convert.ToInt32(drawPanel.Height * 0.9));
            ChangeStartingPointLocation(startingLocation);
            AddRuleToContainer('M', "S[+M][-M]SM");
            AddRuleToContainer('S', "SS");
            AddColorChangeToContainer('M', Color.DarkGreen);
            AddColorChangeToContainer('S', Color.SaddleBrown);
            EnableAndClickRenderButton();
        }

        private void bushToolStripMenuItem_Click(object sender, EventArgs e)
        {
            ClearRulesAndColorChangeContainer();
            axiomTextBox.Text = "F";
            rotationAngleNumUpDown.Value = -25;
            lineLengthNumUpDown.Value = 4;
            lineCoefficientNumUpDown.Value = 1;
            directionComboBox.Text = "Up";
            generationNumUpDown.Value = 5;
            startingLocation = new Point(Convert.ToInt32(drawPanel.Width / 2.0), Convert.ToInt32(drawPanel.Height * 0.97));
            ChangeStartingPointLocation(startingLocation);
            AddRuleToContainer('F', "FF+[+F-F-F]-[-F+F+F]");
            AddColorChangeToContainer('F', Color.FromArgb(0,64,0));
            EnableAndClickRenderButton();
        }

        private void binaryTreeToolStripMenuItem_Click(object sender, EventArgs e)
        {
            ClearRulesAndColorChangeContainer();
            axiomTextBox.Text = "F";
            rotationAngleNumUpDown.Value = 45;
            lineLengthNumUpDown.Value = 3;
            lineCoefficientNumUpDown.Value = 1;
            directionComboBox.Text = "Up";
            generationNumUpDown.Value = 7;
            startingLocation = new Point(Convert.ToInt32(drawPanel.Width / 2.0), Convert.ToInt32(drawPanel.Height * 0.85));
            ChangeStartingPointLocation(startingLocation);
            AddRuleToContainer('F', "G[+F]-F");
            AddRuleToContainer('G', "GG");
            AddColorChangeToContainer('F', Color.DarkGreen);
            AddColorChangeToContainer('G', Color.SaddleBrown);
            EnableAndClickRenderButton();
        }

        private void fernToolStripMenuItem_Click(object sender, EventArgs e)
        {
            ClearRulesAndColorChangeContainer();
            axiomTextBox.Text = "G";
            rotationAngleNumUpDown.Value = 25;
            lineLengthNumUpDown.Value = 5;
            lineCoefficientNumUpDown.Value = 1;
            directionComboBox.Text = "Up";
            generationNumUpDown.Value = 5;
            startingLocation = new Point(Convert.ToInt32(drawPanel.Width / 2.0), Convert.ToInt32(drawPanel.Height * 0.9));
            ChangeStartingPointLocation(startingLocation);
            AddRuleToContainer('G', "F+[[G]-G]-F[-FG]+G");
            AddRuleToContainer('F', "FF");
            AddColorChangeToContainer('G', Color.DarkGreen);
            AddColorChangeToContainer('F', Color.DarkGreen);
            EnableAndClickRenderButton();
        }

        private void islandsAndLakesToolStripMenuItem_Click(object sender, EventArgs e)
        {
            ClearRulesAndColorChangeContainer();
            axiomTextBox.Text = "F+F+F+F";
            rotationAngleNumUpDown.Value = 90;
            lineLengthNumUpDown.Value = 5;
            lineCoefficientNumUpDown.Value = 1;
            directionComboBox.Text = "Right";
            generationNumUpDown.Value = 2;
            startingLocation = new Point(Convert.ToInt32(drawPanel.Width / 2.5), Convert.ToInt32(drawPanel.Height / 1.5));
            ChangeStartingPointLocation(startingLocation);
            AddRuleToContainer('F', "F+f-FF+F+FF+Ff+FF-f+FF-F-FF-Ff-FFF");
            AddRuleToContainer('f', "ffffff");
            EnableAndClickRenderButton();
        }

        private void mapToolStripMenuItem_Click(object sender, EventArgs e)
        {
            ClearRulesAndColorChangeContainer();
            axiomTextBox.Text = "F-F-F-F";
            rotationAngleNumUpDown.Value = 90;
            lineLengthNumUpDown.Value = 2;
            lineCoefficientNumUpDown.Value = 1;
            directionComboBox.Text = "Right";
            generationNumUpDown.Value = 6;
            startingLocation = new Point(Convert.ToInt32(drawPanel.Width / 1.7), Convert.ToInt32(drawPanel.Height / 1.5));
            ChangeStartingPointLocation(startingLocation);
            AddRuleToContainer('F', "F-FF--F-F");
            EnableAndClickRenderButton();
        }

        private void brokenWindowToolStripMenuItem_Click(object sender, EventArgs e)
        {
            ClearRulesAndColorChangeContainer();
            axiomTextBox.Text = "F-F-F-F";
            rotationAngleNumUpDown.Value = 90;
            lineLengthNumUpDown.Value = 5;
            lineCoefficientNumUpDown.Value = 1;
            directionComboBox.Text = "Right";
            generationNumUpDown.Value = 4;
            startingLocation = new Point(Convert.ToInt32(drawPanel.Width / 3.0), Convert.ToInt32(drawPanel.Height / 15.0));
            ChangeStartingPointLocation(startingLocation);
            AddRuleToContainer('F', "FF-F--F-F");
            AddColorChangeToContainer('F', Color.FromArgb(0,128,255));
            EnableAndClickRenderButton();
        }

        private void flowerToolStripMenuItem_Click(object sender, EventArgs e)
        {
            ClearRulesAndColorChangeContainer();
            axiomTextBox.Text = "F";
            rotationAngleNumUpDown.Value = 25;
            lineLengthNumUpDown.Value = 5;
            lineCoefficientNumUpDown.Value = 1;
            directionComboBox.Text = "Up";
            generationNumUpDown.Value = 4;
            startingLocation = new Point(Convert.ToInt32(drawPanel.Width / 2.0), Convert.ToInt32(drawPanel.Height * 0.93));
            ChangeStartingPointLocation(startingLocation);
            AddRuleToContainer('F', "F[+F]F[-F]F");
            AddColorChangeToContainer('F', Color.FromArgb(128,128,64));
            EnableAndClickRenderButton();
        }

        private void quadraticKochsIslandToolStripMenuItem_Click(object sender, EventArgs e)
        {
            ClearRulesAndColorChangeContainer();
            axiomTextBox.Text = "F-F-F-F";
            rotationAngleNumUpDown.Value = 90;
            lineLengthNumUpDown.Value = 1;
            lineCoefficientNumUpDown.Value = 1;
            directionComboBox.Text = "Right";
            generationNumUpDown.Value = 4;
            startingLocation = new Point(Convert.ToInt32(drawPanel.Width / 2.8), Convert.ToInt32(drawPanel.Height / 4.2));
            ChangeStartingPointLocation(startingLocation);
            AddRuleToContainer('F', "F-F+F+FF-F-F+F");
            EnableAndClickRenderButton();
        }

        /// <summary>
        /// Changes the active fractal
        /// </summary>
        private void fractalIdxNumUpDown_ValueChanged(object sender, EventArgs e)
        {
            ClearRulesAndColorChangeContainer();
            if (fractals[Idx()] != null)
            {
                axiomTextBox.Text = fractals[Idx()].Lsys.Axiom;
                rotationAngleNumUpDown.Value = (decimal) fractals[Idx()].Turtle.RotationAngle;
                lineLengthNumUpDown.Value = (decimal) fractals[Idx()].Turtle.LineLength;
                lineCoefficientNumUpDown.Value = (decimal) fractals[Idx()].Turtle.LineLengthCoefficient;
                directionComboBox.Text = ConvertDirectionToText(fractals[Idx()].Turtle.DirectionPoint);
                generationNumUpDown.Value = fractals[Idx()].Lsys.Generation;
                startingLocation = fractals[Idx()].Turtle.StartingPoint;
                UpdateLocationLabel();

                foreach (KeyValuePair<char, string> pair in fractals[Idx()].Lsys.Rules)
                {
                    AddRuleToContainer(pair.Key, pair.Value);
                }

                foreach (KeyValuePair<char, Color> pair in fractals[Idx()].Turtle.Colors)
                {
                    AddColorChangeToContainer(pair.Key, pair.Value);
                }

            }
            else
            {
                ClearForNewFractal();
            }
            drawPanel.Invalidate();
        }
    }
}
