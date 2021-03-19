using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace L_system
{
    public partial class ColorUC : UserControl
    {
        private string previous = "";
        Regex r = new Regex("[A-Z]{1}");
        public EventHandler Changed { get; set; }
        public ColorUC()
        {
            InitializeComponent();
        }

        private void colorPanel_Click(object sender, EventArgs e)
        {
            if (cd.ShowDialog() == DialogResult.OK)
            {
                colorPanel.BackColor = cd.Color;
            }
        }

        private void charColorTextBox_TextChanged(object sender, EventArgs e)
        {
            if (!r.IsMatch(charColorTextBox.Text) && charColorTextBox.Text != "")
            {
                charColorTextBox.Text = previous;
            }
            else
            {
                previous = charColorTextBox.Text;
            }
        }

        private void removeColorChangeButton_Click(object sender, EventArgs e)
        {
            Parent.Controls.Remove(this);
        }

        private void charColorTextBox_Leave(object sender, EventArgs e)
        {
            if (charColorTextBox.Text == "")
            {
                errorProvider1.SetError(charColorTextBox, "Letter cannot be empty");
            }
            else
            {
                errorProvider1.Clear();
            }

            //notify Form1
            Changed?.Invoke(sender, e);
        }
    }
}
