using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace L_system
{
    public partial class RuleUC : UserControl
    {
        public ErrorProvider ErrorProvider = new ErrorProvider();
        public EventHandler Changed { get; set; }
        public RuleUC()
        {
            InitializeComponent();
        }

        private void removeRuleButton_Click(object sender, EventArgs e)
        {
            Parent.Controls.Remove(this);
            Changed?.Invoke(sender, e);
        }

        private void textBox_Leave(object sender, EventArgs e)
        {
            TextBox s = (TextBox) sender;
            if (s.Text == "")
            {
                ErrorProvider.SetError(s, "Rule cannot be empty");
            }
            else if (!Utils.LsystemRegex.IsMatch(s.Text))
            {
                ErrorProvider.SetError(s, "Rule contains disallowed characters");
            }
            else
            {
                ErrorProvider.Clear();
            }

            Changed?.Invoke(sender, e);
        }
    }
}
