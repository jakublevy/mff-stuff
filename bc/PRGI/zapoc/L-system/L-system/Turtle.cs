using System;
using System.CodeDom;
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace L_system
{
    class Turtle : IDisposable
    {
        public Panel Panel { get; set; }
        public string Sentence { get; set; }
        public double LineLength { get; set; }
        public double RotationAngle { get; set; }
        public Point StartingPoint { get; set; }

        private Point directionPoint;

        public Point DirectionPoint
        {
            get => directionPoint;
            set
            {
                if (Utils.AbsOfPoint(value) == new Point(1, 0) || Utils.AbsOfPoint(value) == new Point(0, 1))
                {
                    directionPoint = value;
                }
                else
                {
                    throw new ArgumentException("Invalid value of type Point");
                }
            }
        }
        public double LineLengthCoefficient { get; set; }
        public Dictionary<char, Color> Colors { get; set; }

        private List<DrawInfo> drawInfo = new List<DrawInfo>(); //all information needed to redraw fractal

        /// <summary>
        /// Constructor for Turtle class
        /// </summary>
        /// <param name="sentence"> drawing grammar </param>
        /// <param name="panel"> Control used for drawing </param>
        /// <param name="lineLength"> length of the drawing line </param>
        /// <param name="rotationAngle"> determines the rotation amount when + or - </param>
        /// <param name="startingPoint">the starting location to draw</param>
        /// <param name="directionPoint"> (1,0) means "right", (-1, 0) means left, (0,1) means down, (0,-1) means up </param>
        /// <param name="colors"> associated colors with moving letter </param>
        /// <param name="lineLengthCofficient"> coefficient of shortening the drawing line </param>
        public Turtle(string sentence, Panel panel, double lineLength, double rotationAngle, Point startingPoint,
            Point directionPoint, Dictionary<char, Color> colors, double lineLengthCofficient = 1)
        {
            Sentence = sentence;
            Panel = panel;
            Panel.Paint += Panel_Paint;
            LineLength = lineLength;
            RotationAngle = rotationAngle;
            StartingPoint = startingPoint;
            DirectionPoint = directionPoint;
            Colors = colors;
            LineLengthCoefficient = lineLengthCofficient;
        }

        private void Panel_Paint(object sender, PaintEventArgs e)
        {
            e.Graphics.SmoothingMode = SmoothingMode.AntiAlias;
            for (int i = 0; i < drawInfo.Count; ++i)
            {
                e.Graphics.DrawLine(drawInfo[i].Pen, drawInfo[i].Start.X, drawInfo[i].Start.Y, drawInfo[i].End.X, drawInfo[i].End.Y);
            }
        }

        public void Render()
        {
            State currentState = new State {Location = StartingPoint, CurrentAngle = 0};
            Stack<State> states = new Stack<State>();
            for (int i = 0; i < Sentence.Length; ++i)
            {
                if (Lsystem.IsNotMovingChar(Sentence[i]))
                {
                    switch (Sentence[i]) 
                    {
                        case '+':
                            currentState.CurrentAngle += RotationAngle;
                            break;
                        case '-':
                            currentState.CurrentAngle -= RotationAngle;
                            break;
                        case '[':
                            states.Push(currentState);
                            break;
                        case ']':
                            if (states.Count > 0)
                            {
                                currentState = states.Pop();
                            }
                            else
                            {
                                Utils.ShowErrorMessageBox("Bad grammar");
                                return;
                            }
                            break;
                    }
                }
                else
                {   //moving
                    PointF shift = Utils.ScaleVector(Utils.RotateVector(DirectionPoint, currentState.CurrentAngle), LineLength);
                    if (char.IsLower(Sentence[i]))
                    {
                        currentState.Location = Utils.AddVectors(currentState.Location, shift);
                    }
                    else //drawing 
                    {
                        Color c = Color.Black;
                        if (Colors.ContainsKey(Sentence[i]))
                        {
                            c = Colors[Sentence[i]];
                        }

                        PointF newLocation = Utils.AddVectors(currentState.Location, shift);
                        Pen p = new Pen(c);
                        
                        drawInfo.Add(new DrawInfo {Start = currentState.Location, End = newLocation, Pen = p});
                        currentState.Location = newLocation;
                    }
                }

                LineLength *= LineLengthCoefficient;
            }
            Panel.Invalidate();
        }

        /// <summary>
        /// Disconnects the panel from Turtle and releases memory
        /// </summary>
        public void Dispose()
        {
            Panel.Paint -= Panel_Paint;
            Colors.Clear();
            drawInfo.Clear();

        }
    }

    struct State
    {
        public PointF Location { get; set; }
        public double CurrentAngle { get; set; }
    }

    struct DrawInfo
    {
        public PointF Start { get; set; }
        public PointF End { get; set; }
        public Pen Pen { get; set; }
    }
}
