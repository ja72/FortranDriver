using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Linq;
using System.Numerics;
using System.Runtime.InteropServices;
using System.Text;
using System.Windows.Forms;

using JA.Fortran;
using JA.UI;

namespace JA
{
    public partial class RunningForm1 : Form
    {
        static readonly Random rng = new Random();
        readonly FpsCounter clock;
        readonly DisplayChart chart;

        #region Windows API - User32.dll
        [StructLayout(LayoutKind.Sequential)]
        public struct WinMessage
        {
            public IntPtr hWnd;
            public Message msg;
            public IntPtr wParam;
            public IntPtr lParam;
            public uint time;
            public System.Drawing.Point p;
        }

        [System.Security.SuppressUnmanagedCodeSecurity] // We won't use this maliciously
        [DllImport("User32.dll", CharSet = CharSet.Auto)]
        public static extern bool PeekMessage(out WinMessage msg, IntPtr hWnd, uint messageFilterMin, uint messageFilterMax, uint flags);
        #endregion

        public RunningForm1()
        {
            InitializeComponent();

            //Initialize the machine
            this.clock=new FpsCounter();
            this.chart = new DisplayChart(pic);
        }

        protected override void OnLoad(EventArgs e)
        {
            base.OnLoad(e);

            chart.Elements.Add(new DrawAxis());

            const double pi = Math.PI;
            double x_range = 4*pi;
            this.chart.SetLimits(0, -1, x_range, 1);
            double ox = 0;
            var fun = new DrawFunction((x) => 0.5*Math.Sin(5*(x-ox)/3.2) + 0.5*Math.Cos(5*x/2), 0, x_range)
            {
                Color = Color.Red,
                Update = (chart) => ox+=0.01,
            };
            var x = FVector.LinearSpace(0.0, x_range, 36).ToArray();
            var y = x.Select((xi) => -1 + 2* rng.NextDouble()).ToArray();
            var spl = new FSpline(x, y);
            var dspl = new DrawSpline(spl)
            {
                Color = Color.Blue,
                Update = (chart) =>
                {
                    double xi = spl[18].X;
                    spl[18] = new FVector2(xi, -1 + 2* rng.NextDouble());
                },
            };
            this.chart.Elements.Add(dspl);

            MainLoop();
        }

        void UpdateMachine()
        {
            this.chart.Update();
            pic.Refresh();
        }

        #region Main Loop
        public void MainLoop()
        {
            // Hook the application's idle event
            System.Windows.Forms.Application.Idle += new EventHandler(OnApplicationIdle);
            //System.Windows.Forms.Application.Run(TrackForm);
        }

        private void OnApplicationIdle(object sender, EventArgs e)
        {
            while (AppStillIdle)
            {
                // Render a frame during idle time (no messages are waiting)
                UpdateMachine();
            }
        }

        private bool AppStillIdle
        {
            get
            {
                WinMessage msg;
                return !PeekMessage(out msg, IntPtr.Zero, 0, 0, 0);
            }
        }

        #endregion

        private void pic_SizeChanged(object sender, EventArgs e)
        {
            pic.Refresh();
        }

        private void pic_Paint(object sender, PaintEventArgs e)
        {
            // Show FPS counter
            var fps = clock.Measure();
            var text = $"{fps:F2} fps";
            var sz = e.Graphics.MeasureString(text, SystemFonts.DialogFont);
            var pt = new PointF(pic.Width-1 - sz.Width - 4, 4);
            e.Graphics.DrawString(text, SystemFonts.DialogFont, Brushes.Black, pt);

            // Draw the machine
            e.Graphics.SmoothingMode = System.Drawing.Drawing2D.SmoothingMode.AntiAlias;

            chart.DrawChart(e.Graphics);

        }

        private void pic_MouseClick(object sender, MouseEventArgs e)
        {
        }
    }

}
