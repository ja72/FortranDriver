using System.Diagnostics;

using JA.Fortran;

using Windows.Management.Deployment;

namespace JA.UI
{
    public interface IDrawElement
    {
        void DrawElement(Graphics g, DisplayChart chart);
        Action<DisplayChart> Update { get; set; }
    }
    public class DrawAxis : IDrawElement
    {
        public void DrawElement(Graphics g, DisplayChart chart)
        {
            double x1 = chart.MinX, y1 = chart.MinY;
            double x2 = chart.MaxX, y2 = chart.MaxY;

            chart.Stroke.Color = Color.Black;

            chart.DrawLineArrow(g, chart.MidX - chart.SpanX/2, 0, chart.MidX + chart.SpanX/2, 0, DisplayChart.StartEndSpec.End);
            chart.DrawLineArrow(g, 0, chart.MidY - chart.SpanY/2, 0, chart.MidY + chart.SpanY/2, DisplayChart.StartEndSpec.End);

            Update = (chart) => { };
        }

        public Action<DisplayChart> Update { get; set; }
    }
    public class DrawFunction : IDrawElement
    {
        private Func<double, double> function;

        public DrawFunction(Func<double, double> function, double xMin, double xMax)
        {
            Function=function??throw new ArgumentNullException(nameof(function));
            XMin=xMin;
            XMax=xMax;
            Color = Color.Black;

            Update = (chart) => { };
        }

        public Func<double, double> Function
        {
            get => function;
            set => function=value;
        }
        public double XMin { get; set; }
        public double XMax { get; set; }
        public Color Color { get; set; }

        public void DrawElement(Graphics g, DisplayChart chart)
        {
            if (Function == null)
            {
                return;
            }

            int count = chart.HorizontalPixels;
            Vector2[] nodes = new Vector2[count+1];
            for (int i = 0; i <= count; i++)
            {
                double x = XMin + ((XMax-XMin)*i)/count;
                double y = Function(x);

                nodes[i] = new Vector2(x, y);
            }
            var prevColor = chart.Stroke.Color;
            chart.Stroke.Color = Color;
            chart.DrawLines(g, nodes, false);
            chart.Stroke.Color = prevColor;
        }

        public Action<DisplayChart> Update { get; set; }
    }

    public class DrawSpline : IDrawElement
    {
        readonly FSpline spline;
        public Color Color { get; set; }

        public DrawSpline(FSpline spline)
        {
            this.spline=spline??throw new ArgumentNullException(nameof(spline));
            Color = Color.Black;
            Update = (chart) => { };
        }

        public void DrawElement(Graphics g, DisplayChart chart)
        {
            var prevColor = chart.Stroke.Color;
            chart.Stroke.Color = Color.Purple;
            chart.DrawPoints(g, spline.X.ToArray(), spline.Y.ToArray());
            int count = chart.HorizontalPixels;
            double[] x = HelperFunctions.LinearSpace(count, spline.X.First(), spline.X.Last());
            var cs = spline.Interpolate(x);
            chart.Stroke.Color = Color;
            chart.DrawCurve(g, cs.X.ToArray(), cs.Y.ToArray(), false);
            chart.Stroke.Color = prevColor;
        }
        public Action<DisplayChart> Update { get; set; }
    }
}
