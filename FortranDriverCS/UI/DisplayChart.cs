using System.Data;
using System.Drawing.Drawing2D;
using System.Xml;

namespace JA.UI
{
    public class DisplayChart
    {
        [Flags]
        public enum StartEndSpec
        {
            None = 0,
            Start = 1,
            End = 2,
            Both = 3
        }
        public DisplayChart(Control target, double minX, double minY, double maxX, double maxY, Padding padding)
        {
            Elements = new List<IDrawElement>();
            Target = target ?? throw new ArgumentNullException(nameof(target));
            Stroke = new Pen(Color.Black, 0);
            Fill = new SolidBrush(Color.Black);
            Font = SystemFonts.SmallCaptionFont;
            Padding = padding;
            SetLimits(minX, minY, maxX, maxY);
        }
        public DisplayChart(Control target, double minX, double minY, double maxX, double maxY)
            : this(target, minX, minY, maxX, maxY, new Padding(8))
        { }
        public DisplayChart(Control target)
            : this(target, 0, 0, 1, 1)
        { }

        public void SetLimits(double minX, double minY, double maxX, double maxY)
        {
            MinX = minX;
            MinY = minY;
            MaxX = maxX;
            MaxY = maxY;
        }
        public Control Target { get; }        

        public int HorizontalPixels => Target.ClientSize.Width - Padding.Horizontal;
        public int VerticalPixels => Target.ClientSize.Height - Padding.Vertical;
        public int HorizontalCenter => Padding.Left + Target.ClientSize.Width / 2;
        public int VerticalCenter => Padding.Top + Target.ClientSize.Height / 2;

        public double MinX { get; set; }
        public double MinY { get; set; }
        public double MaxX { get; set; }
        public double MaxY { get; set; }

        public double SpanX => MaxX-MinX;
        public double SpanY => MaxY-MinY;

        public double MidX => (MinX + MaxX) / 2;
        public double MidY => (MinY + MaxY) / 2;

        //public float ScaleX => 

        public Padding Padding { get; set; }

        public Pen Stroke { get; set; }
        public Brush Fill { get; set; }
        public Color FillColor
        {
            get
            {
                if (Fill is SolidBrush solid)
                {
                    return solid.Color;
                }
                return Color.Empty;
            }
            set
            {
                if (Fill is SolidBrush solid)
                {
                    solid.Color = value;
                }
                else
                {
                    Fill = new SolidBrush(value);
                }
            }
        }

        public Font Font { get; set; }

        public List<IDrawElement> Elements { get; }

        public void Update()
        {
            foreach (var item in Elements)
            {
                item.Update(this);
            }
        }

        public PointF GetPointF(Vector2 vector)
        {
            int wt = Target.ClientSize.Width - Padding.Horizontal, ht = Target.ClientSize.Height - Padding.Vertical;
            int ox = Padding.Left + Target.ClientSize.Width / 2, oy = Padding.Top + Target.ClientSize.Height / 2;

            float px = Padding.Left + wt * (float)(vector.X - MinX) / (float)(MaxX - MinX);
            float py = Padding.Top + ht * (float)(vector.Y - MaxY) / (float)(MinY - MaxY);

            return new PointF(px, py);
        }

        public Vector2 GetVector2(Point point)
        {
            int px = point.X - Padding.Left, py = point.Y - Padding.Top;
            int wt = Target.ClientSize.Width - Padding.Horizontal, ht = Target.ClientSize.Height - Padding.Vertical;

            double x = MinX + px * (MaxX - MinX) / wt;
            double y = MaxY + py * (MinY - MaxY) / ht;

            return new Vector2(x, y);
        }

        public PointF[] GetPointFs(params Vector2[] vectors)
        {
            return vectors.Select((v) => GetPointF(v)).ToArray();
        }
        public Vector2[] GetVector2s(params Point[] points)
        {
            return points.Select((p) => GetVector2(p)).ToArray();
        }

        public void AddEndArrow(int size = 6)
            => Stroke.CustomEndCap = new AdjustableArrowCap(size / 3, size);
        public void AddStartArrow(int size = 6)
            => Stroke.CustomStartCap = new AdjustableArrowCap(size / 3, size);
        public void RemoveEndArrow()
            => Stroke.EndCap = LineCap.NoAnchor;
        public void RemoveStartArrow()
            => Stroke.StartCap = LineCap.NoAnchor;

        public void DrawChart(Graphics g)
        {
            //Vector2[] nodes = new Vector2[] {
            //    new Vector2(MinX, MinY),
            //    new Vector2(MaxX, MinY),
            //    new Vector2(MaxX, MaxY),
            //    new Vector2(MinX, MaxY),
            //};

            //PointF[] polygon = GetPointFs(nodes);

            var polygon = new Point[] {
                new Point( Padding.Left, Padding.Top ),
                new Point( Padding.Left + HorizontalPixels, Padding.Top ),
                new Point( Padding.Left + HorizontalPixels, Padding.Top + VerticalPixels),
                new Point( Padding.Left, Padding.Top + VerticalPixels),
            };

            g.DrawPolygon(Pens.Gray, polygon);

            foreach (var item in Elements)
            {
                item.DrawElement(g, this);
            }
        }

        public void DrawPoint(Graphics g, Vector2 vector, int size = 6)
        {
            var point = GetPointF(vector);
            g.DrawEllipse(Stroke, point.X - size / 2, point.Y - size / 2, size, size);
        }
        public void DrawPoint(Graphics g, double x, double y, int size = 6)
            => DrawPoint(g, new Vector2(x, y), size);
        public void FillPoint(Graphics g, Vector2 vector, int size = 6)
        {
            var point = GetPointF(vector);
            g.FillEllipse(Fill, point.X - size / 2, point.Y - size / 2, size, size);
        }
        public void DrawPoints(Graphics g, double[] x, double[] y, int size = 6)
            => DrawPoints(g, Enumerable.Zip(x, y, (xi, yi) => new Vector2(xi, yi)).ToArray(), size);
        public void FillPoints(Graphics g, double[] x, double[] y, int size = 6)
            => FillPoints(g, Enumerable.Zip(x, y, (xi, yi) => new Vector2(xi, yi)).ToArray(), size);
        public void DrawPoints(Graphics g, Vector2[] vectors, int size = 6)
        {
            var points = GetPointFs(vectors);
            foreach (var point in points)
            {
                g.DrawEllipse(Stroke, point.X - size / 2, point.Y - size / 2, size, size);
            }
        }
        public void FillPoints(Graphics g, Vector2[] vectors, int size = 6)
        {
            var points = GetPointFs(vectors);
            foreach (var point in points)
            {
                g.FillEllipse(Fill, point.X - size / 2, point.Y - size / 2, size, size);
            }
        }
        public void DrawLine(Graphics g, Vector2 from, Vector2 to)
        {
            var p_from = GetPointF(from);
            var p_to = GetPointF(to);

            g.DrawLine(Stroke, p_from, p_to);
        }
        public void DrawLine(Graphics g, double fromX, double fromY, double toX, double toY)
            => DrawLine(g, new Vector2(fromX, fromY), new Vector2(toX, toY));

        public void DrawLineArrow(Graphics g, Vector2 from, Vector2 to, StartEndSpec spec = StartEndSpec.End)
        {
            var p_from = GetPointF(from);
            var p_to = GetPointF(to);

            if (spec.HasFlag(StartEndSpec.Start))
            {
                AddStartArrow();
            }
            if (spec.HasFlag(StartEndSpec.End))
            {
                AddEndArrow();
            }

            g.DrawLine(Stroke, p_from, p_to);

            if (spec.HasFlag(StartEndSpec.Start))
            {
                RemoveStartArrow();
            }
            if (spec.HasFlag(StartEndSpec.End))
            {
                RemoveEndArrow();
            }
        }
        public void DrawLineArrow(Graphics g, double fromX, double fromY, double toX, double toY, StartEndSpec spec = StartEndSpec.End)
            => DrawLineArrow(g, new Vector2(fromX, fromY), new Vector2(toX, toY), spec);

        public void DrawPolygon(Graphics g, Vector2[] vectors, bool drawNodes = false, int size = 6)
        {
            var points = GetPointFs(vectors);
            g.DrawPolygon(Stroke, points);

            if (drawNodes)
            {
                foreach (var vector in vectors)
                {
                    DrawPoint(g, vector, size);
                }
            }
        }
        public void FillPolygon(Graphics g, Vector2[] vectors, bool drawNodes = false, int size = 6)
        {
            var points = GetPointFs(vectors);
            g.FillPolygon(Fill, points);

            if (drawNodes)
            {
                foreach (var vector in vectors)
                {
                    FillPoint(g, vector, size);
                }
            }
        }
        public void DrawLines(Graphics g, Vector2[] vectors, bool drawNodes = false, int size = 6)
        {
            var points = GetPointFs(vectors);
            g.DrawLines(Stroke, points);

            if (drawNodes)
            {
                foreach (var vector in vectors)
                {
                    DrawPoint(g, vector, size);
                }
            }
        }
        public void DrawLines(Graphics g, double[] x, double[] y, bool drawNodes = false, int size = 6)
        {
            var vectors = Enumerable.Zip(x, y, (xi, yi) => new Vector2(xi, yi)).ToArray();
            DrawLines(g, vectors, drawNodes, size);
        }
        public void DrawCurve(Graphics g, Vector2[] vectors, bool drawNodes = false, int size = 6, bool closed = false)
        {
            var points = GetPointFs(vectors);
            if (closed)
            {
                g.DrawClosedCurve(Stroke, points);
            }
            else
            {
                g.DrawCurve(Stroke, points);
            }
            if (drawNodes)
            {
                foreach (var vector in vectors)
                {
                    DrawPoint(g, vector, size);
                }
            }
        }
        public void DrawCurve(Graphics g, double[] x, double[] y, bool drawNodes = false, int size = 6, bool closed = false)
        {
            var vectors = Enumerable.Zip(x, y, (xi, yi) => new Vector2(xi, yi)).ToArray();
            DrawCurve(g, vectors, drawNodes, size, closed);
        }

        public void DrawText(Graphics g, Vector2 vector, string text, ContentAlignment alignment = ContentAlignment.BottomRight, int space = 6)
        {
            var point = GetPointF(vector);
            var size = g.MeasureString(text, Font);
            float x = point.X, y = point.Y;

            switch (alignment)
            {
                case ContentAlignment.TopLeft:
                {
                    x -= size.Width - space;
                    y -= size.Height - space;
                }
                break;
                case ContentAlignment.TopCenter:
                {
                    x -= size.Width / 2;
                    y -= size.Height - space;
                }
                break;
                case ContentAlignment.TopRight:
                {
                    x += space;
                    y -= size.Height - space;
                }
                break;
                case ContentAlignment.MiddleLeft:
                {
                    x -= size.Width - space;
                    y -= size.Height / 2;
                }
                break;
                case ContentAlignment.MiddleCenter:
                {
                    x -= size.Width / 2;
                    y -= size.Height / 2;
                }
                break;
                case ContentAlignment.MiddleRight:
                {
                    x += space;
                    y -= size.Height / 2;
                }
                break;
                case ContentAlignment.BottomLeft:
                {
                    x -= size.Width - space;
                    y += space;
                }
                break;
                case ContentAlignment.BottomCenter:
                {
                    x -= size.Width / 2;
                    y += space;
                }
                break;
                case ContentAlignment.BottomRight:
                {
                    x += space;
                    y += space;
                }
                break;
                default:
                throw new NotSupportedException(alignment.ToString());
            }

            g.DrawString(text, Font, Fill, x, y);
        }

    }
}
