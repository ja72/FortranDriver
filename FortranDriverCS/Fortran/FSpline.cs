using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.ComponentModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace JA.Fortran
{
    public class FSpline
    {
        public event EventHandler Refresh;

        readonly int n;
        readonly double[] x;
        readonly double[] y;
        readonly double[] ypp;

        private FSpline(double[] x, double[] y, double[] ypp)
        {
            this.n = y.Length;
            this.x = x;
            this.y = y;
            this.ypp = ypp;
        }
        public FSpline(double x_start, double x_end, double[] y)
        {
            this.n = y.Length;
            this.x = new double[n];
            HelperFunctions.LinearSpace(n, x_start, x_end).CopyTo(this.x, 0);

            this.y = new double[n];
            this.ypp = new double[n];
            FortranMethods.spline_calc_ypp_domain(n, x_start, x_end, this.y, this.ypp);
        }
        public FSpline(double[] x, double[] y)
        {
            if (x.Length != y.Length)
            {
                throw new ArgumentOutOfRangeException(nameof(x), $"Expecing {y.Length} elements.");
            }
            this.n = y.Length;
            this.x = new double[n];
            this.y = new double[n];
            this.ypp = new double[n];

            x.CopyTo(this.x, 0);
            y.CopyTo(this.y, 0);

            FortranMethods.spline_calc_ypp_array(x.Length, this.x, this.y, this.ypp);
        }

        protected void OnRefresh(EventArgs eventArgs)
        {
            Refresh?.Invoke(this, eventArgs);
        }
        public void DoRefresh()
        {            
            FortranMethods.spline_calc_ypp_array(x.Length, this.x, this.y, this.ypp);
            OnRefresh(new EventArgs());
        }

        public ImmutableArray<double> X => x.ToImmutableArray();
        public ImmutableArray<double> Y => y.ToImmutableArray();
        public ImmutableArray<double> Ypp => ypp.ToImmutableArray();
        public FSpline Interpolate(double[] x)
        {
            int m = x.Length;
            double[] xe = new double[m];
            x.CopyTo(xe, 0);
            double[] ye = new double[m];
            double[] yppe = new double[m];
            FortranMethods.spline_interpolate_array(X.Length, this.x, this.y, this.ypp, m, xe, ye, yppe);
            return new FSpline(xe, ye, yppe);
        }

        public Vector2 this[int index]
        {
            get
            {
                return new Vector2(x[index], y[index]);
            } set
            {
                double xi = x[index], yi = y[index];
                if (xi != value.X || yi != value.Y)
                {
                    x[index] = value.X;
                    y[index] = value.Y;
                    DoRefresh();
                }
            }
        }

        public FSpline Interpolate(double x_start, double x_end, int count)
        {
            double[] xe = new double[count];
            HelperFunctions.LinearSpace(count, x_start, x_end).CopyTo(xe, 0);
            double[] ye = new double[count];
            double[] yppe = new double[count];
            FortranMethods.spline_interpolate_domain(X.Length, this.x, this.y, this.ypp, count, x_start, x_end, ye, yppe);
            return new FSpline(xe, ye, yppe);
        }

        public SplinePoint Interpolate(double xe)
        {
            FortranMethods.spline_intepolate_values(X.Length, this.x, this.y, this.ypp, xe, out var ye, out var ype, out var yppe);
            return new SplinePoint(xe, ye, ype, yppe);
        }
    }

    public readonly struct SplinePoint
    {
        readonly double _x;
        readonly double _y;
        readonly double _yp;
        readonly double _ypp;

        public SplinePoint(double x, double y, double yp, double ypp)
        {
            this._x=x;
            this._y=y;
            this._yp=yp;
            this._ypp=ypp;
        }

        public double X => this._x;
        public double Y => this._y;
        public double Yp => this._yp;
        public double Ypp => this._ypp;
    }
}
