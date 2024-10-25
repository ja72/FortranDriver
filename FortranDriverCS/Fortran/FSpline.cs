using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.ComponentModel;
using System.Globalization;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;

using static System.Net.Mime.MediaTypeNames;

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
            var vec = FVector.LinearSpace(x_start, x_end, n);
            vec.CopyTo(this.x, 0);

            this.y = new double[n];
            this.ypp = new double[n];
            call_spline_calc_ypp_domain(n, x_start, x_end, this.y, this.ypp);
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

            call_spline_calc_ypp_array(x.Length, this.x, this.y, this.ypp);
        }

        protected void OnRefresh(EventArgs eventArgs)
        {
            Refresh?.Invoke(this, eventArgs);
        }
        public void DoRefresh()
        {            
            call_spline_calc_ypp_array(x.Length, this.x, this.y, this.ypp);
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
            call_spline_interpolate_array(X.Length, this.x, this.y, this.ypp, m, xe, ye, yppe);
            return new FSpline(xe, ye, yppe);
        }

        public FVector2 this[int index]
        {
            get
            {
                return new FVector2(x[index], y[index]);
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
            var vec = FVector.LinearSpace(x_start, x_end, count);
            vec.CopyTo(xe, 0);
            double[] ye = new double[count];
            double[] yppe = new double[count];
            call_spline_interpolate_domain(X.Length, this.x, this.y, this.ypp, count, x_start, x_end, ye, yppe);
            return new FSpline(xe, ye, yppe);
        }

        public SplinePoint Interpolate(double xe)
        {
            call_spline_interpolate_point(X.Length, this.x, this.y, this.ypp, xe, out var ye, out var ype, out var yppe);
            return new SplinePoint(xe, ye, ype, yppe);
        }
        #region Formatting
        public override string ToString() => ToString(DefaultFormatting);
        public string ToString(string formatting) => ToString(formatting, null);
        public string ToString(string formatting, IFormatProvider formatProvider)
        {
            return $"SPLINE(n={n}, X={x[0].ToString(formatting,formatProvider)}…{x[n-1].ToString(formatting,formatProvider)})";
        }
        #endregion
        public static string DefaultFormatting { get; set; } = "g6";
        public string ToFixedColumnString(string formatting = null, int width = HelperFunctions.DefaultColumnWidth)
        {
            formatting??=DefaultFormatting;
            StringBuilder sb = new StringBuilder();

            string Fmt(double x)
            {
                var f_val = Math.Round(x, HelperFunctions.RoundDigits);
                string text = f_val.ToString(formatting, CultureInfo.CurrentCulture.NumberFormat);
                return text;
            }

            void AddLine(string x, string y, string ypp)
            {
                if (x.Length>width) x=new string('*', width);
                if (y.Length>width) y=new string('*', width);
                if (ypp.Length>width) ypp=new string('*', width);
                sb.Append('|');
                sb.Append($" {x}".PadLeft(width));
                sb.Append(" |");
                sb.Append($" {y}".PadLeft(width));
                sb.Append(" |");
                sb.Append($" {ypp}".PadLeft(width));
                sb.AppendLine(" |");
            }

            AddLine("x", "y", "z");
            for (int i = 0; i < n; i++)
            {
                AddLine(Fmt(x[i]), Fmt(y[i]), Fmt(ypp[i]));
            }
            sb.AppendLine();
            return sb.ToString();            
        }


        #region Fortran API        
        /// <summary>
        /// Calculate the spline coefficients from an array of coordinates.
        /// <code><![CDATA[
        /// pure subroutine call_spline_calc_ypp_array(n,x,y,ypp) bind(c)
        /// ]]></code>
        /// </summary>
        /// <param name="n">The numnber of nodes.</param>
        /// <param name="x">The x-coord of the nodes.</param>
        /// <param name="y">The y-coord of the nodes.</param>
        /// <param name="ypp">The ypp coefficients (calculated).</param>
        [DllImport(FortranMethods.libraryName, EntryPoint = "call_spline_calc_ypp_array", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_spline_calc_ypp_array(int n, double[] x, double[] y, [Out] double[] ypp);
        /// <summary>
        /// Calculate the spline coefficients from range of values.
        /// <code><![CDATA[
        /// pure subroutine call_spline_calc_ypp_domain(n,x_start, x_end,y,ypp) bind(c)
        /// ]]></code>
        /// </summary>
        /// <param name="n">The numnber of nodes.</param>
        /// <param name="x_start">The start of the x-coord.</param>
        /// <param name="x_end">The end of the x-coord.</param>
        /// <param name="y">The y-coord of the nodes.</param>
        /// <param name="ypp">The ypp coefficients (calculated).</param>
        [DllImport(FortranMethods.libraryName, EntryPoint = "call_spline_calc_ypp_domain", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_spline_calc_ypp_domain(int n, double x_start, double x_end, double[] y, [Out] double[] ypp);

        [DllImport(FortranMethods.libraryName, EntryPoint = "call_spline_interpolate_point", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        // pure subroutine call_spline_intepolate_values(n,x,y,ypp,xe,ye,ype,yppe)
        internal static extern void call_spline_interpolate_point(int n, double[] x, double[] y, double[] ypp, double xe, [Out] out double ye, [Out] out double ype, [Out] out double yppe);

        [DllImport(FortranMethods.libraryName, EntryPoint = "call_spline_interpolate_array", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        // pure subroutine call_spline_interpolate_array(n,x,y,ypp,m,xe,ye,yppe) bind(c)
        internal static extern void call_spline_interpolate_array(int n, double[] x, double[] y, double[] ypp, int m, double[] xe, [Out] double[] ye, [Out] double[] yppe);

        [DllImport(FortranMethods.libraryName, EntryPoint = "call_spline_interpolate_domain", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        // pure subroutine call_spline_interpolate_domain(n,x,y,ypp,m,x_start,x_end,ye,yppe) bind(c)
        internal static extern void call_spline_interpolate_domain(int n, double[] x, double[] y, double[] ypp, int m, double x_start, double x_end, [Out] double[] ye, [Out] double[] yppe);

        #endregion
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
