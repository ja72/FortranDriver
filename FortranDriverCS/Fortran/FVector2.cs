using System.Diagnostics.Contracts;
using System.Globalization;
using System.Numerics;
using System.Runtime.InteropServices;
using System.Text;
using JA.Fortran.Arrays;

namespace JA.Fortran
{
    public unsafe struct FVector2 :
        IFormattable,
        IEquatable<FVector2>
    {
        private const int _size = 2;
        public static int Size { get; } = _size;

        fixed double _data[_size];

        #region Factory
        public FVector2(double x, double y)
        {
            this=vec2_values(x, y);
            //fixed (double* ptr = _data)
            //{
            //    _data[0]=x;
            //    _data[1]=y;
            //}
        }
        public FVector2(double[] values, int index = 0)
        {
            Contract.Requires(null!=values);
            Contract.Requires(index+_size==values.Length);
            fixed (double* ptr = _data)
            {
                for (int i = 0; i<_size; i++)
                {
                    _data[i]=values[i];
                }
            }
        }
        public static implicit operator FVector2(double[] values) => new FVector2(values, 0);
        public static FVector2 Zero { get; } = vec2_zeros();
        public static FVector2 EX { get; } = vec2_ux();
        public static FVector2 EY { get; } = vec2_uy();
        public static FVector2 Ones { get; } = vec2_ones();
        public static FVector2 Uniform(ref int seed) => vec2_uniform(ref seed);
        #endregion

        #region Properties
        public double X => _data[0];
        public double Y => _data[1];

        public Span<double> AsSpan()
        {
            fixed (double* ptr = _data)
            {
                return new Span<double>(ptr, _size);
            }
        }
        public double[] ToArray()
        {
            double[] result = new double[_size];
            call_vec2_to_array(this, result);
            return result;
        }

        public FVector ToVector() => new FVector(ToArray());

        public ref double this[int index] => ref _data[index];
        public readonly double Norm() => norm_vec2(this);
        #endregion

        #region Formatting
        public static string DefaultFormat { get; set; } = "g6";
        public static bool ShowAsTable { get; set; } = true;
        public override string ToString() => ToString(DefaultFormatting);
        public string ToString(string formatting) => ToString(formatting, null);
        public string ToString(string formatting, IFormatProvider formatProvider)
        {
            if (ShowAsTable)
            {
                return ToArray().ToTableString(HorizontalAlignment.Right, formatting, formatProvider);
            }
            else
            {
                return ToArray().ToListString(formatting);
            }
        }
        public static string DefaultFormatting { get; set; } = "g6";
        public string ToFixedColumnString(string formatting = null, int width = HelperFunctions.DefaultColumnWidth)
        {
            formatting??=DefaultFormatting;
            StringBuilder sb = new StringBuilder();
            var data = ToArray();
            for (int i = 0; i < data.Length; i++)
            {
                sb.Append('|');
                var f_val = Math.Round(data[i], HelperFunctions.RoundDigits);
                string text = f_val.ToString(formatting, CultureInfo.CurrentCulture.NumberFormat);
                text = text.PadLeft(width);
                if (text.Length > width)
                {
#pragma warning disable IDE0057 // Use range operator
                    text = new string('*', width);
#pragma warning restore IDE0057 // Use range operator
                }
                sb.Append($" {text}");
                sb.AppendLine(" |");
            }
            sb.AppendLine();
            return sb.ToString();            
        }
        #endregion

        #region IEquatable Members
        /// <summary>
        /// Equality overrides from <see cref="System.Object"/>
        /// </summary>
        /// <param name="obj">The object to compare this with</param>
        /// <returns>False if object is a different type, otherwise it calls <code>Equals(FVector2)</code></returns>
        public override bool Equals(object obj)
        {
            return obj is FVector2 vector&&Equals(vector);
        }

        public static bool operator ==(FVector2 target, FVector2 other) => target.Equals(other);
        public static bool operator !=(FVector2 target, FVector2 other) => !( target==other );


        /// <summary>
        /// Checks for equality among <see cref="FVector2"/> classes
        /// </summary>
        /// <param name="other">The other <see cref="FVector2"/> to compare it to</param>
        /// <returns>True if equal</returns>
        public bool Equals(FVector2 other)
        {
            return _data[0]==other._data[0]
                &&_data[1]==other._data[1];
        }

        /// <summary>
        /// Calculates the hash code for the <see cref="FVector2"/>
        /// </summary>
        /// <returns>The int hash value</returns>
        public override int GetHashCode()
        {
            unchecked
            {
                int hc = -1817952719;
                for (int i = 0; i<_size; i++)
                {
                    hc=( -1521134295 )*hc+_data[i].GetHashCode();
                }
                return hc;
            }
        }

        #endregion

        #region Algebra
        public static FVector2 Add(in FVector2 a, in FVector2 b) => add_vec2_vec2(a, b);
        public static FVector2 Subtract(in FVector2 a, in FVector2 b) => sub_vec2_vec2(a, b);
        public static FVector2 Negate(in FVector2 a) => neg_vec2(a);
        public static FVector2 Scale(double factor, in FVector2 a) => mul_scalar_vec2(factor, a);
        public static FVector2 Scale(in FVector2 a, double factor) => mul_vec2_scalar(a, factor);
        public static FVector2 Divide(in FVector2 a, double divider) => div_vec2_scalar(a, divider);
        public static double Dot(in FVector2 a, in FVector2 b) => dot_vec2_vec2(a, b);
        public static FMatrix2 Outer(in FVector2 a, in FVector2 b) => outer_vec2_vec2(a, b);
        public static double Cross(in FVector2 a, in FVector2 b) => cross_vec2_vec2(a, b);
        public static FVector2 Cross(double a, in FVector2 b) => cross_scalar_vec2(a, b);
        public static FVector2 Cross(in FVector2 a, double b) => cross_vec2_scalar(a, b);
        #endregion

        #region Operators
        public static FVector2 operator +(in FVector2 a) => a;
        public static FVector2 operator +(in FVector2 a, in FVector2 b) => Add(a, b);
        public static FVector2 operator -(in FVector2 a) => Negate(a);
        public static FVector2 operator -(in FVector2 a, in FVector2 b) => Subtract(a, b);
        public static FVector2 operator *(double a, in FVector2 b) => Scale(a, b);
        public static FVector2 operator *(in FVector2 a, double b) => Scale(b, a);
        public static FVector2 operator /(in FVector2 a, double b) => Divide(a, b);
        public static double operator |(in FVector2 a, in FVector2 b) => Dot(a, b);
        #endregion

        #region Fortran API
        const string libraryName = FortranMethods.libraryName;

        [DllImport(libraryName, EntryPoint = "vec2_zeros", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector2 vec2_zeros();
        [DllImport(libraryName, EntryPoint = "vec2_ones", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector2 vec2_ones();
        [DllImport(libraryName, EntryPoint = "vec2_ux", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector2 vec2_ux();
        [DllImport(libraryName, EntryPoint = "vec2_uy", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector2 vec2_uy();
        [DllImport(libraryName, EntryPoint = "vec2_values", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector2 vec2_values(double x, double y);
        [DllImport(libraryName, EntryPoint = "vec2_uniform", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector2 vec2_uniform(ref int seed);
        [DllImport(libraryName, EntryPoint = "norm_vec2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern double norm_vec2(FVector2 a);
        [DllImport(libraryName, EntryPoint = "add_vec2_vec2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector2 add_vec2_vec2(in FVector2 a, in FVector2 b);
        [DllImport(libraryName, EntryPoint = "sub_vec2_vec2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector2 sub_vec2_vec2(in FVector2 a, in FVector2 b);
        [DllImport(libraryName, EntryPoint = "neg_vec2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector2 neg_vec2(in FVector2 a);
        [DllImport(libraryName, EntryPoint = "mul_scalar_vec2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector2 mul_scalar_vec2(double s, in FVector2 b);
        [DllImport(libraryName, EntryPoint = "mul_vec2_scalar", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector2 mul_vec2_scalar(in FVector2 a, double s);
        [DllImport(libraryName, EntryPoint = "div_vec2_scalar", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector2 div_vec2_scalar(in FVector2 a, double s);
        [DllImport(libraryName, EntryPoint = "dot_vec2_vec2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern double dot_vec2_vec2(in FVector2 a, in FVector2 b);
        [DllImport(libraryName, EntryPoint = "outer_vec2_vec2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix2 outer_vec2_vec2(in FVector2 a, in FVector2 b);
        [DllImport(libraryName, EntryPoint = "cross_scalar_vec2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector2 cross_scalar_vec2(double a, in FVector2 b);
        [DllImport(libraryName, EntryPoint = "cross_vec2_scalar", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector2 cross_vec2_scalar(in FVector2 a, double b);
        [DllImport(libraryName, EntryPoint = "cross_vec2_vec2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern double cross_vec2_vec2(in FVector2 a, in FVector2 b);
        [DllImport(libraryName, EntryPoint = "call_vec2_to_array", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_vec2_to_array(in FVector2 a, [Out] double[] b);

        #endregion
    }
}
