using System.Diagnostics.Contracts;
using System.Runtime.InteropServices;

using JA.Fortran.Arrays;

namespace JA.Fortran.Physics
{
    public unsafe struct FMatrix6 :
        IFormattable,
        IEquatable<FMatrix6>
    {
        private const int _size = 6;
        private const int _count = _size*_size;
        public static int Size { get; } = _size;
        public static int Count { get; } = _count;

        fixed double _data[_count];

        public FMatrix6(FMatrix3 a, FMatrix3 b, FMatrix3 c, FMatrix3 d)
        {
            this=mat6_block(a,b,c,d);
        }
        public FMatrix6(double[] values, int index = 0)
        {
            Contract.Requires(null!=values);
            Contract.Requires(index+_count==values.Length);
            fixed (double* ptr = _data)
            {
                for (int i = 0; i<_count; i++)
                {
                    _data[i]=values[i];
                }
            }
        }
        public static FMatrix6 Zero { get; } = mat6_zeros();
        public static FMatrix6 Identity { get; } = mat6_eye();
        public static FMatrix6 Scalar(double a) => mat6_scalar(a);

        public static explicit operator FMatrix6(double a) => Scalar(a);

        #region Formatting
        public static string DefaultFormatting { get; set; } = "g6";
        public static bool ShowAsTable { get; set; } = true;
        public override string ToString() => ToString(DefaultFormatting);
        public string ToString(string formatting) => ToString(formatting, null);
        public string ToString(string formatting, IFormatProvider formatProvider)
        {
            if (ShowAsTable)
            {
                return ToArray2().ToTableString(HorizontalAlignment.Right, formatting, formatProvider);
            }
            else
            {
                return ToArray2().ToListString(formatting);
            }
        }
        #endregion

        #region IEquatable Members
        /// <summary>
        /// Equality overrides from <see cref="System.Object"/>
        /// </summary>
        /// <param name="obj">The object to compare this with</param>
        /// <returns>False if object is a different type, otherwise it calls <code>Equals(FMatrix3)</code></returns>
        public override bool Equals(object obj)
        {
            return obj is FMatrix6 vector&&Equals(vector);
        }

        public static bool operator ==(FMatrix6 target, FMatrix6 other) => target.Equals(other);
        public static bool operator !=(FMatrix6 target, FMatrix6 other) => !( target==other );


        /// <summary>
        /// Checks for equality among <see cref="FMatrix6"/> classes
        /// </summary>
        /// <param name="other">The other <see cref="FMatrix6"/> to compare it to</param>
        /// <returns>True if equal</returns>
        public bool Equals(FMatrix6 other)
        {
            // return equ_mat6(this, other);
            for (int i = 0; i<_count; i++)
            {
                if (_data[i]!=other._data[i]) return false;
            }
            return true;
        }

        /// <summary>
        /// Calculates the hash code for the <see cref="FMatrix6"/>
        /// </summary>
        /// <returns>The int hash value</returns>
        public override int GetHashCode()
        {
            unchecked
            {
                int hc = -1817952719;
                for (int i = 0; i<_count; i++)
                {
                    hc=( -1521134295 )*hc+_data[i].GetHashCode();
                }
                return hc;
            }
        }

        #endregion

        #region Properties
        public double E11 => _data[0+0];
        public double E12 => _data[3+0];
        public double E13 => _data[6+0];
        public double E21 => _data[0+1];
        public double E22 => _data[3+1];
        public double E23 => _data[6+1];
        public double E31 => _data[0+2];
        public double E32 => _data[3+2];
        public double E33 => _data[6+2];

        public Span<double> AsSpan()
        {
            fixed (double* ptr = _data)
            {
                return new Span<double>(ptr, _count);
            }
        }
        public double[,] ToArray2()
        {
            double[,] result = new double[_size, _size];
            call_mat6_to_array(this, result);
            return result;
        }

        public FMatrix ToVector() => new FMatrix(ToArray2());
        #endregion

        #region Algebra
        public static FMatrix6 Negate(in FMatrix6 a) => neg_mat6(a);
        public static FMatrix6 Scale(double factor, in FMatrix6 a) => mul_scalar_mat6(factor, a);
        public static FMatrix6 Scale(in FMatrix6 a, double divisor) => mul_mat6_scalar(a, divisor);
        public static FMatrix6 Divide(in FMatrix6 a, double divisor) => div_mat6_scalar(a, divisor);
        public static FMatrix6 Add(in FMatrix6 a, in FMatrix6 b) => add_mat6_mat6(a, b);
        public static FMatrix6 Subtract(in FMatrix6 a, in FMatrix6 b) => sub_mat6_mat6(a, b);
        public static FMatrix6 Add(double a, in FMatrix6 b) => add_scalar_mat6(a, b);
        public static FMatrix6 Subtract(double a, in FMatrix6 b) => sub_scalar_mat6(a, b);
        public static FMatrix6 Add(in FMatrix6 a, double b) => add_mat6_scalar(a, b);
        public static FMatrix6 Subtract(in FMatrix6 a, double b) => sub_mat6_scalar(a, b);
        public static FVector6 Product(in FMatrix6 a, in FVector6 b) => mul_mat6_vec6(a, b);
        public static FVector6 Product(in FVector6 a, in FMatrix6 b) => mul_vec6_mat6(a, b);
        public static FMatrix6 Product(in FMatrix6 a, in FMatrix6 b) => mul_mat6_mat6(a, b);
        public static FMatrix6 Inner(in FMatrix6 a, in FMatrix6 b) => dot_mat6_mat6(a, b);
        public readonly double Norm() => norm_mat6(this);
        public readonly double Trace() => trace_mat6(this);
        public readonly double Determinant() => det_mat6(this);
        public readonly FMatrix6 Transpose() => trans_mat6(this);
        public readonly FMatrix6 Inverse() => inv_mat6(this);
        public readonly FVector6 Solve(in FVector6 b) => solve_mat6_vec6(this, b);
        public readonly FMatrix6 Solve(in FMatrix6 b) => solve_mat6_mat6(this, b);
        #endregion

        #region Operators
        public static FMatrix6 operator +(in FMatrix6 a) => a;
        public static FMatrix6 operator -(in FMatrix6 a) => Negate(a);
        public static FMatrix6 operator +(in FMatrix6 a, in FMatrix6 b) => Add(a, b);
        public static FMatrix6 operator -(in FMatrix6 a, in FMatrix6 b) => Subtract(a, b);
        public static FMatrix6 operator +(double a, in FMatrix6 b) => Add(a, b);
        public static FMatrix6 operator -(double a, in FMatrix6 b) => Subtract(a, b);
        public static FMatrix6 operator +(in FMatrix6 a, double b) => Add(a, b);
        public static FMatrix6 operator -(in FMatrix6 a, double b) => Subtract(a, b);
        public static FMatrix6 operator *(double factor, in FMatrix6 a) => Scale(factor, a);
        public static FMatrix6 operator *(in FMatrix6 a, double factor) => Scale(factor, a);
        public static FMatrix6 operator /(in FMatrix6 a, double divisor) => Divide(a, divisor);
        public static FVector6 operator *(in FMatrix6 a, in FVector6 v) => Product(a, v);
        public static FVector6 operator *(in FVector6 v, in FMatrix6 a) => Product(v, a);
        public static FMatrix6 operator *(in FMatrix6 a, in FMatrix6 b) => Product(a, b);
        public static FMatrix6 operator |(in FMatrix6 a, in FMatrix6 b) => Inner(a, b);
        public static FMatrix6 operator ~(in FMatrix6 A) => A.Transpose();
        public static FMatrix6 operator !(in FMatrix6 A) => A.Inverse();
        public static FVector6 operator /(in FVector6 b, in FMatrix6 A) => A.Solve(b);
        public static FMatrix6 operator /(in FMatrix6 B, in FMatrix6 A) => A.Solve(B);
        #endregion

        #region Fortran API
        const string libraryName = FortranMethods.libraryName;

        [DllImport(libraryName, EntryPoint = "mat6_zeros", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix6 mat6_zeros();
        [DllImport(libraryName, EntryPoint = "mat6_eye", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix6 mat6_eye();
        [DllImport(libraryName, EntryPoint = "mat6_scalar", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix6 mat6_scalar(double a);
        [DllImport(libraryName, EntryPoint = "mat6_block", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix6 mat6_block(in FMatrix3 a, in FMatrix3 b, in FMatrix3 c, in FMatrix3 d);
        [DllImport(libraryName, EntryPoint = "add_mat6_mat6", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix6 add_mat6_mat6(in FMatrix6 a, in FMatrix6 b);
        [DllImport(libraryName, EntryPoint = "sub_mat6_mat6", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix6 sub_mat6_mat6(in FMatrix6 a, in FMatrix6 b);
        [DllImport(libraryName, EntryPoint = "neg_mat6", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix6 neg_mat6(in FMatrix6 a);
        [DllImport(libraryName, EntryPoint = "sub_scalar_mat6", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix6 add_scalar_mat6(double a, in FMatrix6 b);
        [DllImport(libraryName, EntryPoint = "sub_scalar_mat6", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix6 sub_scalar_mat6(double a, in FMatrix6 b);
        [DllImport(libraryName, EntryPoint = "add_mat6_scalar", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix6 add_mat6_scalar(in FMatrix6 a, double b);
        [DllImport(libraryName, EntryPoint = "sub_mat6_scalar", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix6 sub_mat6_scalar(in FMatrix6 a, double b);
        [DllImport(libraryName, EntryPoint = "mul_scalar_mat6", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix6 mul_scalar_mat6(double v, in FMatrix6 a);
        [DllImport(libraryName, EntryPoint = "mul_mat6_scalar", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix6 mul_mat6_scalar(in FMatrix6 a, double b);
        [DllImport(libraryName, EntryPoint = "div_mat6_scalar", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix6 div_mat6_scalar(in FMatrix6 a, double b);
        [DllImport(libraryName, EntryPoint = "mul_mat6_vec6", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector6 mul_mat6_vec6(in FMatrix6 a, in FVector6 b);
        [DllImport(libraryName, EntryPoint = "mul_vec6_mat6", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector6 mul_vec6_mat6(in FVector6 a, in FMatrix6 b);
        [DllImport(libraryName, EntryPoint = "mul_mat6_mat6", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix6 mul_mat6_mat6(in FMatrix6 a, in FMatrix6 b);
        [DllImport(libraryName, EntryPoint = "dot_mat6_mat6", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix6 dot_mat6_mat6(in FMatrix6 a, in FMatrix6 b);
        [DllImport(libraryName, EntryPoint = "norm_mat6", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern double norm_mat6(in FMatrix6 a);
        [DllImport(libraryName, EntryPoint = "trace_mat6", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern double trace_mat6(in FMatrix6 a);
        [DllImport(libraryName, EntryPoint = "det_mat6", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern double det_mat6(in FMatrix6 a);
        [DllImport(libraryName, EntryPoint = "trans_mat6", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix6 trans_mat6(in FMatrix6 a);
        [DllImport(libraryName, EntryPoint = "inv_mat6", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix6 inv_mat6(in FMatrix6 a);
        [DllImport(libraryName, EntryPoint = "solve_mat6_vec6", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector6 solve_mat6_vec6(in FMatrix6 a, in FVector6 b);
        [DllImport(libraryName, EntryPoint = "solve_mat6_mat6", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix6 solve_mat6_mat6(in FMatrix6 a, in FMatrix6 b);
        [DllImport(libraryName, EntryPoint = "call_mat6_to_array", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_mat6_to_array(in FMatrix6 a, [Out] double[,] b);
        #endregion

    }
}
