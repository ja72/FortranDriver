using System.Diagnostics.Contracts;
using System.Runtime.InteropServices;

using JA.Fortran.Arrays;

namespace JA.Fortran
{
    public unsafe struct FMatrix2 :
        IFormattable,
        IEquatable<FMatrix2>
    {
        private const int _size = 2;
        private const int _count = _size*_size;
        public static int Size { get; } = _size;
        public static int Count { get; } = _count;

        fixed double _data[_count];

        #region Factory
        public FMatrix2(double a11, double a12, double a21, double a22)
        {
            this=mat2_values(a11, a12, a21, a22);
            //fixed (double* ptr = _data)
            //{
            //    _data[0]=a11;
            //    _data[1]=a21;
            //    _data[2]=a12;
            //    _data[3]=a22;
            //}
        }
        public FMatrix2(double[] values, int index = 0)
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
        public static FMatrix2 Zero { get; } = mat2_zeros();
        public static FMatrix2 Identity { get; } = mat2_eye();
        public static FMatrix2 Ones { get; } = mat2_ones();

        public static FMatrix2 Diagonal(double a11, double a22)
            => new FMatrix2(a11, 0, 0, a22);
        public static FMatrix2 Scalar(double a)
            => new FMatrix2(a, 0, 0, a);
        public static FMatrix2 Symmetric(double a11, double a12, double a22)
            => new FMatrix2(a11, a12, a12, a22);
        public static FMatrix2 SkewSymmetric(double a12)
            => new FMatrix2(0, -a12, a12, 0);

        public static FMatrix2 Uniform(ref int seed) => mat2_uniform(ref seed);

        public static explicit operator FMatrix2(double a) => Scalar(a); 

        public static implicit operator FMatrix(FMatrix2 matrix) => matrix.ToMatrix();
        #endregion

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
        /// <returns>False if object is a different type, otherwise it calls <code>Equals(FMatrix2)</code></returns>
        public override bool Equals(object obj)
        {
            return obj is FMatrix2 vector&&Equals(vector);
        }

        public static bool operator ==(FMatrix2 target, FMatrix2 other) => target.Equals(other);
        public static bool operator !=(FMatrix2 target, FMatrix2 other) => !( target==other );


        /// <summary>
        /// Checks for equality among <see cref="FMatrix2"/> classes
        /// </summary>
        /// <param name="other">The other <see cref="FMatrix2"/> to compare it to</param>
        /// <returns>True if equal</returns>
        public bool Equals(FMatrix2 other)
        {
            return _data[0]==other._data[0]
                &&_data[1]==other._data[1]
                &&_data[2]==other._data[2]
                &&_data[3]==other._data[3];
        }

        /// <summary>
        /// Calculates the hash code for the <see cref="FMatrix2"/>
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
        public double A11 => _data[0];
        public double A12 => _data[2];
        public double A21 => _data[1];
        public double A22 => _data[3];

        public readonly double Trace() => trace_mat2(this);
        public readonly double Determinant() => det_mat2(this);
        public Span<double> AsSpan()
        {
            fixed (double* ptr = _data)
            {
                return new Span<double>(ptr, _count);
            }
        }
        public double[] ToArray() => AsSpan().ToArray();
        public double[,] ToArray2()
        {
            double[,] result = new double[_size, _size];
            call_mat2_to_array(this, result);
            return result;
        }
        public FMatrix ToMatrix() => new FMatrix(ToArray2());
        #endregion

        #region Algebra
        public static FMatrix2 Negate(in FMatrix2 a) => neg_mat2(a);
        public static FMatrix2 Scale(double factor, in FMatrix2 a) => mul_scalar_mat2(factor, a);
        public static FMatrix2 Scale(in FMatrix2 a, double divisor) => mul_mat2_scalar(a, divisor);
        public static FMatrix2 Divide(in FMatrix2 a, double divisor) => div_mat2_scalar(a, divisor);
        public static FMatrix2 Add(in FMatrix2 a, in FMatrix2 b) => add_mat2_mat2(a, b);
        public static FMatrix2 Subtract(in FMatrix2 a, in FMatrix2 b) => sub_mat2_mat2(a, b);
        public static FMatrix2 Add(double a, in FMatrix2 b) => add_scalar_mat2(a, b);
        public static FMatrix2 Subtract(double a, in FMatrix2 b) => sub_scalar_mat2(a, b);
        public static FMatrix2 Add(in FMatrix2 a, double b) => add_mat2_scalar(a, b);
        public static FMatrix2 Subtract(in FMatrix2 a, double b) => sub_mat2_scalar(a, b);
        public static FVector2 Product(in FMatrix2 a, in FVector2 b) => mul_mat2_vec2(a, b);
        public static FVector2 Product(in FVector2 a, in FMatrix2 b) => mul_vec2_mat2(a, b);
        public static FMatrix2 Product(in FMatrix2 a, in FMatrix2 b) => mul_mat2_mat2(a, b);
        public static FMatrix2 Inner(in FMatrix2 a, in FMatrix2 b) => dot_mat2_mat2(a, b);
        public readonly FMatrix2 Transpose() => trans_mat2(this);
        public readonly FMatrix2 Inverse() => inv_mat2(this);
        public readonly FVector2 Solve(in FVector2 b) => solve_mat2_vec2(this, b);
        public readonly FMatrix2 Solve(in FMatrix2 b) => solve_mat2_mat2(this, b);
        #endregion

        #region Operators
        public static FMatrix2 operator +(in FMatrix2 a) => a;
        public static FMatrix2 operator -(in FMatrix2 a) => Negate(a);
        public static FMatrix2 operator +(in FMatrix2 a, in FMatrix2 b) => Add(a, b);
        public static FMatrix2 operator -(in FMatrix2 a, in FMatrix2 b) => Subtract(a, b);
        public static FMatrix2 operator +(double a, in FMatrix2 b) => Add(a, b);
        public static FMatrix2 operator -(double a, in FMatrix2 b) => Subtract(a, b);
        public static FMatrix2 operator +(in FMatrix2 a, double b) => Add(a, b);
        public static FMatrix2 operator -(in FMatrix2 a, double b) => Subtract(a, b);
        public static FMatrix2 operator *(double factor, in FMatrix2 a) => Scale(factor, a);
        public static FMatrix2 operator *(in FMatrix2 a, double factor) => Scale(factor, a);
        public static FMatrix2 operator /(in FMatrix2 a, double divisor) => Divide(a, divisor);
        public static FVector2 operator *(in FMatrix2 a, in FVector2 v) => Product(a, v);
        public static FVector2 operator *(in FVector2 v, in FMatrix2 a) => Product(v, a);
        public static FMatrix2 operator *(in FMatrix2 a, in FMatrix2 b) => Product(a, b);
        public static FMatrix2 operator |(in FMatrix2 a, in FMatrix2 b) => Inner(a, b);
        public static FMatrix2 operator ~(in FMatrix2 A) => A.Transpose();
        public static FMatrix2 operator !(in FMatrix2 A) => A.Inverse();
        public static FVector2 operator /(in FVector2 b, in FMatrix2 A) => A.Solve(b);
        public static FMatrix2 operator /(in FMatrix2 B, in FMatrix2 A) => A.Solve(B);
        #endregion

        #region Fortran API
        const string libraryName = FortranMethods.libraryName;

        [DllImport(libraryName, EntryPoint = "mat2_zeros", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix2 mat2_zeros();
        [DllImport(libraryName, EntryPoint = "mat2_eye", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix2 mat2_eye();
        [DllImport(libraryName, EntryPoint = "mat2_ones", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix2 mat2_ones();
        [DllImport(libraryName, EntryPoint = "mat2_values", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix2 mat2_values(double a11, double a12, double a21, double a22);
        [DllImport(libraryName, EntryPoint = "mat2_uniform", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix2 mat2_uniform(ref int seed);
        [DllImport(libraryName, EntryPoint = "add_mat2_mat2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix2 add_mat2_mat2(in FMatrix2 a, in FMatrix2 b);
        [DllImport(libraryName, EntryPoint = "sub_mat2_mat2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix2 sub_mat2_mat2(in FMatrix2 a, in FMatrix2 b);
        [DllImport(libraryName, EntryPoint = "add_scalar_mat2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix2 add_scalar_mat2(double a, in FMatrix2 b);
        [DllImport(libraryName, EntryPoint = "sub_scalar_mat2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix2 sub_scalar_mat2(double a, in FMatrix2 b);
        [DllImport(libraryName, EntryPoint = "add_mat2_scalar", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix2 add_mat2_scalar(in FMatrix2 a, double b);
        [DllImport(libraryName, EntryPoint = "sub_mat2_scalar", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix2 sub_mat2_scalar(in FMatrix2 a, double b);
        [DllImport(libraryName, EntryPoint = "neg_mat2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix2 neg_mat2(in FMatrix2 a);
        [DllImport(libraryName, EntryPoint = "mul_scalar_mat2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix2 mul_scalar_mat2(double a, in FMatrix2 b);
        [DllImport(libraryName, EntryPoint = "mul_mat2_scalar", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix2 mul_mat2_scalar(in FMatrix2 a, double b);
        [DllImport(libraryName, EntryPoint = "div_mat2_scalar", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix2 div_mat2_scalar(in FMatrix2 a, double b);
        [DllImport(libraryName, EntryPoint = "mul_mat2_vec2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector2 mul_mat2_vec2(in FMatrix2 a, in FVector2 b);
        [DllImport(libraryName, EntryPoint = "mul_vec2_mat2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector2 mul_vec2_mat2(in FVector2 a, in FMatrix2 b);
        [DllImport(libraryName, EntryPoint = "mul_mat2_mat2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix2 mul_mat2_mat2(in FMatrix2 a, in FMatrix2 b);
        [DllImport(libraryName, EntryPoint = "dot_mat2_mat2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix2 dot_mat2_mat2(in FMatrix2 a, in FMatrix2 b);
        [DllImport(libraryName, EntryPoint = "trace_mat2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern double trace_mat2(in FMatrix2 a);
        [DllImport(libraryName, EntryPoint = "det_mat2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern double det_mat2(in FMatrix2 a);
        [DllImport(libraryName, EntryPoint = "trans_mat2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix2 trans_mat2(in FMatrix2 a);
        [DllImport(libraryName, EntryPoint = "inv_mat2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix2 inv_mat2(in FMatrix2 a);
        [DllImport(libraryName, EntryPoint = "solve_mat2_vec2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector2 solve_mat2_vec2(in FMatrix2 a, in FVector2 b);
        [DllImport(libraryName, EntryPoint = "solve_mat2_mat2", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix2 solve_mat2_mat2(in FMatrix2 a, in FMatrix2 b);
        [DllImport(libraryName, EntryPoint = "call_mat2_to_array", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_mat2_to_array(in FMatrix2 a, [Out] double[,] b);
        #endregion
    }
}
