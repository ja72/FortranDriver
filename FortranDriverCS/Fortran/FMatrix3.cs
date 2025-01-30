using System.Diagnostics.Contracts;
using System.Runtime.InteropServices;

using JA.Fortran.Arrays;

namespace JA.Fortran
{
    public unsafe struct FMatrix3 :
        IFormattable,
        IEquatable<FMatrix3>
    {
        private const int _size = 3;
        private const int _count = _size*_size;
        public static int Size { get; } = _size;
        public static int Count { get; } = _count;

        fixed double _data[_count];

        #region Factory
        public FMatrix3(
            double a11, double a12, double a13,
            double a21, double a22, double a23,
            double a31, double a32, double a33)
        {
            this=mat3_values(
                a11, a12, a13,
                a21, a22, a23,
                a31, a32, a33);
        }
        public FMatrix3(double[] values, int index = 0)
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
        public static FMatrix3 Zero { get; } = mat3_zeros();
        public static FMatrix3 Identity { get; } = mat3_eye();
        public static FMatrix3 Ones { get; } = mat3_ones();

        public static FMatrix3 Diagonal(double a11, double a22, double a33)
            => mat3_diag(a11, a22, a33);
        public static FMatrix3 Scalar(double a)
            => mat3_scalar(a);
        public static FMatrix3 Symmetric(double a11, double a12, double a13, double a22, double a23, double a33)
            => mat3_symm(a11, a12, a13, a22, a23, a33);
        public static FMatrix3 SkewSymmetric(double a32, double a13, double a21)
            => mat3_skew(a32, a13, a21);

        public static FMatrix3 Uniform(ref int seed) => mat3_uniform(ref seed);

        public static explicit operator FMatrix3(double a) => Scalar(a); 

        public static implicit operator FMatrix(FMatrix3 matrix) => matrix.ToMatrix();
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
        /// <returns>False if object is a different type, otherwise it calls <code>Equals(FMatrix3)</code></returns>
        public override bool Equals(object obj)
        {
            return obj is FMatrix3 vector&&Equals(vector);
        }

        public static bool operator ==(FMatrix3 target, FMatrix3 other) => target.Equals(other);
        public static bool operator !=(FMatrix3 target, FMatrix3 other) => !( target==other );


        /// <summary>
        /// Checks for equality among <see cref="FMatrix3"/> classes
        /// </summary>
        /// <param name="other">The other <see cref="FMatrix3"/> to compare it to</param>
        /// <returns>True if equal</returns>
        public bool Equals(FMatrix3 other)
        {
            return _data[0]==other._data[0]
                && _data[1]==other._data[1]
                && _data[2]==other._data[2]
                && _data[3]==other._data[3]
                && _data[4]==other._data[4]
                && _data[5]==other._data[5]
                && _data[6]==other._data[6]
                && _data[7]==other._data[7]
                && _data[8]==other._data[8];
        }

        /// <summary>
        /// Calculates the hash code for the <see cref="FMatrix3"/>
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
        public double A11 => _data[0+0];
        public double A12 => _data[3+0];
        public double A13 => _data[6+0];
        public double A21 => _data[0+1];
        public double A22 => _data[3+1];
        public double A23 => _data[6+1];
        public double A31 => _data[0+2];
        public double A32 => _data[3+2];
        public double A33 => _data[6+2]; 

        public readonly double Trace() => trace_mat3(this);
        public readonly double Determinant() => det_mat3(this);
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
            call_mat3_to_array(this, result);
            return result;
        }

        public FMatrix ToMatrix() => new FMatrix(ToArray2());
        #endregion

        #region Algebra
        public static FMatrix3 Negate(in FMatrix3 a) => neg_mat3(a);
        public static FMatrix3 Scale(double factor, in FMatrix3 a) => mul_scalar_mat3(factor, a);
        public static FMatrix3 Scale(in FMatrix3 a, double divisor) => mul_mat3_scalar(a, divisor);
        public static FMatrix3 Divide(in FMatrix3 a, double divisor) => div_mat3_scalar(a, divisor);
        public static FMatrix3 Add(in FMatrix3 a, in FMatrix3 b) => add_mat3_mat3(a, b);
        public static FMatrix3 Subtract(in FMatrix3 a, in FMatrix3 b) => sub_mat3_mat3(a, b);
        public static FMatrix3 Add(double a, in FMatrix3 b) => add_scalar_mat3(a, b);
        public static FMatrix3 Subtract(double a, in FMatrix3 b) => sub_scalar_mat3(a, b);
        public static FMatrix3 Add(in FMatrix3 a, double b) => add_mat3_scalar(a, b);
        public static FMatrix3 Subtract(in FMatrix3 a, double b) => sub_mat3_scalar(a, b);
        public static FVector3 Product(in FMatrix3 a, in FVector3 b) => mul_mat3_vec3(a, b);
        public static FVector3 Product(in FVector3 a, in FMatrix3 b) => mul_vec3_mat3(a, b);
        public static FMatrix3 Product(in FMatrix3 a, in FMatrix3 b) => mul_mat3_mat3(a, b);
        public static FMatrix3 Inner(in FMatrix3 a, in FMatrix3 b) => dot_mat3_mat3(a, b);
        public readonly FMatrix3 Transpose() => trans_mat3(this);
        public readonly FMatrix3 Inverse() => inv_mat3(this);
        public readonly FVector3 Solve(in FVector3 b) => solve_mat3_vec3(this, b);
        public readonly FMatrix3 Solve(in FMatrix3 b) => solve_mat3_mat3(this, b);
        public readonly FVector3 Rotate(in FVector3 vector, bool inverse = false)
            => mat3_rotate_vec3(this, vector, inverse);
        public readonly FMatrix3 RotateMMOI(double I_1, double I_2, double I_3, bool inverse = false)
            => RotateMMOI([I_1, I_2, I_3], inverse);
        public readonly FMatrix3 RotateMMOI(double[] I_diag, bool inverse = false)
            => mat3_rotate_diag(this, I_diag, inverse);
        #endregion

        #region Operators
        public static FMatrix3 operator +(in FMatrix3 a) => a;
        public static FMatrix3 operator -(in FMatrix3 a) => Negate(a);
        public static FMatrix3 operator +(in FMatrix3 a, in FMatrix3 b) => Add(a, b);
        public static FMatrix3 operator -(in FMatrix3 a, in FMatrix3 b) => Subtract(a, b);
        public static FMatrix3 operator +(double a, in FMatrix3 b) => Add(a, b);
        public static FMatrix3 operator -(double a, in FMatrix3 b) => Subtract(a, b);
        public static FMatrix3 operator +(in FMatrix3 a, double b) => Add(a, b);
        public static FMatrix3 operator -(in FMatrix3 a, double b) => Subtract(a, b);
        public static FMatrix3 operator *(double factor, in FMatrix3 a) => Scale(factor, a);
        public static FMatrix3 operator *(in FMatrix3 a, double factor) => Scale(factor, a);
        public static FMatrix3 operator /(in FMatrix3 a, double divisor) => Divide(a, divisor);
        public static FVector3 operator *(in FMatrix3 a, in FVector3 v) => Product(a, v);
        public static FVector3 operator *(in FVector3 v, in FMatrix3 a) => Product(v, a);
        public static FMatrix3 operator *(in FMatrix3 a, in FMatrix3 b) => Product(a, b);
        public static FMatrix3 operator |(in FMatrix3 a, in FMatrix3 b) => Inner(a, b);
        public static FMatrix3 operator ~(in FMatrix3 A) => A.Transpose();
        public static FMatrix3 operator !(in FMatrix3 A) => A.Inverse();
        public static FVector3 operator /(in FVector3 b, in FMatrix3 A) => A.Solve(b);
        public static FMatrix3 operator /(in FMatrix3 B, in FMatrix3 A) => A.Solve(B);
        #endregion

        #region Fortran API
        const string libraryName = FortranMethods.libraryName;

        [DllImport(libraryName, EntryPoint = "mat3_zeros", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix3 mat3_zeros();
        [DllImport(libraryName, EntryPoint = "mat3_eye", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix3 mat3_eye();
        [DllImport(libraryName, EntryPoint = "mat3_ones", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix3 mat3_ones();
        [DllImport(libraryName, EntryPoint = "mat3_values", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix3 mat3_values(
            double a11, double a12, double a13,
            double a21, double a22, double a23, 
            double a31, double a32, double a33);
        [DllImport(libraryName, EntryPoint = "mat3_uniform", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix3 mat3_uniform(ref int seed);
        [DllImport(libraryName, EntryPoint = "mat3_scalar", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix3 mat3_scalar(double a);
        [DllImport(libraryName, EntryPoint = "mat3_diag", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix3 mat3_diag(double a11, double a22, double a33);
        [DllImport(libraryName, EntryPoint = "mat3_symm", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix3 mat3_symm(double a11, double a12, double a13, double a22, double a23, double a33);
        [DllImport(libraryName, EntryPoint = "mat3_skew", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix3 mat3_skew(double a32, double a13, double a21);
        [DllImport(libraryName, EntryPoint = "add_mat3_mat3", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix3 add_mat3_mat3(in FMatrix3 a, in FMatrix3 b);
        [DllImport(libraryName, EntryPoint = "sub_mat3_mat3", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix3 sub_mat3_mat3(in FMatrix3 a, in FMatrix3 b);
        [DllImport(libraryName, EntryPoint = "neg_mat3", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix3 neg_mat3(in FMatrix3 a);
        [DllImport(libraryName, EntryPoint = "sub_scalar_mat3", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix3 add_scalar_mat3(double a, in FMatrix3 b);
        [DllImport(libraryName, EntryPoint = "sub_scalar_mat3", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix3 sub_scalar_mat3(double a, in FMatrix3 b);
        [DllImport(libraryName, EntryPoint = "add_mat3_scalar", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix3 add_mat3_scalar(in FMatrix3 a, double b);
        [DllImport(libraryName, EntryPoint = "sub_mat3_scalar", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix3 sub_mat3_scalar(in FMatrix3 a, double b);
        [DllImport(libraryName, EntryPoint = "mul_scalar_mat3", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix3 mul_scalar_mat3(double v, in FMatrix3 a);
        [DllImport(libraryName, EntryPoint = "mul_mat3_scalar", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix3 mul_mat3_scalar(in FMatrix3 a, double b);
        [DllImport(libraryName, EntryPoint = "div_mat3_scalar", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix3 div_mat3_scalar(in FMatrix3 a, double b);
        [DllImport(libraryName, EntryPoint = "mul_mat3_vec3", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector3 mul_mat3_vec3(in FMatrix3 a, in FVector3 b);
        [DllImport(libraryName, EntryPoint = "mul_vec3_mat3", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector3 mul_vec3_mat3(in FVector3 a, in FMatrix3 b);
        [DllImport(libraryName, EntryPoint = "mul_mat3_mat3", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix3 mul_mat3_mat3(in FMatrix3 a, in FMatrix3 b);
        [DllImport(libraryName, EntryPoint = "dot_mat3_mat3", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix3 dot_mat3_mat3(in FMatrix3 a, in FMatrix3 b);
        [DllImport(libraryName, EntryPoint = "trans_mat3", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix3 trans_mat3(in FMatrix3 a);
        [DllImport(libraryName, EntryPoint = "trace_mat3", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern double trace_mat3(in FMatrix3 a);
        [DllImport(libraryName, EntryPoint = "det_mat3", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern double det_mat3(in FMatrix3 a);
        [DllImport(libraryName, EntryPoint = "inv_mat3", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix3 inv_mat3(in FMatrix3 a);
        [DllImport(libraryName, EntryPoint = "solve_mat3_vec3", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector3 solve_mat3_vec3(in FMatrix3 a, in FVector3 b);
        [DllImport(libraryName, EntryPoint = "solve_mat3_mat3", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix3 solve_mat3_mat3(in FMatrix3 a, in FMatrix3 b);
        [DllImport(libraryName, EntryPoint = "mat3_rotate_vec3", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector3 mat3_rotate_vec3(in FMatrix3 rotation, in FVector3 vector, bool inverse);
        [DllImport(libraryName, EntryPoint = "mat3_rotate_diag", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix3 mat3_rotate_diag(in FMatrix3 rotation, double[] diag, bool inverse);
        [DllImport(libraryName, EntryPoint = "call_mat3_to_array", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_mat3_to_array(in FMatrix3 a, [Out] double[,] b);
        #endregion

    }
}
