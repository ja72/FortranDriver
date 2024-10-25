using System.Diagnostics.Contracts;
using System.Globalization;
using System.Runtime.InteropServices;
using System.Text;

using Windows.ApplicationModel.Activation;

namespace JA.Fortran
{
    public unsafe struct FVector3 : 
        IFormattable,
        IEquatable<FVector3>
    {
        private const int _size = 3;
        public static int Size { get; } = _size;

        fixed double _data[_size];

        #region Factory
        public FVector3(double x, double y, double z)
        {
            this=vec3_values(x, y, z);
            //fixed (double* ptr = _data)
            //{
            //    ptr[0]=x;
            //    ptr[1]=y;
            //    ptr[2]=z;
            //}

        }
        public FVector3(double[] values, int index = 0)
        {
            Contract.Requires(null!=values);
            Contract.Requires(index+_size<=values.Length);
            fixed (double* ptr = _data)
            {
                for (int i = 0; i<_size; i++)
                {
                    _data[i]=values[i];
                }
            }
        }
        public static implicit operator FVector3(double[] values) => new FVector3(values, 0);
        public static FVector3 Zero { get; } = vec3_zeros();
        public static FVector3 EX { get; } = vec3_ux();
        public static FVector3 EY { get; } = vec3_uy();
        public static FVector3 EZ { get; } = vec3_uz();
        public static FVector3 Ones { get; } = vec3_ones();
        public static FVector3 Uniform(ref int seed) => vec3_uniform(ref seed);
        #endregion

        #region Properties
        public double X => _data[0];
        public double Y => _data[1];
        public double Z => _data[2];

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
            call_vec3_to_array(this, result);
            return result;
        }

        public FVector ToVector() => new FVector(ToArray());

        public ref double this[int index] => ref _data[index];
        public readonly double Norm() => norm_vec3(this);
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
                return ToArray().ToTableString(HorizontalAlignment.Right, formatting, formatProvider);
            }
            else
            {
                return ToArray().ToListString(formatting);
            }
        }
        #endregion

        #region IEquatable Members
        /// <summary>
        /// Equality overrides from <see cref="System.Object"/>
        /// </summary>
        /// <param name="obj">The object to compare this with</param>
        /// <returns>False if object is a different type, otherwise it calls <code>Equals(FVector3)</code></returns>
        public override bool Equals(object obj)
        {
            return obj is FVector3 vector&&Equals(vector);
        }

        public static bool operator ==(FVector3 target, FVector3 other) => target.Equals(other);
        public static bool operator !=(FVector3 target, FVector3 other) => !( target==other );


        /// <summary>
        /// Checks for equality among <see cref="FVector3"/> classes
        /// </summary>
        /// <param name="other">The other <see cref="FVector3"/> to compare it to</param>
        /// <returns>True if equal</returns>
        public bool Equals(FVector3 other)
        {
            return _data[0]==other._data[0]
                && _data[1]==other._data[1]
                && _data[2]==other._data[2];
        }

        /// <summary>
        /// Calculates the hash code for the <see cref="FVector3"/>
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
        public FMatrix3 CrossMatrix() => cross_vec3_op(this);

        #endregion

        #region Algebra
        public static FVector3 Add(in FVector3 a, in FVector3 b) => add_vec3_vec3(a, b);
        public static FVector3 Subtract(in FVector3 a, in FVector3 b) => sub_vec3_vec3(a, b);
        public static FVector3 Negate(in FVector3 a) => neg_vec3(a);
        public static FVector3 Scale(double factor, in FVector3 a) => mul_scalar_vec3(factor, a);
        public static FVector3 Scale(in FVector3 a, double factor) => mul_vec3_scalar(a, factor);
        public static FVector3 Divide(in FVector3 a, double d) => div_vec3_scalar(a, d);
        public static double Dot(in FVector3 A, in FVector3 B) => inner_vec3_vec3(A, B);
        public static FMatrix3 Outer(in FVector3 a, in FVector3 b) => outer_vec3_vec3(a, b);
        public static FVector3 Cross(in FVector3 a, in FVector3 b) => cross_vec3_vec3(a, b);
        public static double AngleBetween(in FVector3 a, in FVector3 b)
            => vec3_angle(a, b);
        #endregion

        #region Operators
        public static FVector3 operator +(in FVector3 a, in FVector3 b) => Add(a, b);
        public static FVector3 operator -(in FVector3 a) => Negate(a);
        public static FVector3 operator -(in FVector3 a, in FVector3 b) => Subtract(a, b);
        public static FVector3 operator *(double f, in FVector3 a) => Scale(f, a);
        public static FVector3 operator *(in FVector3 a, double f) => Scale(f, a);
        public static FVector3 operator /(in FVector3 a, double d) => Divide(a, d);
        public static double operator |(in FVector3 a, in FVector3 b) => Dot(a, b);
        #endregion

        #region Fortran API

        const string libraryName = FortranMethods.libraryName;

        [DllImport(libraryName, EntryPoint = "vec3_zeros", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector3 vec3_zeros();
        [DllImport(libraryName, EntryPoint = "vec3_ones", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector3 vec3_ones();
        [DllImport(libraryName, EntryPoint = "vec3_ux", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector3 vec3_ux();
        [DllImport(libraryName, EntryPoint = "vec3_uy", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector3 vec3_uy();
        [DllImport(libraryName, EntryPoint = "vec3_uz", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector3 vec3_uz();
        [DllImport(libraryName, EntryPoint = "vec3_values", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector3 vec3_values(double x, double y, double z);
        [DllImport(libraryName, EntryPoint = "vec3_uniform", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector3 vec3_uniform(ref int seed);
        [DllImport(libraryName, EntryPoint = "norm_vec3", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern double norm_vec3(FVector3 a);
        [DllImport(libraryName, EntryPoint = "add_vec3_vec3", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector3 add_vec3_vec3(in FVector3 a, in FVector3 b);
        [DllImport(libraryName, EntryPoint = "sub_vec3_vec3", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector3 sub_vec3_vec3(in FVector3 a, in FVector3 b);
        [DllImport(libraryName, EntryPoint = "neg_vec3", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector3 neg_vec3(in FVector3 a);
        [DllImport(libraryName, EntryPoint = "mul_scalar_vec3", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector3 mul_scalar_vec3(double s, in FVector3 b);
        [DllImport(libraryName, EntryPoint = "mul_vec3_scalar", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector3 mul_vec3_scalar(in FVector3 a, double s);
        [DllImport(libraryName, EntryPoint = "div_vec3_scalar", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector3 div_vec3_scalar(in FVector3 a, double s);
        [DllImport(libraryName, EntryPoint = "inner_vec3_vec3", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern double inner_vec3_vec3(in FVector3 a, in FVector3 b  );
        [DllImport(libraryName, EntryPoint = "outer_vec3_vec3", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix3 outer_vec3_vec3(in FVector3 a, in FVector3 b);
        [DllImport(libraryName, EntryPoint = "cross_vec3_vec3", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FVector3 cross_vec3_vec3(in FVector3 a, in FVector3 b);
        [DllImport(libraryName, EntryPoint = "cross_vec3_op", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern FMatrix3 cross_vec3_op(in FVector3 a);
        [DllImport(libraryName, EntryPoint = "vec3_angle", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern double vec3_angle(in FVector3 a, in FVector3 b);
        [DllImport(libraryName, EntryPoint = "call_vec3_to_array", CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
        internal static extern void call_vec3_to_array(in FVector3 a, [Out] double[] b);
        #endregion
    }
}
